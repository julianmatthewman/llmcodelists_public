# Setup and Reference Codelists ------------------------------------------------------------------
library(vitals)
library(ellmer)
library(dplyr)
library(ggplot2)
library(ragnar)
library(tidyverse)
library(janitor)
library(gt)

source("set_vitals_logs_path.R")
vitals::vitals_log_dir_set(vitals_logs_path)
# vitals_view()

# Get list of CSV files in codelists folder
codelist_files <- list.files(
  "codelists",
  pattern = "\\codelist.csv$",
  full.names = TRUE
)
codelists_list <- lapply(codelist_files, read_csv, show_col_types = FALSE)
names(codelists_list) <- tools::file_path_sans_ext(basename(codelist_files))
codelists_list <- map(codelists_list, \(x) {
  x |>
    # filter(CleansedReadCode != "" & !is.na(CleansedReadCode)) |>
    mutate(
      code_term = paste(OriginalReadCode, Term),
      code_term_definite = paste(OriginalReadCode, Term, required),
      snomed_term_definite = paste(SnomedCTConceptId, Term, required)
    )
})

#Make codelists table
codelists <- tibble(
  id = c(1, 2, 3, 4, 5, 6, 7),
  input = c(
    "Atopic eczema",
    "Vascular dementia",
    "Eosinophilic esophagitis",
    "Psoriasis",
    "Hidradentitis suppurativa",
    "Myocardial infarction",
    "Wrist fracture"
  ),
  target = c(
    paste(codelists_list[[which(names(codelists_list) == "eczema codelist")]]$code_term_definite, collapse = "\n"),
    paste(codelists_list[[which(names(codelists_list) == "vascular_dementia codelist")]]$code_term_definite, collapse = "\n"),
    paste(codelists_list[[which(names(codelists_list) == "eosinophilic_esophagitis codelist")]]$code_term_definite, collapse = "\n"),
    paste(codelists_list[[which(names(codelists_list) == "psoriasis codelist")]]$code_term_definite, collapse = "\n"),
    paste(codelists_list[[which(names(codelists_list) == "hidradenitis_suppurativa codelist")]]$code_term_definite, collapse = "\n"),
    paste(codelists_list[[which(names(codelists_list) == "myocardial_infarction codelist")]]$code_term_definite, collapse = "\n"),
    paste(codelists_list[[which(names(codelists_list) == "wrist_fracture codelist")]]$code_term_definite, collapse = "\n")
  )
)

# Count the number of rows and NA CleansedReadCode for each codelist
map_df(
  codelists_list,
  ~ tibble(
    n_rows = nrow(.x),
    n_requied = sum(.x$required == 1),
    n_optional = sum(.x$required == 0),
    n_read = sum(!is.na(.x$CleansedReadCode)),
    n_read_required = sum(!is.na(.x$CleansedReadCode) & .x$required == 1),
    n_read_optional = sum(!is.na(.x$CleansedReadCode) & .x$required == 0)
  ),
  .id = "codelist"
)
save(codelists, file = "output/codelists.RData")

# Scoring instructions (same for all evals)
scoring_instructions <- paste(
  "At the end of your evaluation, in a new line, if correct write 'GRADE: C', if partially correct write 'GRADE: P', and if incorrect write 'GRADE: I'.",
  "In the target codelist in the 'required' column, 1 denotes a required code, 0 denotes an optional code.",
  "Score as correct only if all required codes are included and no codes not in the target codelist (either marked 'required' or 'optional') are included the final codelist.",
  "Score as partially correct if all required codes are included but there are also codes included that are not in the target codelists.",
  "Otherwise score as incorrect."
)

# #Take sample
# codelists <- codelists[7, ]

# Set up RAG -------------------------------------------------------------

store_location <- "aurum_gemini.ragnar.duckdb"
store <- ragnar_store_connect(store_location, read_only = TRUE)

# Custom Solver (sequential version)
rag_solver <- function(inputs, ..., solver_chat) {
  results <- character(length(inputs))
  chats <- list()

  for (i in seq_along(inputs)) {
    ch <- solver_chat$clone()
    ragnar_register_tool_retrieve(ch, store, top_k = 100, deoverlap = FALSE)
    ch$chat(inputs[i])
    results[i] <- ch$last_turn()@text
    chats[[i]] <- ch
  }

  list(
    result = results,
    solver_chat = chats
  )
}

system_prompt_single_step <- paste(
  "Create a codelist for the given clinical condition in the input.",
  "Include all codes that when recorded in a person's medical record would indicate that the person currently has the condition.",
  "Use only codes from the knowledge store.",
  # "If necessary, perform multiple searches of the knowledge store for synonyms to ensure comprehensive code retrieval. ",
  "Do not miss any relevant codes. Codelist can be of any length.",
  "Supply codelists as a tables."
)

# Set up task
task_rag <- Task$new(
  dataset = codelists,
  solver = rag_solver,
  scorer = model_graded_qa(
    partial_credit = TRUE,
    scorer_chat = chat_google_gemini(model = "gemini-3-pro-preview"),
    instructions = scoring_instructions
  ),
  epoch = 3,
  name = "RAG"
)

# Perform RAG eval -----------------------------------------------------------

# Gemini Pro
gemini <- chat_google_gemini(model = "gemini-3-pro-preview", system_prompt = system_prompt_single_step)
task_rag_gemini <- task_rag$clone()
task_rag_gemini$eval(solver_chat = gemini$clone())
task_rag_gemini <- vitals_bind(task_rag_gemini)
save(task_rag_gemini, file = "output/task_rag_gemini.RData")

# Gemini Flash
gemini_flash <- chat_google_gemini(model = "gemini-3-flash-preview", system_prompt = system_prompt_single_step)
task_rag_gemini_flash <- task_rag$clone()
task_rag_gemini_flash$eval(solver_chat = gemini_flash$clone())
task_rag_gemini_flash <- vitals_bind(task_rag_gemini_flash)
save(task_rag_gemini_flash, file = "output/task_rag_gemini_flash.RData")

# OpenAI
openai <- chat_openai(model = "gpt-5.2", system_prompt = system_prompt_single_step)
task_rag_openai <- task_rag$clone()
task_rag_openai$eval(solver_chat = openai$clone())
task_rag_openai <- vitals_bind(task_rag_openai)
save(task_rag_openai, file = "output/task_rag_openai.RData")

# Claude
claude <- chat_claude(system_prompt = system_prompt_single_step, model = "claude-sonnet-4-6")
task_rag_claude <- task_rag$clone()
task_rag_claude$eval(solver_chat = claude$clone())
task_rag_claude <- vitals_bind(task_rag_claude)
save(task_rag_claude, file = "output/task_rag_claude.RData")

# Mistral
mistral <- chat_mistral(system_prompt = system_prompt_single_step)
task_rag_mistral <- task_rag$clone()
task_rag_mistral$eval(solver_chat = mistral$clone())
task_rag_mistral <- vitals_bind(task_rag_mistral)
save(task_rag_mistral, file = "output/task_rag_mistral.RData")


# Setup string search -----------------------------------------------------

# Get CPRD browser
# Replace the path below with the local path to your CPRD data file
base_file <- "path/to/CPRDAurumMedical_2025_09.dta"
cprd_data <- haven::read_dta(base_file) |>
  filter(cleansedreadcode != "") |>
  select(originalreadcode, term)


# Create string search tool
tool_term_search_cprd <- function(search_term) {
  search_term <- paste0("(?=.*", str_split_1(search_term, " "), ")", collapse = "")

  results <- cprd_data |>
    filter(str_detect(tolower(as.character(term)), tolower(search_term)))

  if (nrow(results) == 0) {
    return("No matches found.")
  }

  results |>
    head(500) |>
    knitr::kable(format = "markdown")
}

tool_term_search_cprd <- tool(
  tool_term_search_cprd,
  name = "tool_term_search_cprd",
  description = paste(
    "Search for records in CPRD using a specified term.",
    "The tool detects the presence of the searchterm anywhere in the term column.",
    "Multiple words within a search term are detected in any order, e.g., 'apple banana' matches 'apples and bananas' and 'bananas and apples'.",
    "Use word stems where appropriate, e.g., 'app' matches 'apple' and 'application'."
  ),
  arguments = list(
    search_term = type_string("The term to search for in the CPRD records.")
  )
)

# Create code search tool
tool_code_search_cprd <- function(search_term) {
  search_term <- paste0("^", search_term)

  results <- cprd_data |>
    filter(str_detect(tolower(as.character(originalreadcode)), tolower(search_term)))

  if (nrow(results) == 0) {
    return("No matches found.")
  }

  results |>
    head(500) |>
    knitr::kable(format = "markdown")
}

tool_code_search_cprd <- tool(
  tool_code_search_cprd,
  name = "tool_code_search_cprd",
  description = paste(
    "Search for records in CPRD using a specified Read code.",
    "The tool detects the presence of the searchterm at the start of the string in the code column."
  ),
  arguments = list(
    search_term = type_string("The code to search for in the CPRD records.")
  )
)

# Set up custom solver
stringsearch_solver <- function(inputs, ..., solver_chat) {
  ch <- solver_chat$clone()
  ch$register_tool(tool_term_search_cprd)
  ch$register_tool(tool_code_search_cprd)

  res <- ellmer::parallel_chat(ch, as.list(inputs))
  list(
    result = purrr::map_chr(res, function(c) c$last_turn()@text),
    solver_chat = res
  )
}

system_prompt_stringsearch <- paste(
  "Create a codelist for the given clinical condition in the input.",
  "Include all codes that when recorded in a person's medical record would indicate that the person currently has the condition.",
  "Use the CPRD term search tool and code search tool.",
  "Do not miss any relevant codes. Codelist can be of any length.",
  "Supply codelists as a tables."
)


# Set up task
task_stringsearch <- Task$new(
  dataset = codelists,
  solver = stringsearch_solver,
  scorer = model_graded_qa(
    partial_credit = TRUE,
    scorer_chat = chat_google_gemini(model = "gemini-3-pro-preview"),
    instructions = scoring_instructions
  ),
  epoch = 3,
  name = "String Search"
)

# Perform String Search eval -----------------------------------------------------------

# Gemini Pro
gemini <- chat_google_gemini(model = "gemini-3-pro-preview", system_prompt = system_prompt_stringsearch)
task_stringsearch_gemini <- task_stringsearch$clone()
task_stringsearch_gemini$eval(solver_chat = gemini$clone())
task_stringsearch_gemini <- vitals_bind(task_stringsearch_gemini)
save(task_stringsearch_gemini, file = "output/task_stringsearch_gemini.RData")

# Gemini Flash
gemini_flash <- chat_google_gemini(model = "gemini-3-flash-preview", system_prompt = system_prompt_stringsearch)
task_stringsearch_gemini_flash <- task_stringsearch$clone()
task_stringsearch_gemini_flash$eval(solver_chat = gemini_flash$clone())
task_stringsearch_gemini_flash <- vitals_bind(task_stringsearch_gemini_flash)
save(task_stringsearch_gemini_flash, file = "output/task_stringsearch_gemini_flash.RData")

# Claude
claude <- chat_claude(system_prompt = system_prompt_stringsearch, model = "claude-sonnet-4-6")
task_stringsearch_claude <- task_stringsearch$clone()
task_stringsearch_claude$eval(solver_chat = claude$clone())
task_stringsearch_claude_read <- vitals_bind(task_stringsearch_claude)
save(task_stringsearch_claude_read, file = "output/task_stringsearch_claude_read.RData")

# OpenAI
openai <- chat_openai(model = "gpt-5.2", system_prompt = system_prompt_stringsearch)
task_stringsearch_openai <- task_stringsearch$clone()
task_stringsearch_openai$eval(solver_chat = openai$clone())
task_stringsearch_openai <- vitals_bind(task_stringsearch_openai)
save(task_stringsearch_openai, file = "output/task_stringsearch_openai.RData")

# Mistral
mistral <- chat_mistral(system_prompt = system_prompt_stringsearch)
task_stringsearch_mistral <- task_stringsearch$clone()
task_stringsearch_mistral$eval(solver_chat = mistral$clone())
task_stringsearch_mistral <- vitals_bind(task_stringsearch_mistral)
save(task_stringsearch_mistral, file = "output/task_stringsearch_mistral.RData")

# Setup context -----------------------------------------------------

# Get CPRD browser
# Replace the path below with the local path to your CPRD data file
base_file <- "path/to/CPRDAurumMedical_2025_09.dta"
cprd_data <- haven::read_dta(base_file) |>
  filter(cleansedreadcode != "") |>
  select(originalreadcode, term)

system_prompt_context <- paste(
  "Create a codelist for the given clinical condition in the input.",
  "Include all codes that when recorded in a person's medical record would indicate that the person currently has the condition.",
  "Use only codes from the CPRD browser provided in the input.",
  "Do not miss any relevant codes. Codelist can be of any length.",
  "Supply codelists as a tables."
)

codelists_with_context <- codelists |>
  mutate(
    input = paste(input, "\n\n", "CPRD browser data:", "\n", paste(cprd_data$originalreadcode, cprd_data$term, collapse = "\n"))
  )

# Set up task
task_context <- Task$new(
  dataset = codelists_with_context,
  solver = generate(chat_claude(model = "claude-opus-4-6", system_prompt = system_prompt_context)),
  scorer = model_graded_qa(
    partial_credit = TRUE,
    scorer_chat = chat_google_gemini(model = "gemini-3-pro-preview"),
    instructions = scoring_instructions
  ),
  epoch = 1,
  name = "Context"
)


# Perform context eval -----------------------------------------------------------

# Claude Opus 4.6
claude_opus <- chat_claude(model = "claude-opus-4-6", system_prompt = system_prompt_context)
task_context_claude_opus <- task_context$clone()
task_context_claude_opus$eval(solver_chat = claude_opus$clone())
task_context_claude_opus <- vitals_bind(task_context_claude_opus)
save(task_context_claude_opus, file = "output/task_context_claude_opus.RData")


# Evals for Claude Code codelists ----------------------------------------

claude_code_epochs <- tibble(
  epoch = c(1, 2, 3),
  folder = c(
    "claude_code_codelists/Opus 4-6 1",
    "claude_code_codelists/Opus 4-6 2",
    "claude_code_codelists/Opus 4-6 3"
  )
)

# Map each condition to its gold standard key and base name for file matching
claude_code_condition_map <- tibble(
  id = c(1, 2, 3, 4, 5, 6, 7),
  input = c(
    "Atopic eczema",
    "Vascular dementia",
    "Eosinophilic esophagitis",
    "Psoriasis",
    "Hidradentitis suppurativa",
    "Myocardial infarction",
    "Wrist fracture"
  ),
  condition = c(
    "atopic_dermatitis",
    "vascular_dementia",
    "eosinophilic_oesophagitis",
    "psoriasis",
    "hidradenitis_suppurativa",
    "myocardial_infarction",
    "wrist_fracture"
  ),
  gold_key = c(
    "eczema codelist",
    "vascular_dementia codelist",
    "eosinophilic_esophagitis codelist",
    "psoriasis codelist",
    "hidradenitis_suppurativa codelist",
    "myocardial_infarction codelist",
    "wrist_fracture codelist"
  )
)

# Compare each claude_code codelist to its gold standard by MedCodeId across epochs
task_claude_code <- map_df(1:nrow(claude_code_epochs), function(e) {
  epoch_num <- claude_code_epochs$epoch[e]
  folder <- claude_code_epochs$folder[e]

  pmap_df(claude_code_condition_map, function(id, input, condition, gold_key) {
    # Find the CSV file for this condition in the epoch folder
    csv_files <- list.files(folder, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
    csv_file <- csv_files[grep(condition, csv_files)]
    csv_file <- csv_file[!grepl("(all_codelists|all_conditions)", csv_file)][1]

    # Load claude_code codelist and normalise column names to lowercase
    claude_cl <- read_csv(csv_file, show_col_types = FALSE)
    names(claude_cl) <- tolower(names(claude_cl))

    # Get gold standard codelist
    gold <- codelists_list[[gold_key]]

    claude_ids <- claude_cl$medcodeid
    gold_all_ids <- gold$MedCodeId

    # Missing required: required codes in gold standard not present in claude_code
    missing_required <- gold |>
      filter(required == 1, !MedCodeId %in% claude_ids) |>
      select(MedCodeId, OriginalReadCode, Term)

    # Missing optional: optional codes in gold standard not present in claude_code
    missing_optional <- gold |>
      filter(required == 0, !MedCodeId %in% claude_ids) |>
      select(MedCodeId, OriginalReadCode, Term)

    # Irrelevant: codes in claude_code not present in gold standard (neither required nor optional)
    irrelevant <- claude_cl |>
      filter(!medcodeid %in% gold_all_ids) |>
      select(medcodeid, originalreadcode, term)

    # Score: Correct = all required present and no irrelevant;
    #        Partially correct = all required present but some irrelevant;
    #        Incorrect = missing required codes
    score <- case_when(
      nrow(missing_required) > 0 ~ "I",
      nrow(irrelevant) > 0 ~ "P",
      TRUE ~ "C"
    )

    tibble(
      id = id,
      input = input,
      task = "task_claude_code",
      epoch = epoch_num,
      n_claude_codes = nrow(claude_cl),
      n_gold_required = nrow(gold |> filter(required == 1)),
      n_gold_optional = nrow(gold |> filter(required == 0)),
      n_missing_required = nrow(missing_required),
      n_missing_optional = nrow(missing_optional),
      n_irrelevant = nrow(irrelevant),
      score = score,
      missing_required = list(missing_required),
      missing_optional = list(missing_optional),
      irrelevant = list(irrelevant)
    )
  })
})

save(task_claude_code, file = "output/task_claude_code.RData")
