# Setup and Reference Codelists ------------------------------------------------------------------
library(vitals)
library(ellmer)
library(ragnar)
library(tidyverse)

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
    mutate(code_term_definite = paste(OriginalReadCode, Term, required))
})
codelists_list_read <- map(codelists_list, \(x) x |> filter(CleansedReadCode != "" & !is.na(CleansedReadCode)))

#Make codelists table
keys <- tibble(
  id = c(1, 2, 3, 4, 5, 6, 7),
  input = c(
    "Atopic eczema",
    "Vascular dementia",
    "Eosinophilic esophagitis",
    "Psoriasis",
    "Hidradenitis suppurativa",
    "Myocardial infarction",
    "Wrist fracture"
  ),
  filename = c(
    "eczema codelist",
    "vascular_dementia codelist",
    "eosinophilic_esophagitis codelist",
    "psoriasis codelist",
    "hidradenitis_suppurativa codelist",
    "myocardial_infarction codelist",
    "wrist_fracture codelist"
  )
)

codelists <- keys |>
  rowwise() |>
  mutate(target = paste(codelists_list[[which(names(codelists_list) == filename)]]$code_term_definite, collapse = "\n")) |>
  select(-filename)

codelists_read <- keys |>
  rowwise() |>
  mutate(target = paste(codelists_list_read[[which(names(codelists_list) == filename)]]$code_term_definite, collapse = "\n")) |>
  select(-filename)

# Count the number of rows and NA CleansedReadCode for each codelist
map_df(
  codelists_list,
  ~ tibble(
    n_rows = nrow(.x),
    n_required = sum(.x$required == 1),
    n_optional = sum(.x$required == 0),
    n_read = sum(!is.na(.x$CleansedReadCode)),
    n_read_required = sum(!is.na(.x$CleansedReadCode) & .x$required == 1),
    n_read_optional = sum(!is.na(.x$CleansedReadCode) & .x$required == 0)
  ),
  .id = "codelist"
)
save(codelists, file = "output/codelists.RData")

# Structured output and deterministic scoring ----------------------------------

# Set up structured outputs
type_codelist <- type_object(
  codes = type_array(
    type_string("An OriginalReadCode, exactly as it appears in the source data."),
    "Every code in the final codelist."
  )
)

extraction_prompt <- paste(
  "List the OriginalReadCode, exactly as it appears in the source data, of every code in your final codelist.",
  "Include every code you selected and nothing else."
)

# Pull the code vector out of the list returned by $chat_structured(), dropping
# blanks and duplicates
codes_from_metadata <- function(x) {
  codes <- as.character(unlist(x$codes, use.names = FALSE))
  unique(codes[!is.na(codes) & nzchar(codes)])
}

# Split "OriginalReadCode Term required" lines back into a table
parse_target <- function(target) {
  lines <- str_split_1(target, "\n")
  lines <- lines[nzchar(lines)]
  tibble(
    code = str_extract(lines, "^\\S+"),
    term = str_squish(str_remove(str_remove(lines, "^\\S+"), "\\S+$")),
    required = as.integer(str_extract(lines, "\\S+$"))
  )
}

# Flatten a Chat into one string of everything that passed through its context
# (turn text plus tool results), so we can check whether a code was ever
# retrieved at all. Mirrors extract_chat_text() in eval.R.
extract_chat_text <- function(chat) {
  parts <- map(chat$get_turns(), function(turn) {
    text <- if (length(turn@text) > 0 && nzchar(turn@text)) turn@text else character(0)
    tool_text <- map_chr(turn@contents, function(content) {
      # ellmer's content classes are S7, so the class attribute is namespaced:
      # inherits(content, "ContentToolResult") alone is FALSE and would drop
      # every tool result (and with it every RAG/search hit) from the text
      if (!S7::S7_inherits(content, ContentToolResult)) {
        return("")
      }
      val <- content@value
      tryCatch(
        if (is.character(val)) {
          paste(val, collapse = "\n")
        } else if (is.data.frame(val)) {
          paste(apply(val, 1, paste, collapse = " "), collapse = "\n")
        } else if (is.list(val)) {
          paste(unlist(val), collapse = "\n")
        } else {
          paste(format(val), collapse = "\n")
        },
        error = function(e) ""
      )
    })
    c(text, tool_text)
  })
  paste(unlist(parts, use.names = FALSE), collapse = "\n")
}

# Which of `codes` appear anywhere in the solver's context. Fixed (substring)
# matching, as in eval.R, so a code counts as retrieved wherever it shows up in
# a tool result table.
retrieved_codes <- function(codes, context) {
  if (length(codes) == 0) {
    return(character(0))
  }
  codes[str_detect(context, fixed(codes))]
}

# Render codes for the explanation, with terms where they are known
format_codes <- function(codes, terms = NULL) {
  if (length(codes) == 0) {
    return("none")
  }
  if (is.null(terms)) {
    return(paste(codes, collapse = ", "))
  }
  terms[is.na(terms)] <- "(term unknown)"
  paste(paste(codes, terms), collapse = "; ")
}

# Scoring criteria:
#   C = all required codes present and no codes outside the target codelist
#   P = all required codes present but some codes outside the target codelist
#   I = one or more required codes missing
# `code_terms` is the code -> term named vector for the source data the solver
# searched (`cprd_code_terms` or `cprd_code_terms_read`). It labels codes that
# are not in the target codelist, and so have no term to take from it, and
# defines the set of codes that exist, so hallucinated codes can be reported
# separately.
# Each required code is also checked against the solver's chat context (turn
# text plus tool results) to split misses into retrieval failures (the code
# never reached the model) and usage failures (it was retrieved but left out of
# the final codelist).
detect_codelist <- function(code_terms) {
  function(samples) {
    res <- pmap(list(samples$target, samples$solver_metadata, samples$solver_chat), function(target, meta, chat) {
      gold <- parse_target(target)
      found <- codes_from_metadata(meta)
      required <- gold$code[gold$required == 1]

      missing_required <- setdiff(required, found)
      missing_optional <- setdiff(gold$code[gold$required == 0], found)
      irrelevant <- setdiff(found, gold$code)

      # Required codes that reached the model's context, and of those, the ones
      # it then failed to put in the final codelist
      retrieved <- retrieved_codes(required, extract_chat_text(chat))
      not_retrieved <- setdiff(required, retrieved)
      retrieved_not_used <- intersect(retrieved, missing_required)
      # Codes the model returned that do not exist in the source data at all
      # (a subset of `irrelevant`)
      nonexistent <- setdiff(found, names(code_terms))

      list(
        score = case_when(
          length(missing_required) > 0 ~ "I",
          length(irrelevant) > 0 ~ "P",
          TRUE ~ "C"
        ),
        explanation = paste0(
          length(found),
          " codes extracted; ",
          length(missing_optional),
          " optional target codes missing.\n",
          "Missing required (",
          length(missing_required),
          "): ",
          format_codes(
            missing_required,
            gold$term[match(missing_required, gold$code)]
          ),
          "\n",
          "Not in target codelist (",
          length(irrelevant),
          "): ",
          format_codes(
            irrelevant,
            unname(code_terms[irrelevant])
          ),
          "\n",
          "Not in source data (",
          length(nonexistent),
          "): ",
          format_codes(nonexistent),
          "\n",
          "Required codes retrieved into context: ",
          length(retrieved),
          "/",
          length(required),
          "\n",
          "Never retrieved (",
          length(not_retrieved),
          "): ",
          format_codes(
            not_retrieved,
            gold$term[match(not_retrieved, gold$code)]
          ),
          "\n",
          "Retrieved but not used (",
          length(retrieved_not_used),
          "): ",
          format_codes(
            retrieved_not_used,
            gold$term[match(retrieved_not_used, gold$code)]
          )
        ),
        metadata = list(
          n_found = length(found),
          n_missing_required = length(missing_required),
          n_missing_optional = length(missing_optional),
          n_irrelevant = length(irrelevant),
          n_nonexistent = length(nonexistent),
          n_required = length(required),
          n_retrieved = length(retrieved),
          n_not_retrieved = length(not_retrieved),
          n_retrieved_not_used = length(retrieved_not_used),
          pct_retrieved = if (length(required) == 0) {
            NA_real_
          } else {
            length(retrieved) / length(required)
          },
          missing_required = missing_required,
          missing_optional = missing_optional,
          irrelevant = irrelevant,
          nonexistent = nonexistent,
          not_retrieved = not_retrieved,
          retrieved_not_used = retrieved_not_used
        )
      )
    })

    list(
      score = factor(
        map_chr(res, "score"),
        levels = c("I", "P", "C"),
        ordered = TRUE
      ),
      explanation = map_chr(res, "explanation"),
      scorer_metadata = map(res, "metadata")
    )
  }
}

# #Take sample
# codelists <- codelists[7, ]

# Source data (CPRD browser) ---------------------------------------------------

# Loaded before any task is set up: the string search tools search it, and
# every scorer uses it to name codes outside the target codelist and to flag
# codes that do not exist at all.
base_file <- "..."
# base_file <- "..."
cprd_data <- haven::read_dta(base_file)

cprd_code_terms <- cprd_data |>
  select(originalreadcode, term) |>
  distinct(originalreadcode, .keep_all = TRUE) |>
  mutate(across(everything(), as.character)) |>
  deframe()

# The read-only arm sees only the codes that have a Read code: `cprd_data_read`
# is what the string search tools search, and `cprd_code_terms_read` is the set
# of codes that count as existing when scoring that arm
cprd_data_read <- cprd_data |>
  filter(cleansedreadcode != "")

cprd_code_terms_read <- cprd_data_read |>
  select(originalreadcode, term) |>
  distinct(originalreadcode, .keep_all = TRUE) |>
  mutate(across(everything(), as.character)) |>
  deframe()

# Claude chats: ellmer caps Claude at 4096 output tokens by default, and on
# these models thinking is always on and counts towards that cap, so a long
# codelist could be cut off. On a cutoff, $chat() warns and $chat_structured()
# errors, which would abort that codelist's eval. The flat default is an open
# ellmer issue (https://github.com/tidyverse/ellmer/issues/1150); this can go
# once ellmer defaults to each model's own output limit. Effort is left at each
# model's default (high for Sonnet 5.5, medium for Opus 5.5).
claude_params <- params(max_tokens = 16000)


# Set up RAG -------------------------------------------------------------

store_aurum <- ragnar_store_connect("aurum_gemini.ragnar.duckdb", read_only = TRUE)
store_aurum_read <- ragnar_store_connect("aurum_gemini_read.ragnar.duckdb", read_only = TRUE)

# Custom Solver (sequential version); returns a solver bound to one store
rag_solver <- function(store) {
  function(inputs, ..., solver_chat) {
    results <- character(length(inputs))
    chats <- list()
    codes <- list()

    for (i in seq_along(inputs)) {
      ch <- solver_chat$clone()
      ragnar_register_tool_retrieve(ch, store, top_k = 100, deoverlap = FALSE)
      ch$chat(inputs[i])
      results[i] <- ch$last_turn()@text
      # Follow-up turn on the same chat to extract the codelist as structured
      # data; the tool loop above has already finished by this point
      codes[[i]] <- ch$chat_structured(extraction_prompt, type = type_codelist)
      chats[[i]] <- ch
    }

    list(
      result = results,
      solver_chat = chats,
      solver_metadata = codes
    )
  }
}

system_prompt_single_step <- paste(
  "Create a codelist for the given clinical condition in the input.",
  "Include all codes that when recorded in a person's medical record would indicate that the person currently has the condition.",
  "Use only codes from the knowledge store."
  # "If necessary, perform multiple searches of the knowledge store for synonyms to ensure comprehensive code retrieval. ",
  # "Do not miss any relevant codes. Codelist can be of any length."
)

# Set up task (one codelist per task, so they can be run one at a time)
task_rag <- function(codelist, store = store_aurum, code_terms = cprd_code_terms) {
  Task$new(
    dataset = codelist,
    solver = rag_solver(store),
    scorer = detect_codelist(code_terms = code_terms),
    epochs = 3,
    name = "RAG"
  )
}

# The same task restricted to codes that have a Read code: the read-only store,
# scored against the read-filtered `codelists_read` targets and the matching
# code -> term map
task_rag_read <- function(codelist) {
  task_rag(codelist, store = store_aurum_read, code_terms = cprd_code_terms_read)
}

# Perform RAG eval -----------------------------------------------------------

# Gemini Flash Lite
gemini_flash_lite <- chat_google_gemini(model = "gemini-3.5-flash-lite", system_prompt = system_prompt_single_step)
rag_gemini_flash_lite_1 <- task_rag(codelists[1, ]) # Atopic eczema
rag_gemini_flash_lite_1$eval(solver_chat = gemini_flash_lite)
rag_gemini_flash_lite_2 <- task_rag(codelists[2, ]) # Vascular dementia
rag_gemini_flash_lite_2$eval(solver_chat = gemini_flash_lite)
rag_gemini_flash_lite_3 <- task_rag(codelists[3, ]) # Eosinophilic esophagitis
rag_gemini_flash_lite_3$eval(solver_chat = gemini_flash_lite)
rag_gemini_flash_lite_4 <- task_rag(codelists[4, ]) # Psoriasis
rag_gemini_flash_lite_4$eval(solver_chat = gemini_flash_lite)
rag_gemini_flash_lite_5 <- task_rag(codelists[5, ]) # Hidradenitis suppurativa
rag_gemini_flash_lite_5$eval(solver_chat = gemini_flash_lite)
rag_gemini_flash_lite_6 <- task_rag(codelists[6, ]) # Myocardial infarction
rag_gemini_flash_lite_6$eval(solver_chat = gemini_flash_lite)
rag_gemini_flash_lite_7 <- task_rag(codelists[7, ]) # Wrist fracture
rag_gemini_flash_lite_7$eval(solver_chat = gemini_flash_lite)
task_rag_gemini_flash_lite <- vitals_bind(
  rag_gemini_flash_lite_1,
  rag_gemini_flash_lite_2,
  rag_gemini_flash_lite_3,
  rag_gemini_flash_lite_4,
  rag_gemini_flash_lite_5,
  rag_gemini_flash_lite_6,
  rag_gemini_flash_lite_7
) |>
  mutate(task = "task_rag_gemini_flash_lite")
save(task_rag_gemini_flash_lite, file = "output/task_rag_gemini_flash_lite.RData")

# Gemini Flash
gemini_flash <- chat_google_gemini(model = "gemini-3.6-flash", system_prompt = system_prompt_single_step)
rag_gemini_flash_1 <- task_rag(codelists[1, ]) # Atopic eczema
rag_gemini_flash_1$eval(solver_chat = gemini_flash)
rag_gemini_flash_2 <- task_rag(codelists[2, ]) # Vascular dementia
rag_gemini_flash_2$eval(solver_chat = gemini_flash)
rag_gemini_flash_3 <- task_rag(codelists[3, ]) # Eosinophilic esophagitis
rag_gemini_flash_3$eval(solver_chat = gemini_flash)
rag_gemini_flash_4 <- task_rag(codelists[4, ]) # Psoriasis
rag_gemini_flash_4$eval(solver_chat = gemini_flash)
rag_gemini_flash_5 <- task_rag(codelists[5, ]) # Hidradenitis suppurativa
rag_gemini_flash_5$eval(solver_chat = gemini_flash)
rag_gemini_flash_6 <- task_rag(codelists[6, ]) # Myocardial infarction
rag_gemini_flash_6$eval(solver_chat = gemini_flash)
rag_gemini_flash_7 <- task_rag(codelists[7, ]) # Wrist fracture
rag_gemini_flash_7$eval(solver_chat = gemini_flash)
task_rag_gemini_flash <- vitals_bind(
  rag_gemini_flash_1,
  rag_gemini_flash_2,
  rag_gemini_flash_3,
  rag_gemini_flash_4,
  rag_gemini_flash_5,
  rag_gemini_flash_6,
  rag_gemini_flash_7
) |>
  mutate(task = "task_rag_gemini_flash")
save(task_rag_gemini_flash, file = "output/task_rag_gemini_flash.RData")

# Gemini Pro
gemini_pro <- chat_google_gemini(model = "gemini-3.1-pro-preview", system_prompt = system_prompt_single_step)
rag_gemini_pro_1 <- task_rag(codelists[1, ]) # Atopic eczema
rag_gemini_pro_1$eval(solver_chat = gemini_pro)
rag_gemini_pro_2 <- task_rag(codelists[2, ]) # Vascular dementia
rag_gemini_pro_2$eval(solver_chat = gemini_pro)
rag_gemini_pro_3 <- task_rag(codelists[3, ]) # Eosinophilic esophagitis
rag_gemini_pro_3$eval(solver_chat = gemini_pro)
rag_gemini_pro_4 <- task_rag(codelists[4, ]) # Psoriasis
rag_gemini_pro_4$eval(solver_chat = gemini_pro)
rag_gemini_pro_5 <- task_rag(codelists[5, ]) # Hidradenitis suppurativa
rag_gemini_pro_5$eval(solver_chat = gemini_pro)
rag_gemini_pro_6 <- task_rag(codelists[6, ]) # Myocardial infarction
rag_gemini_pro_6$eval(solver_chat = gemini_pro)
rag_gemini_pro_7 <- task_rag(codelists[7, ]) # Wrist fracture
rag_gemini_pro_7$eval(solver_chat = gemini_pro)
task_rag_gemini_pro <- vitals_bind(
  rag_gemini_pro_1,
  rag_gemini_pro_2,
  rag_gemini_pro_3,
  rag_gemini_pro_4,
  rag_gemini_pro_5,
  rag_gemini_pro_6,
  rag_gemini_pro_7
) |>
  mutate(task = "task_rag_gemini_pro")
save(task_rag_gemini_pro, file = "output/task_rag_gemini_pro.RData")

# GPT Luna
gpt_luna <- chat_openai(model = "gpt-6-luna", system_prompt = system_prompt_single_step)
rag_gpt_luna_1 <- task_rag(codelists[1, ]) # Atopic eczema
rag_gpt_luna_1$eval(solver_chat = gpt_luna)
rag_gpt_luna_2 <- task_rag(codelists[2, ]) # Vascular dementia
rag_gpt_luna_2$eval(solver_chat = gpt_luna)
rag_gpt_luna_3 <- task_rag(codelists[3, ]) # Eosinophilic esophagitis
rag_gpt_luna_3$eval(solver_chat = gpt_luna)
rag_gpt_luna_4 <- task_rag(codelists[4, ]) # Psoriasis
rag_gpt_luna_4$eval(solver_chat = gpt_luna)
rag_gpt_luna_5 <- task_rag(codelists[5, ]) # Hidradenitis suppurativa
rag_gpt_luna_5$eval(solver_chat = gpt_luna)
rag_gpt_luna_6 <- task_rag(codelists[6, ]) # Myocardial infarction
rag_gpt_luna_6$eval(solver_chat = gpt_luna)
rag_gpt_luna_7 <- task_rag(codelists[7, ]) # Wrist fracture
rag_gpt_luna_7$eval(solver_chat = gpt_luna)
task_rag_gpt_luna <- vitals_bind(
  rag_gpt_luna_1,
  rag_gpt_luna_2,
  rag_gpt_luna_3,
  rag_gpt_luna_4,
  rag_gpt_luna_5,
  rag_gpt_luna_6,
  rag_gpt_luna_7
) |>
  mutate(task = "task_rag_gpt_luna")
save(task_rag_gpt_luna, file = "output/task_rag_gpt_luna.RData")

# GPT Sol
gpt_sol <- chat_openai(model = "gpt-6.1-sol", system_prompt = system_prompt_single_step)
rag_gpt_sol_1 <- task_rag(codelists[1, ]) # Atopic eczema
rag_gpt_sol_1$eval(solver_chat = gpt_sol)
rag_gpt_sol_2 <- task_rag(codelists[2, ]) # Vascular dementia
rag_gpt_sol_2$eval(solver_chat = gpt_sol)
rag_gpt_sol_3 <- task_rag(codelists[3, ]) # Eosinophilic esophagitis
rag_gpt_sol_3$eval(solver_chat = gpt_sol)
rag_gpt_sol_4 <- task_rag(codelists[4, ]) # Psoriasis
rag_gpt_sol_4$eval(solver_chat = gpt_sol)
rag_gpt_sol_5 <- task_rag(codelists[5, ]) # Hidradenitis suppurativa
rag_gpt_sol_5$eval(solver_chat = gpt_sol)
rag_gpt_sol_6 <- task_rag(codelists[6, ]) # Myocardial infarction
rag_gpt_sol_6$eval(solver_chat = gpt_sol)
rag_gpt_sol_7 <- task_rag(codelists[7, ]) # Wrist fracture
rag_gpt_sol_7$eval(solver_chat = gpt_sol)
task_rag_gpt_sol <- vitals_bind(
  rag_gpt_sol_1,
  rag_gpt_sol_2,
  rag_gpt_sol_3,
  rag_gpt_sol_4,
  rag_gpt_sol_5,
  rag_gpt_sol_6,
  rag_gpt_sol_7
) |>
  mutate(task = "task_rag_gpt_sol")
save(task_rag_gpt_sol, file = "output/task_rag_gpt_sol.RData")

# Claude Sonnet
claude_sonnet <- chat_claude(model = "claude-sonnet-5-5", system_prompt = system_prompt_single_step, params = claude_params)
rag_claude_sonnet_1 <- task_rag(codelists[1, ]) # Atopic eczema
rag_claude_sonnet_1$eval(solver_chat = claude_sonnet)
rag_claude_sonnet_2 <- task_rag(codelists[2, ]) # Vascular dementia
rag_claude_sonnet_2$eval(solver_chat = claude_sonnet)
rag_claude_sonnet_3 <- task_rag(codelists[3, ]) # Eosinophilic esophagitis
rag_claude_sonnet_3$eval(solver_chat = claude_sonnet)
rag_claude_sonnet_4 <- task_rag(codelists[4, ]) # Psoriasis
rag_claude_sonnet_4$eval(solver_chat = claude_sonnet)
rag_claude_sonnet_5 <- task_rag(codelists[5, ]) # Hidradenitis suppurativa
rag_claude_sonnet_5$eval(solver_chat = claude_sonnet)
rag_claude_sonnet_6 <- task_rag(codelists[6, ]) # Myocardial infarction
rag_claude_sonnet_6$eval(solver_chat = claude_sonnet)
rag_claude_sonnet_7 <- task_rag(codelists[7, ]) # Wrist fracture
rag_claude_sonnet_7$eval(solver_chat = claude_sonnet)
task_rag_claude_sonnet <- vitals_bind(
  rag_claude_sonnet_1,
  rag_claude_sonnet_2,
  rag_claude_sonnet_3,
  rag_claude_sonnet_4,
  rag_claude_sonnet_5,
  rag_claude_sonnet_6,
  rag_claude_sonnet_7
) |>
  mutate(task = "task_rag_claude_sonnet")
save(task_rag_claude_sonnet, file = "output/task_rag_claude_sonnet.RData")

# Claude Opus
claude_opus <- chat_claude(model = "claude-opus-5-5", system_prompt = system_prompt_single_step, params = claude_params)
rag_claude_opus_1 <- task_rag(codelists[1, ]) # Atopic eczema
rag_claude_opus_1$eval(solver_chat = claude_opus)
rag_claude_opus_2 <- task_rag(codelists[2, ]) # Vascular dementia
rag_claude_opus_2$eval(solver_chat = claude_opus)
rag_claude_opus_3 <- task_rag(codelists[3, ]) # Eosinophilic esophagitis
rag_claude_opus_3$eval(solver_chat = claude_opus)
rag_claude_opus_4 <- task_rag(codelists[4, ]) # Psoriasis
rag_claude_opus_4$eval(solver_chat = claude_opus)
rag_claude_opus_5 <- task_rag(codelists[5, ]) # Hidradenitis suppurativa
rag_claude_opus_5$eval(solver_chat = claude_opus)
rag_claude_opus_6 <- task_rag(codelists[6, ]) # Myocardial infarction
rag_claude_opus_6$eval(solver_chat = claude_opus)
rag_claude_opus_7 <- task_rag(codelists[7, ]) # Wrist fracture
rag_claude_opus_7$eval(solver_chat = claude_opus)
task_rag_claude_opus <- vitals_bind(
  rag_claude_opus_1,
  rag_claude_opus_2,
  rag_claude_opus_3,
  rag_claude_opus_4,
  rag_claude_opus_5,
  rag_claude_opus_6,
  rag_claude_opus_7
) |>
  mutate(task = "task_rag_claude_opus")
save(task_rag_claude_opus, file = "output/task_rag_claude_opus.RData")

# Perform RAG eval (Read codes only) -----------------------------------------

# Objects and files carry the `_read` suffix, but the `task` column keeps the
# name of the full-browser run so both arms share facet labels in eval.R

# Gemini Flash Lite
gemini_flash_lite <- chat_google_gemini(model = "gemini-3.5-flash-lite", system_prompt = system_prompt_single_step)
rag_read_gemini_flash_lite_1 <- task_rag_read(codelists_read[1, ]) # Atopic eczema
rag_read_gemini_flash_lite_1$eval(solver_chat = gemini_flash_lite)
rag_read_gemini_flash_lite_2 <- task_rag_read(codelists_read[2, ]) # Vascular dementia
rag_read_gemini_flash_lite_2$eval(solver_chat = gemini_flash_lite)
rag_read_gemini_flash_lite_3 <- task_rag_read(codelists_read[3, ]) # Eosinophilic esophagitis
rag_read_gemini_flash_lite_3$eval(solver_chat = gemini_flash_lite)
rag_read_gemini_flash_lite_4 <- task_rag_read(codelists_read[4, ]) # Psoriasis
rag_read_gemini_flash_lite_4$eval(solver_chat = gemini_flash_lite)
rag_read_gemini_flash_lite_5 <- task_rag_read(codelists_read[5, ]) # Hidradenitis suppurativa
rag_read_gemini_flash_lite_5$eval(solver_chat = gemini_flash_lite)
rag_read_gemini_flash_lite_6 <- task_rag_read(codelists_read[6, ]) # Myocardial infarction
rag_read_gemini_flash_lite_6$eval(solver_chat = gemini_flash_lite)
rag_read_gemini_flash_lite_7 <- task_rag_read(codelists_read[7, ]) # Wrist fracture
rag_read_gemini_flash_lite_7$eval(solver_chat = gemini_flash_lite)
task_rag_gemini_flash_lite_read <- vitals_bind(
  rag_read_gemini_flash_lite_1,
  rag_read_gemini_flash_lite_2,
  rag_read_gemini_flash_lite_3,
  rag_read_gemini_flash_lite_4,
  rag_read_gemini_flash_lite_5,
  rag_read_gemini_flash_lite_6,
  rag_read_gemini_flash_lite_7
) |>
  mutate(task = "task_rag_gemini_flash_lite")
save(task_rag_gemini_flash_lite_read, file = "output/task_rag_gemini_flash_lite_read.RData")

# Gemini Flash
gemini_flash <- chat_google_gemini(model = "gemini-3.6-flash", system_prompt = system_prompt_single_step)
rag_read_gemini_flash_1 <- task_rag_read(codelists_read[1, ]) # Atopic eczema
rag_read_gemini_flash_1$eval(solver_chat = gemini_flash)
rag_read_gemini_flash_2 <- task_rag_read(codelists_read[2, ]) # Vascular dementia
rag_read_gemini_flash_2$eval(solver_chat = gemini_flash)
rag_read_gemini_flash_3 <- task_rag_read(codelists_read[3, ]) # Eosinophilic esophagitis
rag_read_gemini_flash_3$eval(solver_chat = gemini_flash)
rag_read_gemini_flash_4 <- task_rag_read(codelists_read[4, ]) # Psoriasis
rag_read_gemini_flash_4$eval(solver_chat = gemini_flash)
rag_read_gemini_flash_5 <- task_rag_read(codelists_read[5, ]) # Hidradenitis suppurativa
rag_read_gemini_flash_5$eval(solver_chat = gemini_flash)
rag_read_gemini_flash_6 <- task_rag_read(codelists_read[6, ]) # Myocardial infarction
rag_read_gemini_flash_6$eval(solver_chat = gemini_flash)
rag_read_gemini_flash_7 <- task_rag_read(codelists_read[7, ]) # Wrist fracture
rag_read_gemini_flash_7$eval(solver_chat = gemini_flash)
task_rag_gemini_flash_read <- vitals_bind(
  rag_read_gemini_flash_1,
  rag_read_gemini_flash_2,
  rag_read_gemini_flash_3,
  rag_read_gemini_flash_4,
  rag_read_gemini_flash_5,
  rag_read_gemini_flash_6,
  rag_read_gemini_flash_7
) |>
  mutate(task = "task_rag_gemini_flash")
save(task_rag_gemini_flash_read, file = "output/task_rag_gemini_flash_read.RData")

# Gemini Pro
gemini_pro <- chat_google_gemini(model = "gemini-3.1-pro-preview", system_prompt = system_prompt_single_step)
rag_read_gemini_pro_1 <- task_rag_read(codelists_read[1, ]) # Atopic eczema
rag_read_gemini_pro_1$eval(solver_chat = gemini_pro)
rag_read_gemini_pro_2 <- task_rag_read(codelists_read[2, ]) # Vascular dementia
rag_read_gemini_pro_2$eval(solver_chat = gemini_pro)
rag_read_gemini_pro_3 <- task_rag_read(codelists_read[3, ]) # Eosinophilic esophagitis
rag_read_gemini_pro_3$eval(solver_chat = gemini_pro)
rag_read_gemini_pro_4 <- task_rag_read(codelists_read[4, ]) # Psoriasis
rag_read_gemini_pro_4$eval(solver_chat = gemini_pro)
rag_read_gemini_pro_5 <- task_rag_read(codelists_read[5, ]) # Hidradenitis suppurativa
rag_read_gemini_pro_5$eval(solver_chat = gemini_pro)
rag_read_gemini_pro_6 <- task_rag_read(codelists_read[6, ]) # Myocardial infarction
rag_read_gemini_pro_6$eval(solver_chat = gemini_pro)
rag_read_gemini_pro_7 <- task_rag_read(codelists_read[7, ]) # Wrist fracture
rag_read_gemini_pro_7$eval(solver_chat = gemini_pro)
task_rag_gemini_pro_read <- vitals_bind(
  rag_read_gemini_pro_1,
  rag_read_gemini_pro_2,
  rag_read_gemini_pro_3,
  rag_read_gemini_pro_4,
  rag_read_gemini_pro_5,
  rag_read_gemini_pro_6,
  rag_read_gemini_pro_7
) |>
  mutate(task = "task_rag_gemini_pro")
save(task_rag_gemini_pro_read, file = "output/task_rag_gemini_pro_read.RData")

# GPT Luna
gpt_luna <- chat_openai(model = "gpt-6-luna", system_prompt = system_prompt_single_step)
rag_read_gpt_luna_1 <- task_rag_read(codelists_read[1, ]) # Atopic eczema
rag_read_gpt_luna_1$eval(solver_chat = gpt_luna)
rag_read_gpt_luna_2 <- task_rag_read(codelists_read[2, ]) # Vascular dementia
rag_read_gpt_luna_2$eval(solver_chat = gpt_luna)
rag_read_gpt_luna_3 <- task_rag_read(codelists_read[3, ]) # Eosinophilic esophagitis
rag_read_gpt_luna_3$eval(solver_chat = gpt_luna)
rag_read_gpt_luna_4 <- task_rag_read(codelists_read[4, ]) # Psoriasis
rag_read_gpt_luna_4$eval(solver_chat = gpt_luna)
rag_read_gpt_luna_5 <- task_rag_read(codelists_read[5, ]) # Hidradenitis suppurativa
rag_read_gpt_luna_5$eval(solver_chat = gpt_luna)
rag_read_gpt_luna_6 <- task_rag_read(codelists_read[6, ]) # Myocardial infarction
rag_read_gpt_luna_6$eval(solver_chat = gpt_luna)
rag_read_gpt_luna_7 <- task_rag_read(codelists_read[7, ]) # Wrist fracture
rag_read_gpt_luna_7$eval(solver_chat = gpt_luna)
task_rag_gpt_luna_read <- vitals_bind(
  rag_read_gpt_luna_1,
  rag_read_gpt_luna_2,
  rag_read_gpt_luna_3,
  rag_read_gpt_luna_4,
  rag_read_gpt_luna_5,
  rag_read_gpt_luna_6,
  rag_read_gpt_luna_7
) |>
  mutate(task = "task_rag_gpt_luna")
save(task_rag_gpt_luna_read, file = "output/task_rag_gpt_luna_read.RData")

# GPT Sol
gpt_sol <- chat_openai(model = "gpt-6.1-sol", system_prompt = system_prompt_single_step)
rag_read_gpt_sol_1 <- task_rag_read(codelists_read[1, ]) # Atopic eczema
rag_read_gpt_sol_1$eval(solver_chat = gpt_sol)
rag_read_gpt_sol_2 <- task_rag_read(codelists_read[2, ]) # Vascular dementia
rag_read_gpt_sol_2$eval(solver_chat = gpt_sol)
rag_read_gpt_sol_3 <- task_rag_read(codelists_read[3, ]) # Eosinophilic esophagitis
rag_read_gpt_sol_3$eval(solver_chat = gpt_sol)
rag_read_gpt_sol_4 <- task_rag_read(codelists_read[4, ]) # Psoriasis
rag_read_gpt_sol_4$eval(solver_chat = gpt_sol)
rag_read_gpt_sol_5 <- task_rag_read(codelists_read[5, ]) # Hidradenitis suppurativa
rag_read_gpt_sol_5$eval(solver_chat = gpt_sol)
rag_read_gpt_sol_6 <- task_rag_read(codelists_read[6, ]) # Myocardial infarction
rag_read_gpt_sol_6$eval(solver_chat = gpt_sol)
rag_read_gpt_sol_7 <- task_rag_read(codelists_read[7, ]) # Wrist fracture
rag_read_gpt_sol_7$eval(solver_chat = gpt_sol)
task_rag_gpt_sol_read <- vitals_bind(
  rag_read_gpt_sol_1,
  rag_read_gpt_sol_2,
  rag_read_gpt_sol_3,
  rag_read_gpt_sol_4,
  rag_read_gpt_sol_5,
  rag_read_gpt_sol_6,
  rag_read_gpt_sol_7
) |>
  mutate(task = "task_rag_gpt_sol")
save(task_rag_gpt_sol_read, file = "output/task_rag_gpt_sol_read.RData")

# Claude Sonnet
claude_sonnet <- chat_claude(model = "claude-sonnet-5-5", system_prompt = system_prompt_single_step, params = claude_params)
rag_read_claude_sonnet_1 <- task_rag_read(codelists_read[1, ]) # Atopic eczema
rag_read_claude_sonnet_1$eval(solver_chat = claude_sonnet)
rag_read_claude_sonnet_2 <- task_rag_read(codelists_read[2, ]) # Vascular dementia
rag_read_claude_sonnet_2$eval(solver_chat = claude_sonnet)
rag_read_claude_sonnet_3 <- task_rag_read(codelists_read[3, ]) # Eosinophilic esophagitis
rag_read_claude_sonnet_3$eval(solver_chat = claude_sonnet)
rag_read_claude_sonnet_4 <- task_rag_read(codelists_read[4, ]) # Psoriasis
rag_read_claude_sonnet_4$eval(solver_chat = claude_sonnet)
rag_read_claude_sonnet_5 <- task_rag_read(codelists_read[5, ]) # Hidradenitis suppurativa
rag_read_claude_sonnet_5$eval(solver_chat = claude_sonnet)
rag_read_claude_sonnet_6 <- task_rag_read(codelists_read[6, ]) # Myocardial infarction
rag_read_claude_sonnet_6$eval(solver_chat = claude_sonnet)
rag_read_claude_sonnet_7 <- task_rag_read(codelists_read[7, ]) # Wrist fracture
rag_read_claude_sonnet_7$eval(solver_chat = claude_sonnet)
task_rag_claude_sonnet_read <- vitals_bind(
  rag_read_claude_sonnet_1,
  rag_read_claude_sonnet_2,
  rag_read_claude_sonnet_3,
  rag_read_claude_sonnet_4,
  rag_read_claude_sonnet_5,
  rag_read_claude_sonnet_6,
  rag_read_claude_sonnet_7
) |>
  mutate(task = "task_rag_claude_sonnet")
save(task_rag_claude_sonnet_read, file = "output/task_rag_claude_sonnet_read.RData")

# Claude Opus
claude_opus <- chat_claude(model = "claude-opus-5-5", system_prompt = system_prompt_single_step, params = claude_params)
rag_read_claude_opus_1 <- task_rag_read(codelists_read[1, ]) # Atopic eczema
rag_read_claude_opus_1$eval(solver_chat = claude_opus)
rag_read_claude_opus_2 <- task_rag_read(codelists_read[2, ]) # Vascular dementia
rag_read_claude_opus_2$eval(solver_chat = claude_opus)
rag_read_claude_opus_3 <- task_rag_read(codelists_read[3, ]) # Eosinophilic esophagitis
rag_read_claude_opus_3$eval(solver_chat = claude_opus)
rag_read_claude_opus_4 <- task_rag_read(codelists_read[4, ]) # Psoriasis
rag_read_claude_opus_4$eval(solver_chat = claude_opus)
rag_read_claude_opus_5 <- task_rag_read(codelists_read[5, ]) # Hidradenitis suppurativa
rag_read_claude_opus_5$eval(solver_chat = claude_opus)
rag_read_claude_opus_6 <- task_rag_read(codelists_read[6, ]) # Myocardial infarction
rag_read_claude_opus_6$eval(solver_chat = claude_opus)
rag_read_claude_opus_7 <- task_rag_read(codelists_read[7, ]) # Wrist fracture
rag_read_claude_opus_7$eval(solver_chat = claude_opus)
task_rag_claude_opus_read <- vitals_bind(
  rag_read_claude_opus_1,
  rag_read_claude_opus_2,
  rag_read_claude_opus_3,
  rag_read_claude_opus_4,
  rag_read_claude_opus_5,
  rag_read_claude_opus_6,
  rag_read_claude_opus_7
) |>
  mutate(task = "task_rag_claude_opus")
save(task_rag_claude_opus_read, file = "output/task_rag_claude_opus_read.RData")

# Setup string search -----------------------------------------------------

# `cprd_data`, `cprd_data_read` and the code -> term maps are loaded in the
# source data section above

# Create string search tool; bound to the data it searches, so the same solver
# serves the full browser and the read-only subset.
# The data is named rather than passed by value, and looked up when the tool
# runs: a closure over the data frame is serialised into every saved Chat (the
# tool objects survive in the turns even if the chat's tools are cleared), which
# made a single sample of the string search eval ~80 MB.
make_tool_term_search_cprd <- function(data_name = "cprd_data") {
  force(data_name)
  stopifnot(is.character(data_name))

  fn <- function(search_term) {
    search_term <- paste0("(?=.*", str_split_1(search_term, " "), ")", collapse = "")

    results <- get(data_name, envir = globalenv()) |>
      filter(str_detect(tolower(as.character(term)), tolower(search_term)))

    if (nrow(results) == 0) {
      return("No matches found.")
    }

    # Only the code and the term reach the model; the other browser columns
    # (medcodeid, snomed ids, cleansedreadcode, ...) are for filtering only and
    # tripled the size of every tool result when they were included
    results |>
      select(originalreadcode, term) |>
      head(500) |>
      knitr::kable(format = "markdown")
  }

  tool(
    fn,
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
}

# Create code search tool
make_tool_code_search_cprd <- function(data_name = "cprd_data") {
  force(data_name)
  stopifnot(is.character(data_name))

  fn <- function(search_term) {
    search_term <- paste0("^", search_term)

    results <- get(data_name, envir = globalenv()) |>
      filter(str_detect(tolower(as.character(originalreadcode)), tolower(search_term)))

    if (nrow(results) == 0) {
      return("No matches found.")
    }

    results |>
      select(originalreadcode, term) |>
      head(500) |>
      knitr::kable(format = "markdown")
  }

  tool(
    fn,
    name = "tool_code_search_cprd",
    description = paste(
      "Search for records in CPRD using a specified Read code.",
      "The tool detects the presence of the searchterm at the start of the string in the code column."
    ),
    arguments = list(
      search_term = type_string("The code to search for in the CPRD records.")
    )
  )
}

# Set up custom solver; returns a solver whose tools search the data frame
# called `data_name`
stringsearch_solver <- function(data_name = "cprd_data") {
  function(inputs, ..., solver_chat) {
    ch <- solver_chat$clone()
    ch$register_tool(make_tool_term_search_cprd(data_name))
    ch$register_tool(make_tool_code_search_cprd(data_name))

    res <- ellmer::parallel_chat(ch, as.list(inputs))
    results <- purrr::map_chr(res, function(c) c$last_turn()@text)

    # Follow-up turn on each chat to extract the codelist as structured data.
    # Must come after `results` is taken, since it replaces the last turn.
    codes <- purrr::map(
      res,
      function(c) c$chat_structured(extraction_prompt, type = type_codelist)
    )

    list(
      result = results,
      solver_chat = res,
      solver_metadata = codes
    )
  }
}

system_prompt_stringsearch <- paste(
  "Create a codelist for the given clinical condition in the input.",
  "Include all codes that when recorded in a person's medical record would indicate that the person currently has the condition.",
  "Use the CPRD term search tool and code search tool.",
  "Do not miss any relevant codes. Codelist can be of any length.",
  "Supply codelists as a tables."
)


# Set up task (one codelist per task, so they can be run one at a time)
task_stringsearch <- function(codelist, data_name = "cprd_data", code_terms = cprd_code_terms) {
  Task$new(
    dataset = codelist,
    solver = stringsearch_solver(data_name),
    scorer = detect_codelist(code_terms = code_terms),
    epochs = 3,
    name = "String Search"
  )
}

# The same task restricted to codes that have a Read code: the tools search the
# read-only subset, scored against the read-filtered `codelists_read` targets
# and the matching code -> term map
task_stringsearch_read <- function(codelist) {
  task_stringsearch(codelist, data_name = "cprd_data_read", code_terms = cprd_code_terms_read)
}

# Perform String Search eval ---------------------------------------------------

# Gemini Flash Lite
gemini_flash_lite <- chat_google_gemini(model = "gemini-3.5-flash-lite", system_prompt = system_prompt_stringsearch)
ss_gemini_flash_lite_1 <- task_stringsearch(codelists[1, ]) # Atopic eczema
ss_gemini_flash_lite_1$eval(solver_chat = gemini_flash_lite)
ss_gemini_flash_lite_2 <- task_stringsearch(codelists[2, ]) # Vascular dementia
ss_gemini_flash_lite_2$eval(solver_chat = gemini_flash_lite)
ss_gemini_flash_lite_3 <- task_stringsearch(codelists[3, ]) # Eosinophilic esophagitis
ss_gemini_flash_lite_3$eval(solver_chat = gemini_flash_lite)
ss_gemini_flash_lite_4 <- task_stringsearch(codelists[4, ]) # Psoriasis
ss_gemini_flash_lite_4$eval(solver_chat = gemini_flash_lite)
ss_gemini_flash_lite_5 <- task_stringsearch(codelists[5, ]) # Hidradenitis suppurativa
ss_gemini_flash_lite_5$eval(solver_chat = gemini_flash_lite)
ss_gemini_flash_lite_6 <- task_stringsearch(codelists[6, ]) # Myocardial infarction
ss_gemini_flash_lite_6$eval(solver_chat = gemini_flash_lite)
ss_gemini_flash_lite_7 <- task_stringsearch(codelists[7, ]) # Wrist fracture
ss_gemini_flash_lite_7$eval(solver_chat = gemini_flash_lite)
task_stringsearch_gemini_flash_lite <- vitals_bind(
  ss_gemini_flash_lite_1,
  ss_gemini_flash_lite_2,
  ss_gemini_flash_lite_3,
  ss_gemini_flash_lite_4,
  ss_gemini_flash_lite_5,
  ss_gemini_flash_lite_6,
  ss_gemini_flash_lite_7
) |>
  mutate(task = "task_stringsearch_gemini_flash_lite")
save(task_stringsearch_gemini_flash_lite, file = "output/task_stringsearch_gemini_flash_lite.RData")

# Gemini Flash
gemini_flash <- chat_google_gemini(model = "gemini-3.6-flash", system_prompt = system_prompt_stringsearch)
ss_gemini_flash_1 <- task_stringsearch(codelists[1, ]) # Atopic eczema
ss_gemini_flash_1$eval(solver_chat = gemini_flash)
ss_gemini_flash_2 <- task_stringsearch(codelists[2, ]) # Vascular dementia
ss_gemini_flash_2$eval(solver_chat = gemini_flash)
ss_gemini_flash_3 <- task_stringsearch(codelists[3, ]) # Eosinophilic esophagitis
ss_gemini_flash_3$eval(solver_chat = gemini_flash)
ss_gemini_flash_4 <- task_stringsearch(codelists[4, ]) # Psoriasis
ss_gemini_flash_4$eval(solver_chat = gemini_flash)
ss_gemini_flash_5 <- task_stringsearch(codelists[5, ]) # Hidradenitis suppurativa
ss_gemini_flash_5$eval(solver_chat = gemini_flash)
ss_gemini_flash_6 <- task_stringsearch(codelists[6, ]) # Myocardial infarction
ss_gemini_flash_6$eval(solver_chat = gemini_flash)
ss_gemini_flash_7 <- task_stringsearch(codelists[7, ]) # Wrist fracture
ss_gemini_flash_7$eval(solver_chat = gemini_flash)
task_stringsearch_gemini_flash <- vitals_bind(
  ss_gemini_flash_1,
  ss_gemini_flash_2,
  ss_gemini_flash_3,
  ss_gemini_flash_4,
  ss_gemini_flash_5,
  ss_gemini_flash_6,
  ss_gemini_flash_7
) |>
  mutate(task = "task_stringsearch_gemini_flash")
save(task_stringsearch_gemini_flash, file = "output/task_stringsearch_gemini_flash.RData")

# Gemini Pro
gemini_pro <- chat_google_gemini(model = "gemini-3.1-pro-preview", system_prompt = system_prompt_stringsearch)
ss_gemini_pro_1 <- task_stringsearch(codelists[1, ]) # Atopic eczema
ss_gemini_pro_1$eval(solver_chat = gemini_pro)
ss_gemini_pro_2 <- task_stringsearch(codelists[2, ]) # Vascular dementia
ss_gemini_pro_2$eval(solver_chat = gemini_pro)
ss_gemini_pro_3 <- task_stringsearch(codelists[3, ]) # Eosinophilic esophagitis
ss_gemini_pro_3$eval(solver_chat = gemini_pro)
ss_gemini_pro_4 <- task_stringsearch(codelists[4, ]) # Psoriasis
ss_gemini_pro_4$eval(solver_chat = gemini_pro)
ss_gemini_pro_5 <- task_stringsearch(codelists[5, ]) # Hidradenitis suppurativa
ss_gemini_pro_5$eval(solver_chat = gemini_pro)
ss_gemini_pro_6 <- task_stringsearch(codelists[6, ]) # Myocardial infarction
ss_gemini_pro_6$eval(solver_chat = gemini_pro)
ss_gemini_pro_7 <- task_stringsearch(codelists[7, ]) # Wrist fracture
ss_gemini_pro_7$eval(solver_chat = gemini_pro)
task_stringsearch_gemini_pro <- vitals_bind(
  ss_gemini_pro_1,
  ss_gemini_pro_2,
  ss_gemini_pro_3,
  ss_gemini_pro_4,
  ss_gemini_pro_5,
  ss_gemini_pro_6,
  ss_gemini_pro_7
) |>
  mutate(task = "task_stringsearch_gemini_pro")
save(task_stringsearch_gemini_pro, file = "output/task_stringsearch_gemini_pro.RData")

# GPT Luna
gpt_luna <- chat_openai(model = "gpt-6-luna", system_prompt = system_prompt_stringsearch)
ss_gpt_luna_1 <- task_stringsearch(codelists[1, ]) # Atopic eczema
ss_gpt_luna_1$eval(solver_chat = gpt_luna)
ss_gpt_luna_2 <- task_stringsearch(codelists[2, ]) # Vascular dementia
ss_gpt_luna_2$eval(solver_chat = gpt_luna)
ss_gpt_luna_3 <- task_stringsearch(codelists[3, ]) # Eosinophilic esophagitis
ss_gpt_luna_3$eval(solver_chat = gpt_luna)
ss_gpt_luna_4 <- task_stringsearch(codelists[4, ]) # Psoriasis
ss_gpt_luna_4$eval(solver_chat = gpt_luna)
ss_gpt_luna_5 <- task_stringsearch(codelists[5, ]) # Hidradenitis suppurativa
ss_gpt_luna_5$eval(solver_chat = gpt_luna)
ss_gpt_luna_6 <- task_stringsearch(codelists[6, ]) # Myocardial infarction
ss_gpt_luna_6$eval(solver_chat = gpt_luna)
ss_gpt_luna_7 <- task_stringsearch(codelists[7, ]) # Wrist fracture
ss_gpt_luna_7$eval(solver_chat = gpt_luna)
task_stringsearch_gpt_luna <- vitals_bind(
  ss_gpt_luna_1,
  ss_gpt_luna_2,
  ss_gpt_luna_3,
  ss_gpt_luna_4,
  ss_gpt_luna_5,
  ss_gpt_luna_6,
  ss_gpt_luna_7
) |>
  mutate(task = "task_stringsearch_gpt_luna")
save(task_stringsearch_gpt_luna, file = "output/task_stringsearch_gpt_luna.RData")

# GPT Sol
gpt_sol <- chat_openai(model = "gpt-6.1-sol", system_prompt = system_prompt_stringsearch)
ss_gpt_sol_1 <- task_stringsearch(codelists[1, ]) # Atopic eczema
ss_gpt_sol_1$eval(solver_chat = gpt_sol)
ss_gpt_sol_2 <- task_stringsearch(codelists[2, ]) # Vascular dementia
ss_gpt_sol_2$eval(solver_chat = gpt_sol)
ss_gpt_sol_3 <- task_stringsearch(codelists[3, ]) # Eosinophilic esophagitis
ss_gpt_sol_3$eval(solver_chat = gpt_sol)
ss_gpt_sol_4 <- task_stringsearch(codelists[4, ]) # Psoriasis
ss_gpt_sol_4$eval(solver_chat = gpt_sol)
ss_gpt_sol_5 <- task_stringsearch(codelists[5, ]) # Hidradenitis suppurativa
ss_gpt_sol_5$eval(solver_chat = gpt_sol)
ss_gpt_sol_6 <- task_stringsearch(codelists[6, ]) # Myocardial infarction
ss_gpt_sol_6$eval(solver_chat = gpt_sol)
ss_gpt_sol_7 <- task_stringsearch(codelists[7, ]) # Wrist fracture
ss_gpt_sol_7$eval(solver_chat = gpt_sol)
task_stringsearch_gpt_sol <- vitals_bind(
  ss_gpt_sol_1,
  ss_gpt_sol_2,
  ss_gpt_sol_3,
  ss_gpt_sol_4,
  ss_gpt_sol_5,
  ss_gpt_sol_6,
  ss_gpt_sol_7
) |>
  mutate(task = "task_stringsearch_gpt_sol")
save(task_stringsearch_gpt_sol, file = "output/task_stringsearch_gpt_sol.RData")

# Claude Sonnet
claude_sonnet <- chat_claude(model = "claude-sonnet-5-5", system_prompt = system_prompt_stringsearch, params = claude_params)
ss_claude_sonnet_1 <- task_stringsearch(codelists[1, ]) # Atopic eczema
ss_claude_sonnet_1$eval(solver_chat = claude_sonnet)
ss_claude_sonnet_2 <- task_stringsearch(codelists[2, ]) # Vascular dementia
ss_claude_sonnet_2$eval(solver_chat = claude_sonnet)
ss_claude_sonnet_3 <- task_stringsearch(codelists[3, ]) # Eosinophilic esophagitis
ss_claude_sonnet_3$eval(solver_chat = claude_sonnet)
ss_claude_sonnet_4 <- task_stringsearch(codelists[4, ]) # Psoriasis
ss_claude_sonnet_4$eval(solver_chat = claude_sonnet)
ss_claude_sonnet_5 <- task_stringsearch(codelists[5, ]) # Hidradenitis suppurativa
ss_claude_sonnet_5$eval(solver_chat = claude_sonnet)
ss_claude_sonnet_6 <- task_stringsearch(codelists[6, ]) # Myocardial infarction
ss_claude_sonnet_6$eval(solver_chat = claude_sonnet)
ss_claude_sonnet_7 <- task_stringsearch(codelists[7, ]) # Wrist fracture
ss_claude_sonnet_7$eval(solver_chat = claude_sonnet)
task_stringsearch_claude_sonnet <- vitals_bind(
  ss_claude_sonnet_1,
  ss_claude_sonnet_2,
  ss_claude_sonnet_3,
  ss_claude_sonnet_4,
  ss_claude_sonnet_5,
  ss_claude_sonnet_6,
  ss_claude_sonnet_7
) |>
  mutate(task = "task_stringsearch_claude_sonnet")
save(task_stringsearch_claude_sonnet, file = "output/task_stringsearch_claude_sonnet.RData")

# Claude Opus
claude_opus <- chat_claude(model = "claude-opus-5-5", system_prompt = system_prompt_stringsearch, params = claude_params)
ss_claude_opus_1 <- task_stringsearch(codelists[1, ]) # Atopic eczema
ss_claude_opus_1$eval(solver_chat = claude_opus)
ss_claude_opus_2 <- task_stringsearch(codelists[2, ]) # Vascular dementia
ss_claude_opus_2$eval(solver_chat = claude_opus)
ss_claude_opus_3 <- task_stringsearch(codelists[3, ]) # Eosinophilic esophagitis
ss_claude_opus_3$eval(solver_chat = claude_opus)
ss_claude_opus_4 <- task_stringsearch(codelists[4, ]) # Psoriasis
ss_claude_opus_4$eval(solver_chat = claude_opus)
ss_claude_opus_5 <- task_stringsearch(codelists[5, ]) # Hidradenitis suppurativa
ss_claude_opus_5$eval(solver_chat = claude_opus)
ss_claude_opus_6 <- task_stringsearch(codelists[6, ]) # Myocardial infarction
ss_claude_opus_6$eval(solver_chat = claude_opus)
ss_claude_opus_7 <- task_stringsearch(codelists[7, ]) # Wrist fracture
ss_claude_opus_7$eval(solver_chat = claude_opus)
task_stringsearch_claude_opus <- vitals_bind(
  ss_claude_opus_1,
  ss_claude_opus_2,
  ss_claude_opus_3,
  ss_claude_opus_4,
  ss_claude_opus_5,
  ss_claude_opus_6,
  ss_claude_opus_7
) |>
  mutate(task = "task_stringsearch_claude_opus")
save(task_stringsearch_claude_opus, file = "output/task_stringsearch_claude_opus.RData")

# Perform String Search eval (Read codes only) ---------------------------------

# Objects and files carry the `_read` suffix, but the `task` column keeps the
# name of the full-browser run so both arms share facet labels in eval.R

# Gemini Flash Lite
gemini_flash_lite <- chat_google_gemini(model = "gemini-3.5-flash-lite", system_prompt = system_prompt_stringsearch)
ss_read_gemini_flash_lite_1 <- task_stringsearch_read(codelists_read[1, ]) # Atopic eczema
ss_read_gemini_flash_lite_1$eval(solver_chat = gemini_flash_lite)
ss_read_gemini_flash_lite_2 <- task_stringsearch_read(codelists_read[2, ]) # Vascular dementia
ss_read_gemini_flash_lite_2$eval(solver_chat = gemini_flash_lite)
ss_read_gemini_flash_lite_3 <- task_stringsearch_read(codelists_read[3, ]) # Eosinophilic esophagitis
ss_read_gemini_flash_lite_3$eval(solver_chat = gemini_flash_lite)
ss_read_gemini_flash_lite_4 <- task_stringsearch_read(codelists_read[4, ]) # Psoriasis
ss_read_gemini_flash_lite_4$eval(solver_chat = gemini_flash_lite)
ss_read_gemini_flash_lite_5 <- task_stringsearch_read(codelists_read[5, ]) # Hidradenitis suppurativa
ss_read_gemini_flash_lite_5$eval(solver_chat = gemini_flash_lite)
ss_read_gemini_flash_lite_6 <- task_stringsearch_read(codelists_read[6, ]) # Myocardial infarction
ss_read_gemini_flash_lite_6$eval(solver_chat = gemini_flash_lite)
ss_read_gemini_flash_lite_7 <- task_stringsearch_read(codelists_read[7, ]) # Wrist fracture
ss_read_gemini_flash_lite_7$eval(solver_chat = gemini_flash_lite)
task_stringsearch_gemini_flash_lite_read <- vitals_bind(
  ss_read_gemini_flash_lite_1,
  ss_read_gemini_flash_lite_2,
  ss_read_gemini_flash_lite_3,
  ss_read_gemini_flash_lite_4,
  ss_read_gemini_flash_lite_5,
  ss_read_gemini_flash_lite_6,
  ss_read_gemini_flash_lite_7
) |>
  mutate(task = "task_stringsearch_gemini_flash_lite")
save(task_stringsearch_gemini_flash_lite_read, file = "output/task_stringsearch_gemini_flash_lite_read.RData")

# Gemini Flash
gemini_flash <- chat_google_gemini(model = "gemini-3.6-flash", system_prompt = system_prompt_stringsearch)
ss_read_gemini_flash_1 <- task_stringsearch_read(codelists_read[1, ]) # Atopic eczema
ss_read_gemini_flash_1$eval(solver_chat = gemini_flash)
ss_read_gemini_flash_2 <- task_stringsearch_read(codelists_read[2, ]) # Vascular dementia
ss_read_gemini_flash_2$eval(solver_chat = gemini_flash)
ss_read_gemini_flash_3 <- task_stringsearch_read(codelists_read[3, ]) # Eosinophilic esophagitis
ss_read_gemini_flash_3$eval(solver_chat = gemini_flash)
ss_read_gemini_flash_4 <- task_stringsearch_read(codelists_read[4, ]) # Psoriasis
ss_read_gemini_flash_4$eval(solver_chat = gemini_flash)
ss_read_gemini_flash_5 <- task_stringsearch_read(codelists_read[5, ]) # Hidradenitis suppurativa
ss_read_gemini_flash_5$eval(solver_chat = gemini_flash)
ss_read_gemini_flash_6 <- task_stringsearch_read(codelists_read[6, ]) # Myocardial infarction
ss_read_gemini_flash_6$eval(solver_chat = gemini_flash)
ss_read_gemini_flash_7 <- task_stringsearch_read(codelists_read[7, ]) # Wrist fracture
ss_read_gemini_flash_7$eval(solver_chat = gemini_flash)
task_stringsearch_gemini_flash_read <- vitals_bind(
  ss_read_gemini_flash_1,
  ss_read_gemini_flash_2,
  ss_read_gemini_flash_3,
  ss_read_gemini_flash_4,
  ss_read_gemini_flash_5,
  ss_read_gemini_flash_6,
  ss_read_gemini_flash_7
) |>
  mutate(task = "task_stringsearch_gemini_flash")
save(task_stringsearch_gemini_flash_read, file = "output/task_stringsearch_gemini_flash_read.RData")

# Gemini Pro
gemini_pro <- chat_google_gemini(model = "gemini-3.1-pro-preview", system_prompt = system_prompt_stringsearch)
ss_read_gemini_pro_1 <- task_stringsearch_read(codelists_read[1, ]) # Atopic eczema
ss_read_gemini_pro_1$eval(solver_chat = gemini_pro)
ss_read_gemini_pro_2 <- task_stringsearch_read(codelists_read[2, ]) # Vascular dementia
ss_read_gemini_pro_2$eval(solver_chat = gemini_pro)
ss_read_gemini_pro_3 <- task_stringsearch_read(codelists_read[3, ]) # Eosinophilic esophagitis
ss_read_gemini_pro_3$eval(solver_chat = gemini_pro)
ss_read_gemini_pro_4 <- task_stringsearch_read(codelists_read[4, ]) # Psoriasis
ss_read_gemini_pro_4$eval(solver_chat = gemini_pro)
ss_read_gemini_pro_5 <- task_stringsearch_read(codelists_read[5, ]) # Hidradenitis suppurativa
ss_read_gemini_pro_5$eval(solver_chat = gemini_pro)
ss_read_gemini_pro_6 <- task_stringsearch_read(codelists_read[6, ]) # Myocardial infarction
ss_read_gemini_pro_6$eval(solver_chat = gemini_pro)
ss_read_gemini_pro_7 <- task_stringsearch_read(codelists_read[7, ]) # Wrist fracture
ss_read_gemini_pro_7$eval(solver_chat = gemini_pro)
task_stringsearch_gemini_pro_read <- vitals_bind(
  ss_read_gemini_pro_1,
  ss_read_gemini_pro_2,
  ss_read_gemini_pro_3,
  ss_read_gemini_pro_4,
  ss_read_gemini_pro_5,
  ss_read_gemini_pro_6,
  ss_read_gemini_pro_7
) |>
  mutate(task = "task_stringsearch_gemini_pro")
save(task_stringsearch_gemini_pro_read, file = "output/task_stringsearch_gemini_pro_read.RData")

# GPT Luna
gpt_luna <- chat_openai(model = "gpt-6-luna", system_prompt = system_prompt_stringsearch)
ss_read_gpt_luna_1 <- task_stringsearch_read(codelists_read[1, ]) # Atopic eczema
ss_read_gpt_luna_1$eval(solver_chat = gpt_luna)
ss_read_gpt_luna_2 <- task_stringsearch_read(codelists_read[2, ]) # Vascular dementia
ss_read_gpt_luna_2$eval(solver_chat = gpt_luna)
ss_read_gpt_luna_3 <- task_stringsearch_read(codelists_read[3, ]) # Eosinophilic esophagitis
ss_read_gpt_luna_3$eval(solver_chat = gpt_luna)
ss_read_gpt_luna_4 <- task_stringsearch_read(codelists_read[4, ]) # Psoriasis
ss_read_gpt_luna_4$eval(solver_chat = gpt_luna)
ss_read_gpt_luna_5 <- task_stringsearch_read(codelists_read[5, ]) # Hidradenitis suppurativa
ss_read_gpt_luna_5$eval(solver_chat = gpt_luna)
ss_read_gpt_luna_6 <- task_stringsearch_read(codelists_read[6, ]) # Myocardial infarction
ss_read_gpt_luna_6$eval(solver_chat = gpt_luna)
ss_read_gpt_luna_7 <- task_stringsearch_read(codelists_read[7, ]) # Wrist fracture
ss_read_gpt_luna_7$eval(solver_chat = gpt_luna)
task_stringsearch_gpt_luna_read <- vitals_bind(
  ss_read_gpt_luna_1,
  ss_read_gpt_luna_2,
  ss_read_gpt_luna_3,
  ss_read_gpt_luna_4,
  ss_read_gpt_luna_5,
  ss_read_gpt_luna_6,
  ss_read_gpt_luna_7
) |>
  mutate(task = "task_stringsearch_gpt_luna")
save(task_stringsearch_gpt_luna_read, file = "output/task_stringsearch_gpt_luna_read.RData")

# GPT Sol
gpt_sol <- chat_openai(model = "gpt-6.1-sol", system_prompt = system_prompt_stringsearch)
ss_read_gpt_sol_1 <- task_stringsearch_read(codelists_read[1, ]) # Atopic eczema
ss_read_gpt_sol_1$eval(solver_chat = gpt_sol)
ss_read_gpt_sol_2 <- task_stringsearch_read(codelists_read[2, ]) # Vascular dementia
ss_read_gpt_sol_2$eval(solver_chat = gpt_sol)
ss_read_gpt_sol_3 <- task_stringsearch_read(codelists_read[3, ]) # Eosinophilic esophagitis
ss_read_gpt_sol_3$eval(solver_chat = gpt_sol)
ss_read_gpt_sol_4 <- task_stringsearch_read(codelists_read[4, ]) # Psoriasis
ss_read_gpt_sol_4$eval(solver_chat = gpt_sol)
ss_read_gpt_sol_5 <- task_stringsearch_read(codelists_read[5, ]) # Hidradenitis suppurativa
ss_read_gpt_sol_5$eval(solver_chat = gpt_sol)
ss_read_gpt_sol_6 <- task_stringsearch_read(codelists_read[6, ]) # Myocardial infarction
ss_read_gpt_sol_6$eval(solver_chat = gpt_sol)
ss_read_gpt_sol_7 <- task_stringsearch_read(codelists_read[7, ]) # Wrist fracture
ss_read_gpt_sol_7$eval(solver_chat = gpt_sol)
task_stringsearch_gpt_sol_read <- vitals_bind(
  ss_read_gpt_sol_1,
  ss_read_gpt_sol_2,
  ss_read_gpt_sol_3,
  ss_read_gpt_sol_4,
  ss_read_gpt_sol_5,
  ss_read_gpt_sol_6,
  ss_read_gpt_sol_7
) |>
  mutate(task = "task_stringsearch_gpt_sol")
save(task_stringsearch_gpt_sol_read, file = "output/task_stringsearch_gpt_sol_read.RData")

# Claude Sonnet
claude_sonnet <- chat_claude(model = "claude-sonnet-5-5", system_prompt = system_prompt_stringsearch, params = claude_params)
ss_read_claude_sonnet_1 <- task_stringsearch_read(codelists_read[1, ]) # Atopic eczema
ss_read_claude_sonnet_1$eval(solver_chat = claude_sonnet)
ss_read_claude_sonnet_2 <- task_stringsearch_read(codelists_read[2, ]) # Vascular dementia
ss_read_claude_sonnet_2$eval(solver_chat = claude_sonnet)
ss_read_claude_sonnet_3 <- task_stringsearch_read(codelists_read[3, ]) # Eosinophilic esophagitis
ss_read_claude_sonnet_3$eval(solver_chat = claude_sonnet)
ss_read_claude_sonnet_4 <- task_stringsearch_read(codelists_read[4, ]) # Psoriasis
ss_read_claude_sonnet_4$eval(solver_chat = claude_sonnet)
ss_read_claude_sonnet_5 <- task_stringsearch_read(codelists_read[5, ]) # Hidradenitis suppurativa
ss_read_claude_sonnet_5$eval(solver_chat = claude_sonnet)
ss_read_claude_sonnet_6 <- task_stringsearch_read(codelists_read[6, ]) # Myocardial infarction
ss_read_claude_sonnet_6$eval(solver_chat = claude_sonnet)
ss_read_claude_sonnet_7 <- task_stringsearch_read(codelists_read[7, ]) # Wrist fracture
ss_read_claude_sonnet_7$eval(solver_chat = claude_sonnet)
task_stringsearch_claude_sonnet_read <- vitals_bind(
  ss_read_claude_sonnet_1,
  ss_read_claude_sonnet_2,
  ss_read_claude_sonnet_3,
  ss_read_claude_sonnet_4,
  ss_read_claude_sonnet_5,
  ss_read_claude_sonnet_6,
  ss_read_claude_sonnet_7
) |>
  mutate(task = "task_stringsearch_claude_sonnet")
save(task_stringsearch_claude_sonnet_read, file = "output/task_stringsearch_claude_sonnet_read.RData")

# Claude Opus
claude_opus <- chat_claude(model = "claude-opus-5-5", system_prompt = system_prompt_stringsearch, params = claude_params)
ss_read_claude_opus_1 <- task_stringsearch_read(codelists_read[1, ]) # Atopic eczema
ss_read_claude_opus_1$eval(solver_chat = claude_opus)
ss_read_claude_opus_2 <- task_stringsearch_read(codelists_read[2, ]) # Vascular dementia
ss_read_claude_opus_2$eval(solver_chat = claude_opus)
ss_read_claude_opus_3 <- task_stringsearch_read(codelists_read[3, ]) # Eosinophilic esophagitis
ss_read_claude_opus_3$eval(solver_chat = claude_opus)
ss_read_claude_opus_4 <- task_stringsearch_read(codelists_read[4, ]) # Psoriasis
ss_read_claude_opus_4$eval(solver_chat = claude_opus)
ss_read_claude_opus_5 <- task_stringsearch_read(codelists_read[5, ]) # Hidradenitis suppurativa
ss_read_claude_opus_5$eval(solver_chat = claude_opus)
ss_read_claude_opus_6 <- task_stringsearch_read(codelists_read[6, ]) # Myocardial infarction
ss_read_claude_opus_6$eval(solver_chat = claude_opus)
ss_read_claude_opus_7 <- task_stringsearch_read(codelists_read[7, ]) # Wrist fracture
ss_read_claude_opus_7$eval(solver_chat = claude_opus)
task_stringsearch_claude_opus_read <- vitals_bind(
  ss_read_claude_opus_1,
  ss_read_claude_opus_2,
  ss_read_claude_opus_3,
  ss_read_claude_opus_4,
  ss_read_claude_opus_5,
  ss_read_claude_opus_6,
  ss_read_claude_opus_7
) |>
  mutate(task = "task_stringsearch_claude_opus")
save(task_stringsearch_claude_opus_read, file = "output/task_stringsearch_claude_opus_read.RData")


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
    "Hidradenitis suppurativa",
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
