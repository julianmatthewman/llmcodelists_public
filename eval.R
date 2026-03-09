# Setup ---------------------------------------------------

library(vitals)
library(ellmer)
library(dplyr)
library(ggplot2)
library(ragnar)
library(tidyverse)
library(janitor)
library(gt)
library(patchwork)
load("output/codelists.RData")
# Get CPRD browser
# Replace the path below with the local path to your CPRD data file
base_file <- "path/to/CPRDAurumMedical_2025_09.dta"
browser <- haven::read_dta(base_file) |>
  mutate(code_term = paste(originalreadcode, term))

# Load RAG eval data -----------------------------------------------------

## Full CPRD Browser
load("output/task_rag_gemini.RData")
load("output/task_rag_openai.RData")
load("output/task_rag_claude.RData")
load("output/task_rag_gemini_flash.RData")
task_eval <- bind_rows(task_rag_gemini, task_rag_openai, task_rag_claude, task_rag_gemini_flash)

## Read codes only
load("output/task_rag_gemini_read.RData")
load("output/task_rag_openai_read.RData")
load("output/task_rag_claude_read.RData")
load("output/task_rag_gemini_flash_read.RData")
task_eval_read <- bind_rows(task_rag_gemini_read, task_rag_openai_read, task_rag_claude_read, task_rag_gemini_flash_read)

## MedCPT
load("output/task_rag_medcpt_query_gemini.RData")

# Load Stringsearch eval data --------------------------------------------

## Full CPRD Browser
load("output/task_stringsearch_openai.RData")
load("output/task_stringsearch_claude.RData")
load("output/task_stringsearch_gemini_flash.RData")
task_eval_stringsearch <- bind_rows(task_stringsearch_openai, task_stringsearch_claude, task_stringsearch_gemini_flash)

## Read codes only
load("output/task_stringsearch_openai_read.RData")
load("output/task_stringsearch_claude_read.RData")
load("output/task_stringsearch_gemini_flash_read.RData")
task_eval_stringsearch_read <- bind_rows(task_stringsearch_openai_read, task_stringsearch_claude_read, task_stringsearch_gemini_flash_read)


# Load Claude Code eval data ---------------------------------------------
load("output/task_claude_code.RData")

# Make Plotting Function ---------------------------------------------------

make_eval_plot <- function(x) {
  x |>
    left_join(codelists) |>
    mutate(
      input = factor(
        input,
        levels = c(
          "Wrist fracture",
          "Myocardial infarction",
          "Psoriasis",
          "Vascular dementia",
          "Atopic eczema",
          "Hidradentitis suppurativa",
          "Eosinophilic esophagitis"
        )
      )
    ) |>
    mutate(
      task = factor(
        case_when(
          task == "task_rag_gemini" ~ "3 Pro",
          task == "task_rag_gemini_flash" ~ "3 Flash",
          task == "task_rag_openai" ~ "GPT 5.2",
          task == "task_rag_claude" ~ "Sonnet 4.6",
          task == "task_stringsearch_gemini" ~ "3 Pro",
          task == "task_stringsearch_gemini_flash" ~ "3 Flash",
          task == "task_stringsearch_openai" ~ "GPT 5.2",
          task == "task_stringsearch_claude" ~ "Sonnet 4.6",
          task == "task_claude_code" ~ "Claude Code"
        )
      ),
      score = factor(
        case_when(
          score == "I" ~ "Incorrect",
          score == "P" ~ "Partially correct",
          score == "C" ~ "Correct"
        ),
        # align ordering with original factor
        levels = c("Incorrect", "Partially correct", "Correct"),
        ordered = TRUE
      )
    ) |>
    ggplot(aes(x = epoch, y = input)) +
    geom_tile(aes(fill = score), alpha = 2 / 3, colour = "white") +
    facet_wrap(~task, ncol = 4) +
    scale_fill_manual(
      values = c("Incorrect" = "#E41A1C", "Partially correct" = "#377EB8", "Correct" = "#4DAF4A"),
      drop = FALSE
    ) +
    labs(x = "Epoch", y = NULL) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      plot.title = element_text(hjust = 0.5)
    )
}

# Plots (RAG) ---------------------------------------------------

plot_read <- make_eval_plot(task_eval_read) +
  labs(title = "Read codes only")
plot_full <- make_eval_plot(task_eval) +
  labs(title = "Full CPRD Medical Browser")

plot_merged <- plot_full +
  plot_spacer() +
  plot_read +
  plot_layout(widths = c(10, 1, 10), guides = "collect", axes = "collect_y", axis_titles = "collect") &
  theme(legend.position = "bottom", legend.margin = margin(t = 0), plot.margin = margin(2, 2, 2, 2), legend.key.size = unit(0.5, "cm"))
plot_merged
ggsave("output/plot_merged.png", plot_merged, width = 183, height = 70, units = "mm", dpi = 300)
ggsave("output/plot_merged.svg", plot_merged, width = 183, height = 70, units = "mm")


# Plots (stringsearch) ---------------------------------------------------

plot_stringsearch_read <- make_eval_plot(task_eval_stringsearch_read) +
  labs(title = "Read codes only")
plot_stringsearch_full <- make_eval_plot(task_eval_stringsearch) +
  labs(title = "Full CPRD Medical Browser")

plot_stringsearch_merged <- plot_stringsearch_full +
  plot_spacer() +
  plot_stringsearch_read +
  plot_layout(widths = c(10, 1, 10), guides = "collect", axes = "collect_y", axis_titles = "collect") &
  theme(legend.position = "bottom", legend.margin = margin(t = 0), plot.margin = margin(2, 2, 2, 2), legend.key.size = unit(0.5, "cm"))
plot_stringsearch_merged
ggsave("output/plot_stringsearch_merged.png", plot_stringsearch_merged, width = 150, height = 70, units = "mm", dpi = 300)
ggsave("output/plot_stringsearch_merged.svg", plot_stringsearch_merged, width = 150, height = 70, units = "mm")

# Plots (Claude Code) ---------------------------------------------------

plot_claude_code <- make_eval_plot(task_claude_code) +
  labs(title = "Full CPRD Medical Browser") +
  theme(
    legend.margin = margin(t = 0, l = -108),
    legend.key.size = unit(0.3, "cm"),
    legend.text = element_text(size = 7),
    legend.position = "bottom",
    legend.justification = "left",
    plot.title = element_text(hjust = 1)
  )
plot_claude_code
ggsave("output/plot_claude_code.png", plot_claude_code, width = 65, height = 80, units = "mm", dpi = 300)
ggsave("output/plot_claude_code.svg", plot_claude_code, width = 65, height = 80, units = "mm")

# Plots (MedCPT RAG) ---------------------------------------------------

plot_rag_medcpt <- make_eval_plot(task_rag_medcpt_query_gemini) +
  labs(title = "Full CPRD Medical Browser") +
  theme(
    legend.margin = margin(t = 0, l = -108),
    legend.key.size = unit(0.3, "cm"),
    legend.text = element_text(size = 7),
    legend.position = "bottom",
    legend.justification = "left",
    plot.title = element_text(hjust = 1)
  )
plot_rag_medcpt
ggsave("output/plot_rag_medcpt.png", plot_rag_medcpt, width = 65, height = 80, units = "mm", dpi = 300)
ggsave("output/plot_rag_medcpt.svg", plot_rag_medcpt, width = 65, height = 80, units = "mm")


# Summarise mistakes with LLM (Claude Code) ------------------------------------
gradings_cc <- task_claude_code |>
  mutate(
    codelist = input,
    score_num = case_when(score == "I" ~ 0, score == "P" ~ 1, score == "C" ~ 2),
    missing_required_str = sapply(missing_required, \(x) {
      if (nrow(x) == 0) "None" else paste(x$OriginalReadCode, x$Term, sep = ": ", collapse = ", ")
    }),
    irrelevant_str = sapply(irrelevant, \(x) if (nrow(x) == 0) "None" else paste(x$originalreadcode, x$term, sep = ": ", collapse = ", ")),
    grading = paste0(
      "Score: ",
      score,
      "\n",
      "Missing required codes: ",
      missing_required_str,
      "\n",
      "Irrelevant codes included: ",
      irrelevant_str
    )
  ) |>
  group_by(codelist) |>
  summarise(grading = paste("## Epoch", epoch, "\n", grading, collapse = "\n\n\n"), score_num = sum(score_num))

# Summary of missed codes
chat <- chat_google_gemini(
  model = "gemini-3.1-pro-preview",
  system_prompt = paste(
    "You are given submissions for an evaluation of Claude Code's performance in creating clinical codelists against a Gold standard.",
    "Each submission contains the evaluation result for one codelist.",
    "For each submission summarise which required codes were missed. Focus only on the missed codes; do not mention any irrelevant codes that were included.",
    "Examples can be given for similar missing codes. Not every code needs to be listed.",
    "Be terse but always mention codes and terms together and format both codes and terms in bold. Don't include the date or 'preview' in the model version names."
  )
)
type_summary <- type_object(summary = type_string())
summaries_cc <- parallel_chat_structured(chat, as.list(gradings_cc$grading), type_summary)

gt_cc_missed <- gradings_cc |>
  select(-grading) |>
  bind_cols(summaries_cc) |>
  gt() |>
  data_color(
    columns = score_num,
    target_columns = codelist,
    palette = "RdYlGn"
  ) |>
  cols_hide(score_num) |>
  fmt_markdown(columns = summary)
gt_cc_missed
gtsave(gt_cc_missed, "output/gt_cc_missed.html")

# Summary of irrelevant codes
chat <- chat_google_gemini(
  model = "gemini-3.1-pro-preview",
  system_prompt = paste(
    "You are given submissions for an evaluation of Claude Code's performance in creating clinical codelists against a Gold standard.",
    "Each submission contains the evaluation result for one codelist.",
    "For each submission summarise which irrelevant codes were included. Irrelevant codes are codes that are neither required nor optional. Focus only on the irrelevant codes that were included; do not mention relevant codes that were missed.",
    "Examples can be given for similar missing codes. Not every code needs to be listed.",
    "Try and stick to 200 words max, be terse but always mention codes and terms together and format both codes and terms in bold. Don't include the date or 'preview' in the model version names."
  )
)
type_summary <- type_object(summary = type_string())
summaries2_cc <- parallel_chat_structured(chat, as.list(gradings_cc$grading), type_summary)

gt_cc_irrelevant <- gradings_cc |>
  select(-grading) |>
  bind_cols(summaries2_cc) |>
  gt() |>
  data_color(
    columns = score_num,
    target_columns = codelist,
    palette = "RdYlGn"
  ) |>
  cols_hide(score_num) |>
  fmt_markdown(columns = summary)
gt_cc_irrelevant
gtsave(gt_cc_irrelevant, "output/gt_cc_irrelevant.html")


# Summarise mistakes with LLM (RAG) --------------------------------------------
gradings <- task_eval |>
  rowwise() |>
  mutate(
    codelist = metadata$input,
    temp_resp = metadata$scorer_metadata[[1]]$response,
    model = metadata$solver_chat[[1]]$get_model(),
    score_num = case_when(score == "I" ~ 0, score == "P" ~ 1, score == "C" ~ 2)
  ) |>
  group_by(codelist, model) |>
  summarise(grading = paste("## Epoch", epoch, "\n", temp_resp, collapse = "\n\n\n"), score_num = sum(score_num)) |>
  summarise(grading = paste("# Model", model, "\n", grading, collapse = "\n\n\n"), score_num = sum(score_num))

# Summary of missed codes
chat <- chat_google_gemini(
  model = "gemini-flash-latest",
  system_prompt = paste(
    "You are given submissions for an evaluation of LLM's performance in creating clinical codelists against a Gold standard.",
    "Each submission contains the evaluation result for one codelists using different LLMs and were run several times (epochs).",
    "For each submission summarise which required codes were missed. Focus only on the missed codes; do not mention any irrelevant codes that were included. Highlight differences between models.",
    "Examples can be given for similar codes. Not every code needs to be listed.",
    "Be terse but always report codes and terms together with the code first followed by the term seperated by just one space. Format only the terms and not the codes in bold. Don't include the date in the model version names."
  )
)
type_summary <- type_object(summary = type_string())
summaries <- parallel_chat_structured(chat, as.list(gradings$grading), type_summary)

gt_rag_missed <- gradings |>
  select(-grading) |>
  bind_cols(summaries) |>
  gt() |>
  data_color(
    columns = score_num,
    target_columns = codelist,
    palette = "RdYlGn"
  ) |>
  cols_hide(score_num) |>
  fmt_markdown(columns = summary)
gt_rag_missed
gtsave(gt_rag_missed, "output/gt_rag_missed.html")

# Summary of irrelevant codes
chat <- chat_google_gemini(
  model = "gemini-flash-latest",
  system_prompt = paste(
    "You are given submissions for an evaluation of LLM's performance in creating clinical codelists against a Gold standard.",
    "Each submission contains the evaluation result for one codelists using different LLMs and were run several times (epochs).",
    "For each submission summarise which irrelevant codes were included. Focus only on the irrelevant codes that were included; do not mention relevant codes that were missed. Highlight differences between models.",
    "Examples can be given for similar codes. Not every code needs to be listed.",
    "Be terse but always report codes and terms together with the code first followed by the term seperated by just one space. Format only the terms and not the codes in bold. Don't include the date in the model version names."
  )
)
type_summary <- type_object(summary = type_string())
summaries2 <- parallel_chat_structured(chat, as.list(gradings$grading), type_summary)

gt_rag_irrelevant <- gradings |>
  select(-grading) |>
  bind_cols(summaries2) |>
  gt() |>
  data_color(
    columns = score_num,
    target_columns = codelist,
    palette = "RdYlGn"
  ) |>
  cols_hide(score_num) |>
  fmt_markdown(columns = summary)
gt_rag_irrelevant
gtsave(gt_rag_irrelevant, "output/gt_rag_irrelevant.html")


# Summarise mistakes with LLM (stringsearch) --------------------------------------------
gradings <- task_eval_stringsearch |>
  rowwise() |>
  mutate(
    codelist = metadata$input,
    temp_resp = metadata$scorer_metadata[[1]]$response,
    model = metadata$solver_chat[[1]]$get_model(),
    score_num = case_when(score == "I" ~ 0, score == "P" ~ 1, score == "C" ~ 2)
  ) |>
  group_by(codelist, model) |>
  summarise(grading = paste("## Epoch", epoch, "\n", temp_resp, collapse = "\n\n\n"), score_num = sum(score_num)) |>
  summarise(grading = paste("# Model", model, "\n", grading, collapse = "\n\n\n"), score_num = sum(score_num))

# Summary of missed codes
chat <- chat_google_gemini(
  model = "gemini-flash-latest",
  system_prompt = paste(
    "You are given submissions for an evaluation of LLM's performance in creating clinical codelists against a Gold standard.",
    "Each submission contains the evaluation result for one codelists using different LLMs and were run several times (epochs).",
    "For each submission summarise which required codes were missed. Focus only on the missed codes; do not mention any irrelevant codes that were included. Highlight differences between models.",
    "Examples can be given for similar codes. Not every code needs to be listed.",
    "Be terse but always report codes and terms together with the code first followed by the term seperated by just one space. Format only the terms and not the codes in bold. Don't include the date in the model version names."
  )
)
type_summary <- type_object(summary = type_string())
summaries <- parallel_chat_structured(chat, as.list(gradings$grading), type_summary)

gt_stringsearch_missed <- gradings |>
  select(-grading) |>
  bind_cols(summaries) |>
  gt() |>
  data_color(
    columns = score_num,
    target_columns = codelist,
    palette = "RdYlGn"
  ) |>
  cols_hide(score_num) |>
  fmt_markdown(columns = summary)
gt_stringsearch_missed
gtsave(gt_stringsearch_missed, "output/gt_stringsearch_missed.html")

# Summary of irrelevant codes
chat <- chat_google_gemini(
  model = "gemini-flash-latest",
  system_prompt = paste(
    "You are given submissions for an evaluation of LLM's performance in creating clinical codelists against a Gold standard.",
    "Each submission contains the evaluation result for one codelists using different LLMs and were run several times (epochs).",
    "For each submission summarise which irrelevant codes were included. Focus only on the irrelevant codes that were included; do not mention relevant codes that were missed. Highlight differences between models.",
    "Examples can be given for similar codes. Not every code needs to be listed.",
    "Be terse but always report codes and terms together with the code first followed by the term seperated by just one space. Format only the terms and not the codes in bold. Don't include the date in the model version names."
  )
)
type_summary <- type_object(summary = type_string())
summaries2 <- parallel_chat_structured(chat, as.list(gradings$grading), type_summary)

gt_stringsearch_irrelevant <- gradings |>
  select(-grading) |>
  bind_cols(summaries2) |>
  gt() |>
  data_color(
    columns = score_num,
    target_columns = codelist,
    palette = "RdYlGn"
  ) |>
  cols_hide(score_num) |>
  fmt_markdown(columns = summary)
gt_stringsearch_irrelevant
gtsave(gt_stringsearch_irrelevant, "output/gt_stringsearch_irrelevant.html")


# Check for hallucinations (markdown parser) -----------------------------------------------

parse_md_tables <- function(text) {
  lines <- strsplit(text, "\n")[[1]]
  table_lines <- grep("^\\|", lines)
  if (length(table_lines) == 0) {
    return(list())
  }
  groups <- split(table_lines, cumsum(c(1, diff(table_lines) != 1)))
  tables <- lapply(groups, function(idx) {
    tbl_lines <- lines[idx]
    sep <- grepl("^[|\\s:-]+$", tbl_lines, perl = TRUE)
    tbl_lines <- tbl_lines[!sep]
    parsed <- lapply(tbl_lines, function(l) {
      cells <- strsplit(trimws(l), "\\s*\\|\\s*")[[1]]
      cells <- cells[cells != ""]
      trimws(cells)
    })
    if (length(parsed) < 2) {
      return(NULL)
    }
    header <- parsed[[1]]
    rows <- parsed[-1]
    df <- do.call(
      rbind,
      lapply(rows, function(r) {
        length(r) <- length(header)
        r
      })
    )
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    names(df) <- gsub("\\*\\*(.+?)\\*\\*", "\\1", header)
    df[] <- lapply(df, function(col) gsub("\\*\\*(.+?)\\*\\*", "\\1", col))
    df
  })
  tables <- Filter(Negate(is.null), tables)
  if (length(tables) == 0) {
    return(NULL)
  }
  tables[[1]]
}

gradings <- task_eval |>
  rowwise() |>
  mutate(
    codelist = metadata$input,
    result = metadata$result,
    model = metadata$solver_chat[[1]]$get_model(),
    score_num = case_when(score == "I" ~ 0, score == "P" ~ 1, score == "C" ~ 2),
    tables = list(parse_md_tables(result)),
    invalid_codes = {
      tbl <- parse_md_tables(result)
      if (is.null(tbl)) {
        list(character(0))
      } else {
        code_col <- grep("code", names(tbl), ignore.case = TRUE, value = TRUE)[1]
        if (is.na(code_col)) {
          list(character(0))
        } else {
          list(tbl[[code_col]][!tbl[[code_col]] %in% browser$originalreadcode])
        }
      }
    }
  )

gradings |>
  ungroup() |>
  mutate(
    tables = map_chr(tables, \(tbl) {
      if (is.null(tbl)) {
        return("")
      }
      knitr::kable(tbl, format = "html")
    }),
    invalid_codes = map_chr(invalid_codes, \(x) paste(x, collapse = ", "))
  ) |>
  select(codelist, model, result, tables, invalid_codes) |>
  group_by(codelist, model) |>
  gt() |>
  fmt(columns = tables, fns = \(x) map(x, html))

gt_hallucinations_markdown <- gradings |>
  select(codelist, model, invalid_codes) |>
  group_by(model) |>
  gt()
gtsave(gt_hallucinations_markdown, "output/gt_hallucinations_markdown.html")

gt_hallucinations_markdown_summary <- gradings |>
  ungroup() |>
  mutate(
    n_total = map_int(tables, \(tbl) if (!is.data.frame(tbl)) 0L else nrow(tbl)),
    n_invalid = lengths(invalid_codes),
    pct_hallucinated = ifelse(n_total == 0, 0, n_invalid / n_total)
  ) |>
  select(codelist, model, epoch, score, n_total, n_invalid, pct_hallucinated) |>
  group_by(codelist, model) |>
  gt() |>
  fmt_percent(columns = pct_hallucinated, decimals = 0) |>
  cols_label(
    epoch = "Epoch",
    score = "Score",
    n_total = "Total codes",
    n_invalid = "Hallucinated",
    pct_hallucinated = "% hallucinated"
  )
gt_hallucinations_markdown_summary
gtsave(gt_hallucinations_markdown_summary, "output/gt_hallucinations_markdown_summary.html")

gt_hallucinations_markdown_by_model <- gradings |>
  ungroup() |>
  mutate(
    n_invalid = lengths(invalid_codes),
    has_hallucination = n_invalid > 0
  ) |>
  group_by(model) |>
  summarise(
    n_epochs = n(),
    n_with_hallucinations = sum(has_hallucination),
    pct_with_hallucinations = n_with_hallucinations / n_epochs
  ) |>
  gt() |>
  fmt_percent(columns = pct_with_hallucinations, decimals = 0) |>
  cols_label(
    model = "Model",
    n_epochs = "Epochs",
    n_with_hallucinations = "With hallucinations",
    pct_with_hallucinations = "%"
  )
gt_hallucinations_markdown_by_model
gtsave(gt_hallucinations_markdown_by_model, "output/gt_hallucinations_markdown_by_model.html")


# Check for hallucinations (structured data) --------------------------

type_codelist <- type_array(
  items = type_object(
    code = type_string("The medical code"),
    term = type_string("The term/description for the code")
  )
)

chat <- chat_claude(
  model = "claude-sonnet-4-6",
  system_prompt = paste(
    "Extract all medical codes and their terms from the codelist table in the text.",
    "Return every row from the table. Do not add or modify any codes or terms."
  )
)

gradings_structured <- task_eval |>
  rowwise() |>
  mutate(
    codelist = metadata$input,
    result = metadata$result,
    model = metadata$solver_chat[[1]]$get_model(),
    score_num = case_when(score == "I" ~ 0, score == "P" ~ 1, score == "C" ~ 2)
  ) |>
  ungroup()

# parsed_tables <- parallel_chat_structured(
#   chat,
#   as.list(gradings_structured$result),
#   type_codelist
# )
# save(parsed_tables, file = "output/parsed_tables_hallucinations.RData")
load("output/parsed_tables_hallucinations.RData")

gradings_structured <- gradings_structured |>
  mutate(
    parsed = parsed_tables,
    invalid_codes = map(parsed, \(tbl) {
      if (nrow(tbl) == 0) {
        return(character(0))
      }
      code_term <- paste(tbl$code, tbl$term)
      code_term[!code_term %in% browser$code_term]
    })
  )

gradings_structured |>
  mutate(
    tables = map_chr(parsed, \(tbl) {
      if (nrow(tbl) == 0) {
        return("")
      }
      knitr::kable(tbl, format = "html")
    }),
    invalid_codes = map_chr(invalid_codes, \(x) paste(x, collapse = ", "))
  ) |>
  select(codelist, model, result, tables, invalid_codes) |>
  group_by(codelist, model) |>
  gt() |>
  fmt(columns = tables, fns = \(x) map(x, html))

gt_hallucinations_structured <- gradings_structured |>
  select(codelist, model, invalid_codes) |>
  group_by(model) |>
  gt()
gt_hallucinations_structured
gtsave(gt_hallucinations_structured, "output/gt_hallucinations_structured.html")

gt_hallucinations_structured_summary <- gradings_structured |>
  mutate(
    n_total = map_int(parsed, nrow),
    n_invalid = lengths(invalid_codes),
    pct_hallucinated = ifelse(n_total == 0, 0, n_invalid / n_total)
  ) |>
  select(codelist, model, epoch, score, n_total, n_invalid, pct_hallucinated) |>
  group_by(codelist, model) |>
  gt() |>
  fmt_percent(columns = pct_hallucinated, decimals = 0) |>
  cols_label(
    epoch = "Epoch",
    score = "Score",
    n_total = "Total codes",
    n_invalid = "Hallucinated",
    pct_hallucinated = "% hallucinated"
  )
gt_hallucinations_structured_summary
gtsave(gt_hallucinations_structured_summary, "output/gt_hallucinations_structured_summary.html")

gt_hallucinations_structured_by_model <- gradings_structured |>
  mutate(
    n_invalid = lengths(invalid_codes),
    has_hallucination = n_invalid > 0
  ) |>
  group_by(model) |>
  summarise(
    n_epochs = n(),
    n_with_hallucinations = sum(has_hallucination),
    pct_with_hallucinations = n_with_hallucinations / n_epochs
  ) |>
  gt() |>
  fmt_percent(columns = pct_with_hallucinations, decimals = 0) |>
  cols_label(
    model = "Model",
    n_epochs = "Epochs",
    n_with_hallucinations = "With hallucinations",
    pct_with_hallucinations = "%"
  )
gt_hallucinations_structured_by_model
gtsave(gt_hallucinations_structured_by_model, "output/gt_hallucinations_structured_by_model.html")


# Check for retrieved codes ----------------------------------------------

# Helper: extract all text from a Chat object (including tool results)
extract_chat_text <- function(chat) {
  turns <- chat$get_turns()
  all_text <- character()
  for (turn in turns) {
    # Text from ContentText elements
    if (length(turn@text) > 0 && nzchar(turn@text)) {
      all_text <- c(all_text, turn@text)
    }
    # Tool result values (may be string, data frame, or list)
    for (content in turn@contents) {
      if (inherits(content, "ContentToolResult")) {
        val <- content@value
        txt <- tryCatch(
          {
            if (is.character(val)) {
              paste(val, collapse = "\n")
            } else if (is.data.frame(val)) {
              paste(apply(val, 1, paste, collapse = " "), collapse = "\n")
            } else if (is.list(val)) {
              paste(unlist(val), collapse = "\n")
            } else {
              paste(format(val), collapse = "\n")
            }
          },
          error = function(e) ""
        )
        all_text <- c(all_text, txt)
      }
    }
  }
  paste(all_text, collapse = "\n")
}

# Helper: parse target string to extract required codes
# Target format per line: "CODE TERM REQUIRED_FLAG"
parse_required_codes <- function(target) {
  lines <- strsplit(target, "\n")[[1]]
  code <- sub("^(\\S+) .*", "\\1", lines)
  required <- as.integer(sub(".* (\\d)$", "\\1", lines))
  code[required == 1]
}

# Check which required codes appear in the solver_chat context
check_retrieved <- function(task_data) {
  task_data |>
    rowwise() |>
    mutate(
      codelist = metadata$input,
      model = metadata$solver_chat[[1]]$get_model(),
      required_codes = list(parse_required_codes(metadata$target)),
      chat_text = extract_chat_text(metadata$solver_chat[[1]]),
      retrieved = list(required_codes[vapply(required_codes, \(code) grepl(code, chat_text, fixed = TRUE), logical(1))]),
      not_retrieved = list(setdiff(required_codes, retrieved)),
      result_codes = {
        tbl <- parse_md_tables(metadata$result)
        if (is.null(tbl)) {
          list(character(0))
        } else {
          code_col <- grep("code", names(tbl), ignore.case = TRUE, value = TRUE)[1]
          if (is.na(code_col)) list(character(0)) else list(tbl[[code_col]])
        }
      },
      retrieved_not_used = list(setdiff(retrieved, result_codes)),
      n_required = length(required_codes),
      n_retrieved = length(retrieved),
      n_not_retrieved = length(not_retrieved),
      n_retrieved_not_used = length(retrieved_not_used),
      pct_retrieved = n_retrieved / n_required
    ) |>
    ungroup()
}

## RAG eval
retrieved_rag <- check_retrieved(task_eval)

format_retrieved_summary <- function(gt_tbl) {
  gt_tbl |>
    fmt_percent(columns = pct_retrieved, decimals = 0) |>
    cols_label(
      epoch = "Epoch",
      score = "Score",
      n_required = "Required",
      n_retrieved = "Retrieved",
      n_not_retrieved = "Not retrieved",
      n_retrieved_not_used = "Unused",
      pct_retrieved = "% retrieved"
    )
}

format_retrieved_detail <- function(gt_tbl) {
  gt_tbl |>
    cols_label(
      epoch = "Epoch",
      score = "Score",
      not_retrieved_str = "Not retrieved",
      retrieved_not_used_str = "Retrieved but unused"
    )
}

gt_rag_retrieved_summary <- retrieved_rag |>
  select(codelist, model, epoch, score, n_required, n_retrieved, n_not_retrieved, n_retrieved_not_used, pct_retrieved) |>
  group_by(codelist, model) |>
  gt() |>
  format_retrieved_summary()
gt_rag_retrieved_summary
gtsave(gt_rag_retrieved_summary, "output/gt_rag_retrieved_summary.html")

gt_rag_retrieved_detail <- retrieved_rag |>
  mutate(
    not_retrieved_str = map_chr(not_retrieved, \(x) paste(x, collapse = ", ")),
    retrieved_not_used_str = map_chr(retrieved_not_used, \(x) paste(x, collapse = ", "))
  ) |>
  select(codelist, model, epoch, score, not_retrieved_str, retrieved_not_used_str) |>
  filter(not_retrieved_str != "" | retrieved_not_used_str != "") |>
  group_by(codelist, model) |>
  gt() |>
  format_retrieved_detail()
gt_rag_retrieved_detail
gtsave(gt_rag_retrieved_detail, "output/gt_rag_retrieved_detail.html")

## Stringsearch eval
retrieved_stringsearch <- check_retrieved(task_eval_stringsearch)

gt_stringsearch_retrieved_summary <- retrieved_stringsearch |>
  select(codelist, model, epoch, score, n_required, n_retrieved, n_not_retrieved, n_retrieved_not_used, pct_retrieved) |>
  group_by(codelist, model) |>
  gt() |>
  format_retrieved_summary()
gt_stringsearch_retrieved_summary
gtsave(gt_stringsearch_retrieved_summary, "output/gt_stringsearch_retrieved_summary.html")

gt_stringsearch_retrieved_detail <- retrieved_stringsearch |>
  mutate(
    not_retrieved_str = map_chr(not_retrieved, \(x) paste(x, collapse = ", ")),
    retrieved_not_used_str = map_chr(retrieved_not_used, \(x) paste(x, collapse = ", "))
  ) |>
  select(codelist, model, epoch, score, not_retrieved_str, retrieved_not_used_str) |>
  filter(not_retrieved_str != "" | retrieved_not_used_str != "") |>
  group_by(codelist, model) |>
  gt() |>
  format_retrieved_detail()
gt_stringsearch_retrieved_detail
gtsave(gt_stringsearch_retrieved_detail, "output/gt_stringsearch_retrieved_detail.html")

# Check for retrieved codes (structured data) ------------------------------

check_retrieved_structured <- function(task_data, parsed_tables = NULL) {
  prepped <- task_data |>
    rowwise() |>
    mutate(
      codelist = metadata$input,
      result = metadata$result,
      model = metadata$solver_chat[[1]]$get_model(),
      required_codes = list(parse_required_codes(metadata$target)),
      chat_text = extract_chat_text(metadata$solver_chat[[1]]),
      retrieved = list(required_codes[vapply(required_codes, \(code) grepl(code, chat_text, fixed = TRUE), logical(1))]),
      not_retrieved = list(setdiff(required_codes, retrieved))
    ) |>
    ungroup()

  if (is.null(parsed_tables)) {
    chat <- chat_claude(
      model = "claude-sonnet-4-6",
      system_prompt = paste(
        "Extract all medical codes and their terms from the codelist table in the text.",
        "Return every row from the table. Do not add or modify any codes or terms."
      )
    )

    parsed_tables <- parallel_chat_structured(
      chat,
      as.list(prepped$result),
      type_codelist
    )
  }

  prepped |>
    mutate(
      parsed = parsed_tables,
      result_codes = map(parsed, \(tbl) {
        if (nrow(tbl) == 0) character(0) else tbl$code
      }),
      retrieved_not_used = map2(retrieved, result_codes, \(r, rc) setdiff(r, rc)),
      n_required = lengths(required_codes),
      n_retrieved = lengths(retrieved),
      n_not_retrieved = lengths(not_retrieved),
      n_retrieved_not_used = lengths(retrieved_not_used),
      pct_retrieved = n_retrieved / n_required
    )
}

## RAG eval
retrieved_rag_structured <- check_retrieved_structured(task_eval, parsed_tables = parsed_tables)

gt_rag_retrieved_structured_summary <- retrieved_rag_structured |>
  select(codelist, model, epoch, score, n_required, n_retrieved, n_not_retrieved, n_retrieved_not_used, pct_retrieved) |>
  group_by(codelist, model) |>
  gt() |>
  format_retrieved_summary()
gt_rag_retrieved_structured_summary
gtsave(gt_rag_retrieved_structured_summary, "output/gt_rag_retrieved_structured_summary.html")

gt_rag_retrieved_structured_detail <- retrieved_rag_structured |>
  mutate(
    not_retrieved_str = map_chr(not_retrieved, \(x) paste(x, collapse = ", ")),
    retrieved_not_used_str = map_chr(retrieved_not_used, \(x) paste(x, collapse = ", "))
  ) |>
  select(codelist, model, epoch, score, not_retrieved_str, retrieved_not_used_str) |>
  filter(not_retrieved_str != "" | retrieved_not_used_str != "") |>
  group_by(codelist, model) |>
  gt() |>
  format_retrieved_detail()
gt_rag_retrieved_structured_detail
gtsave(gt_rag_retrieved_structured_detail, "output/gt_rag_retrieved_structured_detail.html")

## Stringsearch eval
retrieved_stringsearch_structured <- check_retrieved_structured(task_eval_stringsearch)

gt_stringsearch_retrieved_structured_summary <- retrieved_stringsearch_structured |>
  select(codelist, model, epoch, score, n_required, n_retrieved, n_not_retrieved, n_retrieved_not_used, pct_retrieved) |>
  group_by(codelist, model) |>
  gt() |>
  format_retrieved_summary()
gt_stringsearch_retrieved_structured_summary
gtsave(gt_stringsearch_retrieved_structured_summary, "output/gt_stringsearch_retrieved_structured_summary.html")

gt_stringsearch_retrieved_structured_detail <- retrieved_stringsearch_structured |>
  mutate(
    not_retrieved_str = map_chr(not_retrieved, \(x) paste(x, collapse = ", ")),
    retrieved_not_used_str = map_chr(retrieved_not_used, \(x) paste(x, collapse = ", "))
  ) |>
  select(codelist, model, epoch, score, not_retrieved_str, retrieved_not_used_str) |>
  filter(not_retrieved_str != "" | retrieved_not_used_str != "") |>
  group_by(codelist, model) |>
  gt() |>
  format_retrieved_detail()
gt_stringsearch_retrieved_structured_detail
gtsave(gt_stringsearch_retrieved_structured_detail, "output/gt_stringsearch_retrieved_structured_detail.html")

## Failure mode summary (incorrect epochs only)
summarise_failure_modes <- function(data) {
  data |>
    filter(score == "I") |>
    mutate(
      retrieval_failure = n_not_retrieved > 0,
      usage_failure = n_retrieved_not_used > 0
    ) |>
    group_by(model) |>
    summarise(
      n_incorrect = n(),
      n_retrieval_failure = sum(retrieval_failure),
      n_usage_failure = sum(usage_failure),
      pct_retrieval_failure = n_retrieval_failure / n_incorrect,
      pct_usage_failure = n_usage_failure / n_incorrect
    )
}

format_failure_modes <- function(data) {
  data |>
    gt() |>
    fmt_percent(columns = starts_with("pct_"), decimals = 0) |>
    cols_label(
      model = "Model",
      n_incorrect = "Incorrect epochs",
      n_retrieval_failure = "N",
      pct_retrieval_failure = "%",
      n_usage_failure = "N",
      pct_usage_failure = "%"
    ) |>
    tab_spanner(label = "Not retrieved", columns = c(n_retrieval_failure, pct_retrieval_failure)) |>
    tab_spanner(label = "Retrieved but unused", columns = c(n_usage_failure, pct_usage_failure))
}

gt_failure_modes_rag <- summarise_failure_modes(retrieved_rag_structured) |>
  format_failure_modes()
gt_failure_modes_rag
gtsave(gt_failure_modes_rag, "output/gt_failure_modes_rag.html")

gt_failure_modes_stringsearch <- summarise_failure_modes(retrieved_stringsearch_structured) |>
  format_failure_modes()
gt_failure_modes_stringsearch
gtsave(gt_failure_modes_stringsearch, "output/gt_failure_modes_stringsearch.html")


# Merged hallucinations + retrieved summary (RAG) ------------------------

hallucination_cols <- gradings_structured |>
  mutate(
    n_total = map_int(parsed, nrow),
    n_invalid = lengths(invalid_codes),
    pct_hallucinated = ifelse(n_total == 0, 0, n_invalid / n_total)
  ) |>
  select(codelist, model, epoch, n_total, n_invalid, pct_hallucinated)

retrieved_cols <- retrieved_rag_structured |>
  select(codelist, model, epoch, score, n_required, n_retrieved, n_not_retrieved, n_retrieved_not_used, pct_retrieved)

gt_rag_merged <- retrieved_cols |>
  left_join(hallucination_cols, by = c("codelist", "model", "epoch")) |>
  group_by(model, codelist) |>
  gt() |>
  fmt_percent(columns = c(pct_retrieved, pct_hallucinated), decimals = 0) |>
  cols_label(
    epoch = "Epoch",
    score = "Score",
    n_required = "Required",
    n_retrieved = "Retrieved",
    n_not_retrieved = "Not retrieved",
    n_retrieved_not_used = "Unused",
    pct_retrieved = "%",
    n_total = "Total",
    n_invalid = "Hallucinated",
    pct_hallucinated = "%"
  ) |>
  cols_hide(pct_retrieved) |>
  tab_spanner(label = "Retrieval", columns = c(n_required, n_retrieved, n_not_retrieved, n_retrieved_not_used, pct_retrieved)) |>
  tab_spanner(label = "Hallucinations", columns = c(n_total, n_invalid, pct_hallucinated)) |>
  tab_options(data_row.padding = px(1), row_group.padding = px(2))
gt_rag_merged
gtsave(gt_rag_merged, "output/gt_rag_merge.html")
