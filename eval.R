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
# base_file <- "..."
base_file <- "..."
browser <- haven::read_dta(base_file)

# Load RAG eval data -----------------------------------------------------

## Full CPRD Browser
load("output/task_rag_gemini_flash_lite.RData")
load("output/task_rag_gemini_flash.RData")
load("output/task_rag_gemini_pro.RData")
load("output/task_rag_gpt_luna.RData")
load("output/task_rag_gpt_sol.RData")
load("output/task_rag_claude_sonnet.RData")
load("output/task_rag_claude_opus.RData")
task_eval <- bind_rows(task_rag_gemini_flash_lite, task_rag_gemini_flash, task_rag_gemini_pro, task_rag_gpt_luna, task_rag_gpt_sol, task_rag_claude_sonnet, task_rag_claude_opus)

## Read codes only
load("output/task_rag_gemini_flash_lite_read.RData")
load("output/task_rag_gemini_flash_read.RData")
load("output/task_rag_gemini_pro_read.RData")
load("output/task_rag_gpt_luna_read.RData")
load("output/task_rag_gpt_sol_read.RData")
load("output/task_rag_claude_sonnet_read.RData")
load("output/task_rag_claude_opus_read.RData")
task_eval_read <- bind_rows(task_rag_gemini_flash_lite_read, task_rag_gemini_flash_read, task_rag_gemini_pro_read, task_rag_gpt_luna_read, task_rag_gpt_sol_read, task_rag_claude_sonnet_read, task_rag_claude_opus_read)

## MedCPT
load("output/task_rag_medcpt_query_gemini.RData")

# Load Stringsearch eval data --------------------------------------------

## Full CPRD Browser
load("output/task_stringsearch_gemini_flash_lite.RData")
load("output/task_stringsearch_gemini_flash.RData")
load("output/task_stringsearch_gemini_pro.RData")
load("output/task_stringsearch_gpt_luna.RData")
load("output/task_stringsearch_gpt_sol.RData")
load("output/task_stringsearch_claude_sonnet.RData")
load("output/task_stringsearch_claude_opus.RData")
task_eval_stringsearch <- bind_rows(task_stringsearch_gemini_flash_lite, task_stringsearch_gemini_flash, task_stringsearch_gemini_pro, task_stringsearch_gpt_luna, task_stringsearch_gpt_sol, task_stringsearch_claude_sonnet, task_stringsearch_claude_opus)

## Read codes only
load("output/task_stringsearch_gemini_flash_lite_read.RData")
load("output/task_stringsearch_gemini_flash_read.RData")
load("output/task_stringsearch_gemini_pro_read.RData")
load("output/task_stringsearch_gpt_luna_read.RData")
load("output/task_stringsearch_gpt_sol_read.RData")
load("output/task_stringsearch_claude_sonnet_read.RData")
load("output/task_stringsearch_claude_opus_read.RData")
task_eval_stringsearch_read <- bind_rows(task_stringsearch_gemini_flash_lite_read, task_stringsearch_gemini_flash_read, task_stringsearch_gemini_pro_read, task_stringsearch_gpt_luna_read, task_stringsearch_gpt_sol_read, task_stringsearch_claude_sonnet_read, task_stringsearch_claude_opus_read)


# Load Claude Code eval data ---------------------------------------------
load("output/task_claude_code.RData")

# Make Plotting Function ---------------------------------------------------

# Facet labels come from the model each run actually used (recorded in
# metadata$solver_chat), so a task name that was reused across model versions
# (e.g. task_rag_gemini) can't mislabel the plot.
model_labels <- c(
  "gemini-3-flash-lite-preview" = "3 Flash Lite",
  "gemini-3.5-flash-lite" = "3.5 Flash Lite",
  "gemini-3-flash-preview" = "3 Flash",
  "gemini-3.6-flash" = "3.6 Flash",
  "gemini-3-pro-preview" = "3 Pro",
  "gemini-3.1-pro-preview" = "3.1 Pro",
  "gpt-5.6-luna" = "GPT 5.6 Luna",
  "gpt-6-luna" = "GPT 6 Luna",
  "gpt-6.1-sol" = "GPT 6.1 Sol",
  "claude-sonnet-5" = "Sonnet 5",
  "claude-sonnet-5-5" = "Sonnet 5.5",
  "claude-opus-5-5" = "Opus 5.5"
)

# Flash Lite, then Flash, then Pro; unlabelled models keep their raw id
model_levels <- c(unname(model_labels), "Claude Code")

# Runs without solver metadata (Claude Code) fall back to the task name
task_labels <- c("task_claude_code" = "Claude Code")

get_model_label <- function(x) {
  models <- if ("metadata" %in% names(x)) {
    map_chr(x$metadata, \(m) {
      chat <- pluck(m, "solver_chat", 1)
      if (is.null(chat)) NA_character_ else chat$get_model()
    })
  } else {
    rep(NA_character_, nrow(x))
  }
  labels <- unname(model_labels[models])
  labels <- coalesce(labels, models, unname(task_labels[x$task]), x$task)
  factor(labels, levels = union(model_levels, unique(labels)))
}

score_colours <- c("Incorrect" = "#E41A1C", "Partially correct" = "#377EB8", "Correct" = "#4DAF4A")

# Legend key: the usual tile with its score letter on top, matching the cells
draw_key_score <- function(data, params, size) {
  letter <- substr(names(score_colours)[match(data$fill, score_colours)], 1, 1)
  grid::grobTree(
    draw_key_polygon(data, params, size),
    grid::textGrob(letter, gp = grid::gpar(col = "white", fontface = "bold", fontsize = 2 * .pt))
  )
}

make_eval_plot <- function(x) {
  x |>
    mutate(task = get_model_label(x)) |>
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
          "Hidradenitis suppurativa",
          "Eosinophilic esophagitis"
        )
      )
    ) |>
    mutate(
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
    geom_tile(aes(fill = score), alpha = 2 / 3, colour = "white", key_glyph = draw_key_score) +
    # Letter in each cell so the plot doesn't rely on colour alone
    geom_text(aes(label = substr(score, 1, 1)), size = 2, colour = "white", fontface = "bold", na.rm = TRUE, show.legend = FALSE) +
    facet_wrap(~task, nrow = 1) +
    scale_fill_manual(values = score_colours, drop = FALSE) +
    # UK spelling on the axis; the evaluation inputs keep their original name
    scale_y_discrete(labels = c("Eosinophilic esophagitis" = "Eosinophilic oesophagitis")) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      # Title sits above the y-axis labels in the model-name row: the margins
      # cancel its 6.6pt height (zero-height row) and centre it on the 17.3pt strip
      plot.title.position = "plot",
      plot.title = element_text(size = 7, face = "bold", hjust = 0, margin = margin(t = 5.3, b = -11.9))
    )
}

# Plots (RAG) ---------------------------------------------------

plot_read <- make_eval_plot(task_eval_read) +
  labs(title = "Read codes only")
plot_full <- make_eval_plot(task_eval) +
  labs(title = "Full CPRD Medical Browser")

# One row of models per arm, stacked. The read row is padded on the right so
# its tiles stay the same width as the full row's while it has fewer models.
n_full <- n_distinct(get_model_label(task_eval))
n_read <- n_distinct(get_model_label(task_eval_read))

plot_merged <- plot_full +
  plot_read +
  plot_spacer() +
  plot_layout(design = "AA\nBC", widths = c(n_read, max(n_full - n_read, 0)), guides = "collect") &
  theme(legend.position = "bottom", legend.margin = margin(t = 0), plot.margin = margin(2, 2, 2, 2), legend.key.size = unit(0.5, "cm"))
plot_merged
ggsave("output/plot_merged.png", plot_merged, width = 183, height = 108, units = "mm", dpi = 300)
ggsave("output/plot_merged.svg", plot_merged, width = 183, height = 108, units = "mm")


# Plots (stringsearch) ---------------------------------------------------

plot_stringsearch_read <- make_eval_plot(task_eval_stringsearch_read) +
  labs(title = "Read codes only")
plot_stringsearch_full <- make_eval_plot(task_eval_stringsearch) +
  labs(title = "Full CPRD Medical Browser")

# Same stacked layout as the RAG plot
n_stringsearch_full <- n_distinct(get_model_label(task_eval_stringsearch))
n_stringsearch_read <- n_distinct(get_model_label(task_eval_stringsearch_read))

plot_stringsearch_merged <- plot_stringsearch_full +
  plot_stringsearch_read +
  plot_spacer() +
  plot_layout(design = "AA\nBC", widths = c(n_stringsearch_read, max(n_stringsearch_full - n_stringsearch_read, 0)), guides = "collect") &
  theme(legend.position = "bottom", legend.margin = margin(t = 0), plot.margin = margin(2, 2, 2, 2), legend.key.size = unit(0.5, "cm"))
plot_stringsearch_merged
ggsave("output/plot_stringsearch_merged.png", plot_stringsearch_merged, width = 183, height = 108, units = "mm", dpi = 300)
ggsave("output/plot_stringsearch_merged.svg", plot_stringsearch_merged, width = 183, height = 108, units = "mm")

# Plots (Claude Code) ---------------------------------------------------

plot_claude_code <- make_eval_plot(task_claude_code) +
  labs(title = "Full CPRD Medical Browser") +
  theme(
    legend.margin = margin(t = 0, l = -108),
    legend.key.size = unit(0.3, "cm"),
    legend.text = element_text(size = 7),
    legend.position = "bottom",
    legend.justification = "left"
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
    legend.justification = "left"
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
  summarise(grading = paste("## Run", epoch, "\n", grading, collapse = "\n\n\n"), score_num = sum(score_num))

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

gt_claudecode_llmsummary_missed_full <- gradings_cc |>
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
gt_claudecode_llmsummary_missed_full
gtsave(gt_claudecode_llmsummary_missed_full, "output/gt_claudecode_llmsummary_missed_full.html")
gtsave(gt_claudecode_llmsummary_missed_full, "output/gt_claudecode_llmsummary_missed_full.docx")

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

gt_claudecode_llmsummary_irrelevant_full <- gradings_cc |>
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
gt_claudecode_llmsummary_irrelevant_full
gtsave(gt_claudecode_llmsummary_irrelevant_full, "output/gt_claudecode_llmsummary_irrelevant_full.html")
gtsave(gt_claudecode_llmsummary_irrelevant_full, "output/gt_claudecode_llmsummary_irrelevant_full.docx")


# Format scorer metadata for the LLM summariser --------------------------------

# detect_codelist() records codes only, so terms are looked up in the CPRD
# browser; codes with no match were invented by the model.
code_term_lookup <- browser |>
  distinct(originalreadcode, .keep_all = TRUE) |>
  select(originalreadcode, term) |>
  mutate(across(everything(), as.character)) |>
  deframe()

label_codes <- function(codes) {
  if (length(codes) == 0) {
    return("None")
  }
  terms <- unname(code_term_lookup[codes])
  terms[is.na(terms)] <- "(code not in CPRD browser)"
  paste(codes, terms, collapse = ", ")
}

# Same shape as the Claude Code gradings above, so summaries are comparable
format_grading <- function(meta, score) {
  # Runs scored by model_graded_qa() stored the grader's free text instead
  if (!is.null(meta$response)) {
    return(meta$response)
  }
  paste0(
    "Score: ",
    score,
    "\n",
    "Missing required codes: ",
    label_codes(meta$missing_required),
    "\n",
    "Irrelevant codes included: ",
    label_codes(meta$irrelevant)
  )
}

# Summarise mistakes with LLM --------------------------------------------------

# One row per codelist, with every model and run folded into a single markdown
# document for the summariser
make_gradings <- function(task_data) {
  task_data |>
    rowwise() |>
    mutate(
      codelist = metadata$input,
      temp_resp = format_grading(metadata$scorer_metadata[[1]], score),
      model = metadata$solver_chat[[1]]$get_model(),
      score_num = case_when(score == "I" ~ 0, score == "P" ~ 1, score == "C" ~ 2)
    ) |>
    group_by(codelist, model) |>
    summarise(grading = paste("## Run", epoch, "\n", temp_resp, collapse = "\n\n\n"), score_num = sum(score_num)) |>
    summarise(grading = paste("# Model", model, "\n", grading, collapse = "\n\n\n"), score_num = sum(score_num))
}

# `focus` is the one instruction that differs between the two summaries
summariser_prompt <- function(focus) {
  paste(
    "You are given submissions for an evaluation of LLM's performance in creating clinical codelists against a Gold standard.",
    "Each submission contains the evaluation result for one codelists using different LLMs and were run several times; each run is a separate section.",
    focus,
    "Examples can be given for similar codes. Not every code needs to be listed.",
    "Be terse but always report codes and terms together with the code first followed by the term seperated by just one space. Format only the terms and not the codes in bold. Don't include the date in the model version names."
  )
}

focus_missed <- paste(
  "For each submission summarise which required codes were missed.",
  "Focus only on the missed codes; do not mention any irrelevant codes that were included.",
  "Highlight differences between models."
)

focus_irrelevant <- paste(
  "For each submission summarise which irrelevant codes were included.",
  "Focus only on the irrelevant codes that were included; do not mention relevant codes that were missed.",
  "Highlight differences between models."
)

# One LLM request per codelist, so each call costs nrow(gradings) requests
summarise_mistakes <- function(gradings, focus, model = "gemini-3.1-pro-preview") {
  chat <- chat_google_gemini(model = model, system_prompt = summariser_prompt(focus))
  summaries <- parallel_chat_structured(
    chat,
    as.list(gradings$grading),
    type_object(summary = type_string())
  )

  gradings |>
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
}

## RAG eval (full CPRD Browser)
gradings_rag <- make_gradings(task_eval)

gt_rag_llmsummary_missed_full <- summarise_mistakes(gradings_rag, focus_missed)
gt_rag_llmsummary_missed_full
gtsave(gt_rag_llmsummary_missed_full, "output/gt_rag_llmsummary_missed_full.html")
gtsave(gt_rag_llmsummary_missed_full, "output/gt_rag_llmsummary_missed_full.docx")

gt_rag_llmsummary_irrelevant_full <- summarise_mistakes(gradings_rag, focus_irrelevant)
gt_rag_llmsummary_irrelevant_full
gtsave(gt_rag_llmsummary_irrelevant_full, "output/gt_rag_llmsummary_irrelevant_full.html")
gtsave(gt_rag_llmsummary_irrelevant_full, "output/gt_rag_llmsummary_irrelevant_full.docx")

## RAG eval (Read codes only)
gradings_rag_read <- make_gradings(task_eval_read)

gt_rag_llmsummary_missed_read <- summarise_mistakes(gradings_rag_read, focus_missed)
gt_rag_llmsummary_missed_read
gtsave(gt_rag_llmsummary_missed_read, "output/gt_rag_llmsummary_missed_read.html")
gtsave(gt_rag_llmsummary_missed_read, "output/gt_rag_llmsummary_missed_read.docx")

gt_rag_llmsummary_irrelevant_read <- summarise_mistakes(gradings_rag_read, focus_irrelevant)
gt_rag_llmsummary_irrelevant_read
gtsave(gt_rag_llmsummary_irrelevant_read, "output/gt_rag_llmsummary_irrelevant_read.html")
gtsave(gt_rag_llmsummary_irrelevant_read, "output/gt_rag_llmsummary_irrelevant_read.docx")

## Stringsearch eval (full CPRD Browser)
gradings_stringsearch <- make_gradings(task_eval_stringsearch)

gt_stringsearch_llmsummary_missed_full <- summarise_mistakes(gradings_stringsearch, focus_missed)
gt_stringsearch_llmsummary_missed_full
gtsave(gt_stringsearch_llmsummary_missed_full, "output/gt_stringsearch_llmsummary_missed_full.html")
gtsave(gt_stringsearch_llmsummary_missed_full, "output/gt_stringsearch_llmsummary_missed_full.docx")

gt_stringsearch_llmsummary_irrelevant_full <- summarise_mistakes(gradings_stringsearch, focus_irrelevant)
gt_stringsearch_llmsummary_irrelevant_full
gtsave(gt_stringsearch_llmsummary_irrelevant_full, "output/gt_stringsearch_llmsummary_irrelevant_full.html")
gtsave(gt_stringsearch_llmsummary_irrelevant_full, "output/gt_stringsearch_llmsummary_irrelevant_full.docx")

## Stringsearch eval (Read codes only)
gradings_stringsearch_read <- make_gradings(task_eval_stringsearch_read)

gt_stringsearch_llmsummary_missed_read <- summarise_mistakes(gradings_stringsearch_read, focus_missed)
gt_stringsearch_llmsummary_missed_read
gtsave(gt_stringsearch_llmsummary_missed_read, "output/gt_stringsearch_llmsummary_missed_read.html")
gtsave(gt_stringsearch_llmsummary_missed_read, "output/gt_stringsearch_llmsummary_missed_read.docx")

gt_stringsearch_llmsummary_irrelevant_read <- summarise_mistakes(gradings_stringsearch_read, focus_irrelevant)
gt_stringsearch_llmsummary_irrelevant_read
gtsave(gt_stringsearch_llmsummary_irrelevant_read, "output/gt_stringsearch_llmsummary_irrelevant_read.html")
gtsave(gt_stringsearch_llmsummary_irrelevant_read, "output/gt_stringsearch_llmsummary_irrelevant_read.docx")


# Flatten scorer metadata ------------------------------------------------------

# detect_codelist() (vitals.R) extracts the final codelist as structured data
# during the solving step and records every code-level comparison against the
# target, so the checks below read that metadata instead of re-parsing the
# model's markdown or re-extracting it with a second LLM call.
#
# One row per sample: identifiers, the scorer's counts, and its code vectors.
# Samples scored by model_graded_qa() carry a grader transcript instead of these
# fields and are dropped, so evals predating the deterministic scorer need to be
# re-run before they show up in the tables below.

# How many tool calls the solver made, taken from the saved Chat. The turns keep
# every tool request the model issued (the structured-extraction follow-up turn
# in vitals.R adds no tool calls), so this counts searches of the RAG store or
# the string search tools. NA when the run saved no Chat (e.g. Claude Code).
count_tool_calls <- function(chat) {
  if (!inherits(chat, "Chat")) {
    return(NA_integer_)
  }
  sum(map_int(chat$get_turns(), \(turn) {
    # ellmer's content classes are S7, so inherits() alone would never match
    sum(map_lgl(turn@contents, \(content) S7::S7_inherits(content, ellmer::ContentToolRequest)))
  }))
}

# Time the model spent generating, summed over the Chat's assistant turns
# (ellmer times each request and keeps the duration on the turn). This is model
# time, not sample wall clock: it excludes the local tool calls between turns,
# i.e. the RAG store lookups and the string searches over the browser.
# The vitals logs record a per-sample `time`, but that is the duration of the
# last turn only, so the saved Chats are the better source.
chat_duration <- function(chat) {
  if (!inherits(chat, "Chat")) {
    return(NA_real_)
  }
  durations <- chat$get_turns() |>
    keep(\(turn) S7::S7_inherits(turn, ellmer::AssistantTurn)) |>
    map_dbl(\(turn) turn@duration)
  if (all(is.na(durations))) NA_real_ else sum(durations, na.rm = TRUE)
}

# Tokens the solver used, summed over the Chat's turns. `input` excludes cached
# input, so the three add up to the billed usage (this mirrors how ellmer prices
# a turn). Includes the structured-extraction follow-up turn, as does the
# duration above.
chat_token_use <- function(chat) {
  if (!inherits(chat, "Chat")) {
    return(c(input = NA_real_, output = NA_real_, cached_input = NA_real_))
  }
  tokens <- chat$get_tokens()
  c(
    input = sum(tokens$input, na.rm = TRUE),
    output = sum(tokens$output, na.rm = TRUE),
    cached_input = sum(tokens$cached_input, na.rm = TRUE)
  )
}

# USD per million tokens, from https://ai.google.dev/gemini-api/docs/pricing
# (retrieved 2026-08-13). Cost is recomputed from these rather than read from
# chat$get_cost(): ellmer stores a per-turn cost at request time, and it is NA
# for every run whose model its price table did not know then, which is all of
# them here except the pro arms.
#
# Gemini 3 Pro charges the higher `large` rate on prompts over 200k tokens, so
# pricing is per turn rather than per sample: a solver can cross the threshold
# partway through its tool loop. Models with flat pricing have a `small` row
# only. Google publishes no cached-input rate for these models; every Google
# model ellmer prices charges cached input at a tenth of input, so that is what
# is assumed here. It matters most for gemini-3.6-flash, where most of the
# context is cached.
#
# OpenAI prices are from the model pages under
# https://developers.openai.com/api/docs/models (retrieved 2026-10-06), at
# Standard rates. Every GPT model here moves to the `large` rate on prompts over
# 272k input tokens (2x input and cached input, 1.5x output). OpenAI also bills
# cache writes, but ellmer does not record them separately, so they are not
# counted.
#
# Claude prices are from https://platform.claude.com/docs/en/about-claude/pricing
# (retrieved 2026-10-06), at standard rates. These models have no long-context
# premium, so they have a `small` row only. Cached input is a cache hit, which
# is 0.05x input on Opus 5.5 and 0.1x on Sonnet 5.5.
# chat_claude() caches the prompt by default, and ellmer adds the cache-write
# tokens to `input`, so they are priced at the base input rate here rather than
# the 1.25x that is billed; Claude costs are slightly understated as a result.
#
# `large_above` is the prompt size, in tokens, beyond which `large` applies.
model_prices <- tribble(
  ~model, ~tier, ~price_input, ~price_output, ~price_cached, ~large_above,
  "gemini-3.1-pro-preview", "small", 2.00, 12.00, 0.200, 200e3,
  "gemini-3.1-pro-preview", "large", 4.00, 18.00, 0.400, 200e3,
  "gemini-3-pro-preview", "small", 2.00, 12.00, 0.200, 200e3,
  "gemini-3-pro-preview", "large", 4.00, 18.00, 0.400, 200e3,
  "gemini-3.6-flash", "small", 1.50, 7.50, 0.150, 200e3,
  "gemini-3.5-flash-lite", "small", 0.30, 2.50, 0.030, 200e3,
  "gemini-3-flash-preview", "small", 0.50, 3.00, 0.050, 200e3,
  "gpt-6-luna", "small", 0.10, 0.50, 0.010, 272e3,
  "gpt-6-luna", "large", 0.20, 0.75, 0.020, 272e3,
  "gpt-6.1-sol", "small", 2.00, 10.00, 0.100, 272e3,
  "gpt-6.1-sol", "large", 4.00, 15.00, 0.200, 272e3,
  "claude-sonnet-5-5", "small", 2.00, 10.00, 0.200, Inf,
  "claude-opus-5-5", "small", 4.00, 20.00, 0.200, Inf
)

# Sum of each turn's cost. The tier is set by the size of the prompt that turn
# actually sent, which is the fresh input plus whatever was served from cache.
# NA for models missing from the table above, rather than a wrong number.
chat_cost <- function(chat, prices = model_prices) {
  if (!inherits(chat, "Chat")) {
    return(NA_real_)
  }
  model <- chat$get_model()
  prices <- prices[prices$model == model, ]
  if (nrow(prices) == 0) {
    return(NA_real_)
  }

  tokens <- chat$get_tokens()
  tier <- ifelse(tokens$input + tokens$cached_input > prices$large_above[1], "large", "small")
  tier[!tier %in% prices$tier] <- "small"
  idx <- match(tier, prices$tier)

  sum(
    (tokens$input * prices$price_input[idx] +
      tokens$output * prices$price_output[idx] +
      tokens$cached_input * prices$price_cached[idx]) /
      1e6
  )
}

# Code-level recall, precision and F1 against the gold standard, on the same
# terms as the C/P/I score: optional codes are neither credited when included
# nor penalised when left out.
#   recall    = required codes returned / required codes
#   precision = returned codes in the target codelist (required or optional) /
#               returned codes
# Precision is NA when no codes were returned (failed extraction); F1 is then 0,
# since recall is 0, so failed samples still pull the F1 averages down.
add_code_metrics <- function(data) {
  data |>
    mutate(
      recall = ifelse(n_required == 0, NA_real_, (n_required - n_missing_required) / n_required),
      precision = ifelse(n_found == 0, NA_real_, (n_found - n_irrelevant) / n_found),
      f1 = ifelse(recall == 0, 0, 2 * precision * recall / (precision + recall))
    )
}

scorer_metadata_table <- function(task_data) {
  meta <- map(task_data$metadata, \(m) pluck(m, "scorer_metadata", 1))
  scored <- map_lgl(meta, \(x) is.list(x) && "n_found" %in% names(x))

  if (!all(scored)) {
    warning(
      sum(!scored),
      " of ",
      length(scored),
      " samples were not scored by detect_codelist() and are dropped; re-run the eval to include them.",
      call. = FALSE
    )
  }

  task_data <- task_data[scored, ]
  meta <- meta[scored]

  tokens <- map(task_data$metadata, \(m) chat_token_use(pluck(m, "solver_chat", 1)))

  tibble(
    codelist = map_chr(task_data$metadata, \(m) m$input),
    model = map_chr(task_data$metadata, \(m) m$solver_chat[[1]]$get_model()),
    run = task_data$epoch,
    score = task_data$score,
    score_num = case_when(task_data$score == "I" ~ 0, task_data$score == "P" ~ 1, task_data$score == "C" ~ 2),
    # The codelist the solver returned as structured data
    found = map(task_data$metadata, \(m) pluck(m, "solver_metadata", 1, "codes", .default = character(0))),
    n_found = map_int(meta, "n_found"),
    n_missing_required = map_int(meta, "n_missing_required"),
    n_missing_optional = map_int(meta, "n_missing_optional"),
    n_irrelevant = map_int(meta, "n_irrelevant"),
    n_nonexistent = map_int(meta, "n_nonexistent"),
    n_required = map_int(meta, "n_required"),
    n_retrieved = map_int(meta, "n_retrieved"),
    n_not_retrieved = map_int(meta, "n_not_retrieved"),
    n_retrieved_not_used = map_int(meta, "n_retrieved_not_used"),
    pct_retrieved = map_dbl(meta, "pct_retrieved"),
    n_tool_calls = map_int(task_data$metadata, \(m) count_tool_calls(pluck(m, "solver_chat", 1))),
    latency_s = map_dbl(task_data$metadata, \(m) chat_duration(pluck(m, "solver_chat", 1))),
    n_input_tokens = map_dbl(tokens, "input"),
    n_output_tokens = map_dbl(tokens, "output"),
    n_cached_tokens = map_dbl(tokens, "cached_input"),
    cost_usd = map_dbl(task_data$metadata, \(m) chat_cost(pluck(m, "solver_chat", 1))),
    missing_required = map(meta, "missing_required"),
    irrelevant = map(meta, "irrelevant"),
    nonexistent = map(meta, "nonexistent"),
    not_retrieved = map(meta, "not_retrieved"),
    retrieved_not_used = map(meta, "retrieved_not_used")
  ) |>
    add_code_metrics()
}

scored_rag <- scorer_metadata_table(task_eval)
scored_stringsearch <- scorer_metadata_table(task_eval_stringsearch)
# Read-only arms. Their scorer was given cprd_code_terms_read, which is exactly
# the set of codes the solver could see (the read-only store for RAG,
# cprd_data_read for the string search tools), so `nonexistent` counts every
# returned code that is not in that set — including codes that are real in the
# full browser but have no Read code.
scored_rag_read <- scorer_metadata_table(task_eval_read)
scored_stringsearch_read <- scorer_metadata_table(task_eval_stringsearch_read)
# Claude Code was compared to the gold standard outside vitals (vitals.R), by
# MedCodeId and counting CSV rows rather than unique codes
scored_claude_code <- task_claude_code |>
  transmute(
    codelist = input,
    model = "Claude Code",
    run = epoch,
    score,
    n_found = n_claude_codes,
    n_required = n_gold_required,
    n_missing_required,
    n_irrelevant
  ) |>
  add_code_metrics()


# Code-level recall, precision and F1 ------------------------------------------

# Definitions are with add_code_metrics() above. Per-model figures are means
# over samples (codelist x run), so each codelist counts equally regardless of
# its size. Precision is averaged over samples that returned any codes.

# Footnotes defining the metrics; `means_over`, the source note saying what the
# figures are averaged over, switches the wording to describe averaged figures.
# `any_empty` is whether any sample returned no codes (precision NA); the
# averaging caveat is only needed then.
footnote_code_metrics <- function(gt_tbl, means_over = NULL, any_empty = TRUE) {
  precision_note <- if (is.null(means_over)) {
    "Shown as a dash when no codes were returned."
  } else if (any_empty) {
    "Averaged over samples that returned any codes."
  }
  gt_tbl <- gt_tbl |>
    tab_footnote(
      "Required codes returned / required codes in the gold standard. Optional codes do not count towards recall.",
      locations = cells_column_labels(recall)
    ) |>
    tab_footnote(
      paste("Returned codes in the gold standard (required or optional) / codes returned.", precision_note),
      locations = cells_column_labels(precision)
    ) |>
    tab_footnote(
      "Harmonic mean of recall and precision: 2 × precision × recall / (precision + recall).",
      locations = cells_column_labels(f1)
    )
  if (!is.null(means_over)) {
    gt_tbl <- gt_tbl |>
      tab_source_note(means_over)
  }
  gt_tbl
}

# One row per run: how the run went, the codelist it returned against the gold
# standard (the counts behind recall and precision), the metrics, and the
# failure modes: invalid codes (a subset of the irrelevant ones), and required
# codes that never reached its context, or did but were left out of the
# codelist. Claude Code has no solver Chat or retrieval check, so those columns
# and their spanners are left out.
#
# Invalid is `nonexistent`: returned codes that are not in the source data the
# solver was given, which is the full browser (cprd_code_terms) for the full
# arm and the read-only store (cprd_code_terms_read) for the read arm, so a real
# code without a Read code counts as invalid there.
#
# With `mean_over_runs`, there is one row per codelist instead, grouped by
# model: Score lists each run's score in run order, and the other columns are
# means over the runs (precision over the runs that returned any codes).
code_metrics_spanners <- list(
  "Run" = c("run", "score", "n_tool_calls", "latency_s", "cost_usd"),
  "Codelist vs gold standard" = c("n_required", "n_missing_required", "n_found", "n_irrelevant"),
  "Metrics" = c("recall", "precision", "f1"),
  "Failure modes" = c("n_nonexistent", "n_not_retrieved", "n_retrieved_not_used")
)

make_code_metrics_summary <- function(data, mean_over_runs = FALSE) {
  data <- data |>
    select(codelist, model, any_of(unlist(code_metrics_spanners, use.names = FALSE))) |>
    # Models in the plots' order (model_labels), not alphabetical
    mutate(model = factor(model, levels = union(names(model_labels), unique(model))))
  # Checked before averaging, which drops the NA precisions
  any_empty <- any(is.na(data$precision))

  if (mean_over_runs) {
    data <- data |>
      arrange(model, codelist, run) |>
      group_by(model, codelist) |>
      summarise(
        score = paste(score, collapse = " "),
        across(where(is.numeric) & !run, \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      ) |>
      # mean() of only NAs is NaN; keep it NA so sub_missing() shows a dash
      mutate(across(where(is.double), \(x) if_else(is.nan(x), NA_real_, x))) |>
      select(codelist, model, any_of(unlist(code_metrics_spanners, use.names = FALSE)))
  }

  labels <- c(
    run = "#",
    score = "Score",
    n_tool_calls = "Tool calls",
    latency_s = "Latency (s)",
    cost_usd = "Cost",
    n_required = "Required",
    n_missing_required = "Missing required",
    n_found = "Returned",
    n_irrelevant = "Irrelevant",
    n_nonexistent = "Invalid",
    recall = "Recall",
    precision = "Precision",
    f1 = "F1",
    n_not_retrieved = "Not retrieved",
    n_retrieved_not_used = "Retrieved but unused"
  )

  if (mean_over_runs) {
    gt_tbl <- data |>
      gt(groupname_col = "model", rowname_col = "codelist") |>
      fmt_number(
        columns = any_of(c("n_tool_calls", "n_missing_required", "n_found", "n_irrelevant", "n_nonexistent", "n_not_retrieved", "n_retrieved_not_used")),
        decimals = 1
      ) |>
      fmt_number(columns = n_required, decimals = 0)
  } else {
    gt_tbl <- data |>
      group_by(codelist, model) |>
      gt()
  }

  gt_tbl <- gt_tbl |>
    fmt_number(columns = any_of("latency_s"), decimals = 0) |>
    fmt_currency(columns = any_of("cost_usd"), currency = "USD", decimals = 2) |>
    fmt_percent(columns = c(recall, precision), decimals = 0) |>
    fmt_number(columns = f1, decimals = 2) |>
    sub_missing(columns = c(recall, precision, f1)) |>
    cols_label(.list = as.list(labels[names(labels) %in% names(data)])) |>
    footnote_code_metrics(
      means_over = if (mean_over_runs) "Means over runs. Score lists each run's score in run order.",
      any_empty = any_empty
    )

  for (label in names(code_metrics_spanners)) {
    columns <- intersect(code_metrics_spanners[[label]], names(data))
    if (length(columns) > 0) {
      gt_tbl <- tab_spanner(gt_tbl, label = label, columns = all_of(columns))
    }
  }

  if ("n_nonexistent" %in% names(data)) {
    gt_tbl <- gt_tbl |>
      sub_missing(columns = n_nonexistent) |>
      tab_footnote(
        "Returned codes that do not exist in the source data.",
        locations = cells_column_labels(n_nonexistent)
      )
  }

  if ("n_not_retrieved" %in% names(data)) {
    gt_tbl <- gt_tbl |>
      tab_footnote(
        "Required codes that never appeared in the solver's context (its own turns and tool results).",
        locations = cells_column_labels(n_not_retrieved)
      ) |>
      tab_footnote(
        "Required codes that appeared in the solver's context but were left out of the returned codelist.",
        locations = cells_column_labels(n_retrieved_not_used)
      )
  }
  gt_tbl
}

make_code_metrics_by_model <- function(data) {
  any_empty <- any(is.na(data$precision))
  data |>
    # Models in the plots' order (model_labels), not alphabetical
    mutate(model = factor(model, levels = union(names(model_labels), unique(model)))) |>
    group_by(model) |>
    summarise(
      n_samples = n(),
      recall = mean(recall, na.rm = TRUE),
      precision = mean(precision, na.rm = TRUE),
      f1 = mean(f1, na.rm = TRUE)
    ) |>
    gt() |>
    fmt_percent(columns = c(recall, precision), decimals = 0) |>
    fmt_number(columns = f1, decimals = 2) |>
    cols_label(
      model = "Model",
      n_samples = "Samples",
      recall = "Mean recall",
      precision = "Mean precision",
      f1 = "Mean F1"
    ) |>
    footnote_code_metrics(
      means_over = "Means over samples (codelist × run), so each codelist counts equally regardless of its size.",
      any_empty = any_empty
    )
}

## RAG eval (full CPRD Browser)
gt_rag_code_metrics_full <- make_code_metrics_summary(scored_rag)
gt_rag_code_metrics_full
gtsave(gt_rag_code_metrics_full, "output/gt_rag_code_metrics_full.html")
gtsave(gt_rag_code_metrics_full, "output/gt_rag_code_metrics_full.docx")

gt_rag_code_metrics_by_model_full <- make_code_metrics_by_model(scored_rag)
gt_rag_code_metrics_by_model_full
gtsave(gt_rag_code_metrics_by_model_full, "output/gt_rag_code_metrics_by_model_full.html")
gtsave(gt_rag_code_metrics_by_model_full, "output/gt_rag_code_metrics_by_model_full.docx")

gt_rag_code_metrics_by_codelist_full <- make_code_metrics_summary(scored_rag, mean_over_runs = TRUE)
gt_rag_code_metrics_by_codelist_full
gtsave(gt_rag_code_metrics_by_codelist_full, "output/gt_rag_code_metrics_by_codelist_full.html")
gtsave(gt_rag_code_metrics_by_codelist_full, "output/gt_rag_code_metrics_by_codelist_full.docx")

## RAG eval (Read codes only)
gt_rag_code_metrics_read <- make_code_metrics_summary(scored_rag_read)
gt_rag_code_metrics_read
gtsave(gt_rag_code_metrics_read, "output/gt_rag_code_metrics_read.html")
gtsave(gt_rag_code_metrics_read, "output/gt_rag_code_metrics_read.docx")

gt_rag_code_metrics_by_model_read <- make_code_metrics_by_model(scored_rag_read)
gt_rag_code_metrics_by_model_read
gtsave(gt_rag_code_metrics_by_model_read, "output/gt_rag_code_metrics_by_model_read.html")
gtsave(gt_rag_code_metrics_by_model_read, "output/gt_rag_code_metrics_by_model_read.docx")

gt_rag_code_metrics_by_codelist_read <- make_code_metrics_summary(scored_rag_read, mean_over_runs = TRUE)
gt_rag_code_metrics_by_codelist_read
gtsave(gt_rag_code_metrics_by_codelist_read, "output/gt_rag_code_metrics_by_codelist_read.html")
gtsave(gt_rag_code_metrics_by_codelist_read, "output/gt_rag_code_metrics_by_codelist_read.docx")

## Stringsearch eval (full CPRD Browser)
gt_stringsearch_code_metrics_full <- make_code_metrics_summary(scored_stringsearch)
gt_stringsearch_code_metrics_full
gtsave(gt_stringsearch_code_metrics_full, "output/gt_stringsearch_code_metrics_full.html")
gtsave(gt_stringsearch_code_metrics_full, "output/gt_stringsearch_code_metrics_full.docx")

gt_stringsearch_code_metrics_by_model_full <- make_code_metrics_by_model(scored_stringsearch)
gt_stringsearch_code_metrics_by_model_full
gtsave(gt_stringsearch_code_metrics_by_model_full, "output/gt_stringsearch_code_metrics_by_model_full.html")
gtsave(gt_stringsearch_code_metrics_by_model_full, "output/gt_stringsearch_code_metrics_by_model_full.docx")

gt_stringsearch_code_metrics_by_codelist_full <- make_code_metrics_summary(scored_stringsearch, mean_over_runs = TRUE)
gt_stringsearch_code_metrics_by_codelist_full
gtsave(gt_stringsearch_code_metrics_by_codelist_full, "output/gt_stringsearch_code_metrics_by_codelist_full.html")
gtsave(gt_stringsearch_code_metrics_by_codelist_full, "output/gt_stringsearch_code_metrics_by_codelist_full.docx")

## Stringsearch eval (Read codes only)
gt_stringsearch_code_metrics_read <- make_code_metrics_summary(scored_stringsearch_read)
gt_stringsearch_code_metrics_read
gtsave(gt_stringsearch_code_metrics_read, "output/gt_stringsearch_code_metrics_read.html")
gtsave(gt_stringsearch_code_metrics_read, "output/gt_stringsearch_code_metrics_read.docx")

gt_stringsearch_code_metrics_by_model_read <- make_code_metrics_by_model(scored_stringsearch_read)
gt_stringsearch_code_metrics_by_model_read
gtsave(gt_stringsearch_code_metrics_by_model_read, "output/gt_stringsearch_code_metrics_by_model_read.html")
gtsave(gt_stringsearch_code_metrics_by_model_read, "output/gt_stringsearch_code_metrics_by_model_read.docx")

gt_stringsearch_code_metrics_by_codelist_read <- make_code_metrics_summary(scored_stringsearch_read, mean_over_runs = TRUE)
gt_stringsearch_code_metrics_by_codelist_read
gtsave(gt_stringsearch_code_metrics_by_codelist_read, "output/gt_stringsearch_code_metrics_by_codelist_read.html")
gtsave(gt_stringsearch_code_metrics_by_codelist_read, "output/gt_stringsearch_code_metrics_by_codelist_read.docx")

## Claude Code (full CPRD Browser)
gt_claude_code_code_metrics <- make_code_metrics_summary(scored_claude_code)
gt_claude_code_code_metrics
gtsave(gt_claude_code_code_metrics, "output/gt_claude_code_code_metrics.html")
gtsave(gt_claude_code_code_metrics, "output/gt_claude_code_code_metrics.docx")

gt_claude_code_code_metrics_by_model <- make_code_metrics_by_model(scored_claude_code)
gt_claude_code_code_metrics_by_model
gtsave(gt_claude_code_code_metrics_by_model, "output/gt_claude_code_code_metrics_by_model.html")
gtsave(gt_claude_code_code_metrics_by_model, "output/gt_claude_code_code_metrics_by_model.docx")

gt_claude_code_code_metrics_by_codelist <- make_code_metrics_summary(scored_claude_code, mean_over_runs = TRUE)
gt_claude_code_code_metrics_by_codelist
gtsave(gt_claude_code_code_metrics_by_codelist, "output/gt_claude_code_code_metrics_by_codelist.html")
gtsave(gt_claude_code_code_metrics_by_codelist, "output/gt_claude_code_code_metrics_by_codelist.docx")


# Check for retrieved codes ----------------------------------------------

# The scorer already compares each required code against the solver's context
# (turn text plus tool results) and against the returned codelist, splitting
# misses into codes that were never retrieved and codes that were retrieved but
# left out.

## Failure mode summary (incorrect runs only)
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
      n_incorrect = "Incorrect runs",
      n_retrieval_failure = "N",
      pct_retrieval_failure = "%",
      n_usage_failure = "N",
      pct_usage_failure = "%"
    ) |>
    tab_spanner(label = "Not retrieved", columns = c(n_retrieval_failure, pct_retrieval_failure)) |>
    tab_spanner(label = "Retrieved but unused", columns = c(n_usage_failure, pct_usage_failure))
}

gt_rag_failure_modes_full <- summarise_failure_modes(scored_rag) |>
  format_failure_modes()
gt_rag_failure_modes_full
gtsave(gt_rag_failure_modes_full, "output/gt_rag_failure_modes_full.html")
gtsave(gt_rag_failure_modes_full, "output/gt_rag_failure_modes_full.docx")

gt_rag_failure_modes_read <- summarise_failure_modes(scored_rag_read) |>
  format_failure_modes()
gt_rag_failure_modes_read
gtsave(gt_rag_failure_modes_read, "output/gt_rag_failure_modes_read.html")
gtsave(gt_rag_failure_modes_read, "output/gt_rag_failure_modes_read.docx")

gt_stringsearch_failure_modes_full <- summarise_failure_modes(scored_stringsearch) |>
  format_failure_modes()
gt_stringsearch_failure_modes_full
gtsave(gt_stringsearch_failure_modes_full, "output/gt_stringsearch_failure_modes_full.html")
gtsave(gt_stringsearch_failure_modes_full, "output/gt_stringsearch_failure_modes_full.docx")

gt_stringsearch_failure_modes_read <- summarise_failure_modes(scored_stringsearch_read) |>
  format_failure_modes()
gt_stringsearch_failure_modes_read
gtsave(gt_stringsearch_failure_modes_read, "output/gt_stringsearch_failure_modes_read.html")
gtsave(gt_stringsearch_failure_modes_read, "output/gt_stringsearch_failure_modes_read.docx")
