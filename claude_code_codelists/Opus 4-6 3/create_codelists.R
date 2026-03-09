# ==============================================================================
# Codelist Creation for CPRD Aurum Medical Dictionary
# ==============================================================================
# Following guidance from: Davé & Petersen (2021). Creating medical codelists
# to identify cases in primary care databases. NIHR Open Research, 4-20.
# https://openresearch.nihr.ac.uk/articles/4-20
#
# Conditions: Atopic dermatitis, Vascular dementia, Eosinophilic oesophagitis,
#             Psoriasis, Hidradenitis suppurativa, Myocardial infarction,
#             Wrist fracture
#
# Objective: Include all codes that, when recorded in a person's medical record,
#            would indicate that the person currently has the condition.
# ==============================================================================

library(haven)
library(dplyr)
library(stringr)

# --- Load CPRD Aurum Medical Dictionary ---
dict <- read_dta("CPRDAurumMedical_2025_09.dta")

cat("Dictionary loaded:", nrow(dict), "codes\n\n")

# ==============================================================================
# Helper function: search dictionary by regex patterns, with optional exclusions
# ==============================================================================
search_dict <- function(dict, include_patterns, exclude_patterns = NULL,
                        category_exclude = NULL) {
  # Combine include patterns into one regex (case-insensitive)
  include_regex <- paste(include_patterns, collapse = "|")
  matches <- dict[grepl(include_regex, dict$term, ignore.case = TRUE, perl = TRUE), ]

  # Apply exclusion patterns
  if (!is.null(exclude_patterns) && length(exclude_patterns) > 0) {
    exclude_regex <- paste(exclude_patterns, collapse = "|")
    matches <- matches[!grepl(exclude_regex, matches$term, ignore.case = TRUE, perl = TRUE), ]
  }

  # Exclude specific EMIS categories (e.g., 7 = family history, 12 = reason for care)
  if (!is.null(category_exclude)) {
    matches <- matches[!matches$emiscodecategoryid %in% category_exclude, ]
  }

  return(matches)
}

# ==============================================================================
# 1. ATOPIC DERMATITIS
# ==============================================================================
# Concept: Atopic dermatitis (atopic eczema) - a chronic inflammatory skin
#   condition characterised by itchy, red, swollen, and cracked skin.
# Synonyms: atopic dermatitis, atopic eczema, Besnier's prurigo, flexural
#   eczema, infantile eczema, endogenous eczema, atopic neurodermatitis,
#   diffuse neurodermatitis
# Exclusions: contact dermatitis, seborrhoeic eczema/dermatitis, non-atopic
#   eczema, neurodermatitis (circumscribed = lichen simplex chronicus),
#   family history, adverse drug reactions, reason-for-care codes,
#   procedures/management
# Category exclusions: 5 (drug-induced), 7 (family history),
#   12 (reason for care), 17 (educational)
# ==============================================================================
cat("--- 1. Atopic Dermatitis ---\n")

ad_include <- c(
  "atopic dermatitis",
  "atopic eczema",
  "besnier",
  "flexural eczema",
  "infantile eczema",
  "endogenous eczema",
  "atopic neurodermatitis",
  "neurodermatitis.*diffuse",
  "diffuse.*neurodermatitis"
)

ad_exclude <- c(
  "family history",
  "\\bFH\\b",
  "adverse reaction",
  "\\[RFC\\]",
  "reason for care",
  "education",
  "management plan",
  "monitoring",
  "review",
  "referral",
  "annual review"
)

ad_codelist <- search_dict(dict, ad_include, ad_exclude,
                           category_exclude = c(5, 7, 12, 17, 37, 40, 46))

cat("  Codes found:", nrow(ad_codelist), "\n")

# ==============================================================================
# 2. VASCULAR DEMENTIA
# ==============================================================================
# Concept: Vascular dementia - dementia caused by cerebrovascular disease
#   (impaired blood supply to the brain).
# Synonyms: vascular dementia, arteriosclerotic dementia, multi-infarct
#   dementia, subcortical vascular dementia, Binswanger's disease/
#   encephalopathy, CADASIL (cerebral autosomal dominant arteriopathy with
#   subcortical infarcts and leukoencephalopathy)
# Exclusions: family history, other dementia types (Alzheimer's,
#   frontotemporal, Lewy body), reason-for-care, management/review codes
# Category exclusions: 7 (family history), 12 (reason for care),
#   37 (cause of death)
# ==============================================================================
cat("--- 2. Vascular Dementia ---\n")

vd_include <- c(
  "vascular dementia",
  "arteriosclerotic dementia",
  "multi-infarct dementia",
  "multi infarct dementia",
  "subcortical vascular dementia",
  "binswanger",
  "\\bCADASIL\\b"
)

vd_exclude <- c(
  "family history",
  "\\bFH\\b",
  "\\[RFC\\]",
  "reason for care",
  "adverse reaction",
  "referral",
  "education",
  "cause of death"
)

vd_codelist <- search_dict(dict, vd_include, vd_exclude,
                           category_exclude = c(7, 12, 37, 40, 46))

cat("  Codes found:", nrow(vd_codelist), "\n")

# ==============================================================================
# 3. EOSINOPHILIC OESOPHAGITIS
# ==============================================================================
# Concept: Eosinophilic oesophagitis (EoE) - a chronic immune-mediated
#   inflammatory disease of the oesophagus characterised by eosinophilic
#   infiltration of the oesophageal mucosa.
# Synonyms: eosinophilic oesophagitis, eosinophilic esophagitis (US spelling)
# Exclusions: family history, reason for care
# Category exclusions: 7 (family history), 12 (reason for care)
# ==============================================================================
cat("--- 3. Eosinophilic Oesophagitis ---\n")

eoe_include <- c(
  "eosinophilic oesophagitis",
  "eosinophilic esophagitis"
)

eoe_exclude <- c(
  "family history",
  "\\bFH\\b",
  "\\[RFC\\]",
  "reason for care",
  "adverse reaction"
)

eoe_codelist <- search_dict(dict, eoe_include, eoe_exclude,
                            category_exclude = c(7, 12, 37))

cat("  Codes found:", nrow(eoe_codelist), "\n")

# ==============================================================================
# 4. PSORIASIS
# ==============================================================================
# Concept: Psoriasis - a chronic autoimmune skin disease causing red, scaly
#   patches on the skin.
# Synonyms: psoriasis (all subtypes: vulgaris, guttate, plaque, pustular,
#   erythrodermic, inverse/flexural, scalp, nail, palmoplantar, etc.),
#   psoriatic arthritis (implies presence of psoriasis)
# Exclusions: parapsoriasis (a distinct group of conditions), family history,
#   assessment tools/scores (PASI, PEST, PsAID), adverse drug reactions,
#   reason-for-care codes
# Note: Psoriatic arthritis is INCLUDED as it indicates the person has
#   psoriasis. H/O psoriasis is included as psoriasis is chronic.
# Category exclusions: 1 (observations/scores), 5 (drug-induced),
#   7 (family history), 12 (reason for care), 37 (admin)
# ==============================================================================
cat("--- 4. Psoriasis ---\n")

pso_include <- c(
  "\\bpsoriasis\\b",
  "\\bpsoriatic\\b",
  "\\bsebopsoriasis\\b"
)

pso_exclude <- c(
  "\\bparapsoriasis\\b",
  "family history",
  "\\bFH\\b",
  "\\[RFC\\]",
  "reason for care",
  "adverse reaction",
  "cause of death",
  "exception reporting",
  "excepted from",
  "PASI",
  "psoriasis area and severity index",
  "PEST",
  "Psoriasis Epidemiology Screening Tool",
  "PsAID",
  "Psoriatic Arthritis Impact of Disease",
  "\\bfear\\b",
  "\\banxiety\\b",
  "education for psoriasis"
)

pso_codelist <- search_dict(dict, pso_include, pso_exclude,
                            category_exclude = c(1, 5, 7, 12, 37, 40, 46))

cat("  Codes found:", nrow(pso_codelist), "\n")

# ==============================================================================
# 5. HIDRADENITIS SUPPURATIVA
# ==============================================================================
# Concept: Hidradenitis suppurativa (HS) - a chronic inflammatory skin
#   condition affecting the hair follicles and apocrine glands, causing
#   painful, boil-like lumps.
# Synonyms: hidradenitis suppurativa, hydradenitis suppurativa (common
#   misspelling in records), hidradenitis (when referring to suppurativa),
#   suppurative hidradenitis, acne inversa
# Exclusions: neutrophilic eccrine hidradenitis (a distinct condition),
#   family history, follicular occlusion triad (broader syndrome code)
# Category exclusions: 7 (family history), 12 (reason for care)
# ==============================================================================
cat("--- 5. Hidradenitis Suppurativa ---\n")

hs_include <- c(
  "hidradenitis suppurativa",
  "hydradenitis suppurativa",
  "suppurative hidradenitis",
  "hidradenitis axillaris",
  "acne inversa",
  "\\bhidradenitis\\b"
)

hs_exclude <- c(
  "neutrophilic eccrine",
  "family history",
  "\\bFH\\b",
  "\\[RFC\\]",
  "reason for care",
  "adverse reaction",
  "follicular occlusion triad",
  "retro-auricular cysts"
)

hs_codelist <- search_dict(dict, hs_include, hs_exclude,
                           category_exclude = c(7, 12, 37))

cat("  Codes found:", nrow(hs_codelist), "\n")

# ==============================================================================
# 6. MYOCARDIAL INFARCTION
# ==============================================================================
# Concept: Myocardial infarction (MI) - death of heart muscle due to
#   interruption of blood supply.
# Synonyms: myocardial infarction (acute, old, healed, silent, subsequent),
#   heart attack, STEMI, NSTEMI, coronary thrombosis
# Inclusions: Acute MI (all types/walls), subsequent MI, old/healed MI,
#   silent MI, history of MI, STEMI, NSTEMI, MI with complications (as these
#   confirm MI occurred), post-MI syndrome/Dressler's (confirms MI),
#   coronary thrombosis (when it results in MI), MINOCA
# Exclusions: Family history, ECG findings (test results, not diagnoses),
#   "coronary thrombosis not resulting in MI", observation/suspected MI,
#   fear/anxiety about MI, cause of death, exception reporting,
#   management plans, procedures, reason-for-care
# Category exclusions: 3 (ECG/tests), 7 (family history), 12 (reason for care),
#   31 (clinical management plans/tests), 37 (admin/cause of death)
# ==============================================================================
cat("--- 6. Myocardial Infarction ---\n")

mi_include <- c(
  "myocardial infarction",
  "\\bheart attack\\b",
  "\\bSTEMI\\b",
  "\\bNSTEMI\\b",
  "\\bAMI\\b",
  "coronary thrombosis",
  "\\bMINOCA\\b",
  "dressler"
)

mi_exclude <- c(
  "family history",
  "\\bFH\\b",
  "\\bNo FH\\b",
  "\\[RFC\\]",
  "reason for care",
  "adverse reaction",
  "not resulting in myocardial infarction",
  "not resulting in MI",
  "\\bno myocardial infarction\\b",
  "observation for suspected",
  "\\bfear\\b",
  "\\banxiety\\b",
  "cause of death",
  "exception reporting",
  "excepted from",
  "management plan",
  "risk score",
  "Thrombolysis In Myocardial Infarction risk",
  "coronary arteriosclerosis in patient with history",
  "new myocardial infarction compared to prior study",
  "diabetes mellitus insulin-glucose infusion",
  "acute mesenteric ischaemia",
  "acute mesenteric infarction",
  "acute coronary artery occlusion not resulting"
)

mi_codelist <- search_dict(dict, mi_include, mi_exclude,
                           category_exclude = c(3, 7, 12, 27, 37, 40, 46))

# Also remove the radioisotope scan and ECG-on-ECG codes that slipped through
mi_codelist <- mi_codelist[!grepl(
  "radioisotope scan|electrocardiogram|\\bECG\\b|stress test",
  mi_codelist$term, ignore.case = TRUE
), ]

cat("  Codes found:", nrow(mi_codelist), "\n")

# ==============================================================================
# 7. WRIST FRACTURE
# ==============================================================================
# Concept: Fracture of the wrist - fractures of the distal radius, distal
#   ulna, and/or carpal bones.
# Synonyms: wrist fracture, Colles' fracture, Smith's fracture, Barton's
#   fracture, scaphoid fracture, fracture of carpal bone(s), fracture of
#   distal/lower end of radius, fracture of distal/lower end of ulna and
#   radius, fracture-dislocation/subluxation of wrist, greenstick fracture
#   of distal radius, torus fracture of radius, die-punch fracture,
#   chauffeur's fracture, reverse Colles', fracture of lunate/triquetral/
#   pisiform/hamate/capitate/trapezium/trapezoid
# Exclusions: Metacarpal fractures (hand fractures, not wrist), finger/
#   phalanx fractures, procedures (closed/open reduction, fixation,
#   arthroscopy), application of casts, sequelae, scaphoid bone of foot,
#   non-fracture codes (bone structure, dislocation without fracture)
# Category exclusions: 7 (family history), 12 (reason for care)
# ==============================================================================
cat("--- 7. Wrist Fracture ---\n")

wf_include <- c(
  "fracture.*wrist",
  "wrist.*fracture",
  "\\bcolles\\b",
  "smith'?s fracture",
  "smith fracture",
  "reverse colles",
  "barton'?s fracture",
  "barton fracture",
  "volar barton",
  "dorsal barton",
  "fracture.*scaphoid",
  "scaphoid.*fracture",
  "fracture.*carpal",
  "carpal.*fracture",
  "fracture.*lunate",
  "lunate.*fracture",
  "fracture.*triquetral",
  "triquetral.*fracture",
  "fracture.*pisiform",
  "pisiform.*fracture",
  "fracture.*hamate",
  "hamate.*fracture",
  "fracture.*capitate",
  "capitate.*fracture",
  "fracture.*trapez",
  "trapez.*fracture",
  "fracture.*distal.*radius",
  "fracture.*lower.*end.*radius",
  "fracture.*radius.*distal",
  "fracture.*lower end of radius",
  "fracture of distal end of radius",
  "distal radius.*fracture",
  "lower end.*radius.*fracture",
  "fracture.*radius and ulna.*distal",
  "fracture.*radius and ulna.*lower",
  "fracture of lower end of both ulna and radius",
  "closed fracture of lower end of radius",
  "open fracture of lower end of radius",
  "fracture at wrist",
  "die.punch",
  "chauffeur.*fracture",
  "greenstick fracture.*distal radius",
  "greenstick.*radius",
  "torus fracture.*radius",
  "extraarticular fracture.*distal radius",
  "extra.articular fracture.*distal radius",
  "intra.articular fracture.*distal radius",
  "greenstick fracture.*distal radius",
  "greenstick fracture.*radius",
  "closed fracture of distal epiphysis of radius",
  "open fracture of metaphysis of distal.*radius",
  "closed fracture of metaphysis of distal.*radius",
  "fracture.*navicular.*wrist",
  "navicular.*wrist.*fracture",
  "fracture dislocation.*wrist",
  "fracture subluxation.*wrist",
  "fracture.dislocation.*radiocarpal",
  "fracture.subluxation.*radiocarpal",
  "fracture.dislocation.*peri.?lunate",
  "fracture.subluxation.*peri.?lunate",
  "fracture.dislocation.*mid.?carpal",
  "fracture.subluxation.*mid.?carpal",
  "fracture.dislocation.*lunate",
  "fracture.subluxation.*lunate",
  "#Radius/ulna-lower end",
  "Colles fracture",
  "fracture.*scaphoid bone of wrist",
  "fracture of bone of.*wrist",
  "pathological fracture of distal radius",
  "pathological fracture of distal ulna"
)

wf_exclude <- c(
  "metacarpal",
  "phalanx",
  "phalang",
  "finger",
  "\\breduction\\b",
  "\\bfixation\\b",
  "arthroscopy",
  "osteotomy",
  "application of.*cast",
  "application of.*plaster",
  "colles plaster",
  "sequelae",
  "sequel",
  "\\bfoot\\b",
  "^bone structure",
  "^distal radius$",          # anatomy-only term
  "\\bdislocation of radius",
  "\\bentire\\b",
  "articular cartilage",
  "epiphyseal plate",
  "growth plate",
  "epiphyseal arrest",
  "hemiarthroplasty",
  "prosthetic",
  "disorder due to and following",
  "family history",
  "\\bFH\\b",
  "\\[RFC\\]",
  "reason for care",
  "interosseous membrane disruption"
)

wf_codelist <- search_dict(dict, wf_include, wf_exclude,
                           category_exclude = c(7, 12, 37, 40, 46))

# Remove any remaining procedure/surgical codes
wf_codelist <- wf_codelist[!grepl(
  "\\breduction of\\b|\\bopen reduction\\b|\\bclosed reduction\\b|percutaneous.*fixation|internal fixation.*without fracture|\\bK-wire\\b|\\bKirschner\\b",
  wf_codelist$term, ignore.case = TRUE
), ]

cat("  Codes found:", nrow(wf_codelist), "\n")

# ==============================================================================
# Assemble all codelists with condition labels and categories
# ==============================================================================

add_condition <- function(codelist, condition_name) {
  codelist %>%
    mutate(condition = condition_name) %>%
    select(condition, medcodeid, term, snomedctconceptid, snomedctdescriptionid,
           originalreadcode, cleansedreadcode, emiscodecategoryid, observations)
}

all_codelists <- bind_rows(
  add_condition(ad_codelist, "Atopic dermatitis"),
  add_condition(vd_codelist, "Vascular dementia"),
  add_condition(eoe_codelist, "Eosinophilic oesophagitis"),
  add_condition(pso_codelist, "Psoriasis"),
  add_condition(hs_codelist, "Hidradenitis suppurativa"),
  add_condition(mi_codelist, "Myocardial infarction"),
  add_condition(wf_codelist, "Wrist fracture")
)

# ==============================================================================
# Export
# ==============================================================================

# Individual codelists
write.csv(ad_codelist %>% add_condition("Atopic dermatitis"),
          "codelist_atopic_dermatitis.csv", row.names = FALSE)
write.csv(vd_codelist %>% add_condition("Vascular dementia"),
          "codelist_vascular_dementia.csv", row.names = FALSE)
write.csv(eoe_codelist %>% add_condition("Eosinophilic oesophagitis"),
          "codelist_eosinophilic_oesophagitis.csv", row.names = FALSE)
write.csv(pso_codelist %>% add_condition("Psoriasis"),
          "codelist_psoriasis.csv", row.names = FALSE)
write.csv(hs_codelist %>% add_condition("Hidradenitis suppurativa"),
          "codelist_hidradenitis_suppurativa.csv", row.names = FALSE)
write.csv(mi_codelist %>% add_condition("Myocardial infarction"),
          "codelist_myocardial_infarction.csv", row.names = FALSE)
write.csv(wf_codelist %>% add_condition("Wrist fracture"),
          "codelist_wrist_fracture.csv", row.names = FALSE)

# Combined codelist
write.csv(all_codelists, "codelist_all_conditions.csv", row.names = FALSE)

# ==============================================================================
# Summary
# ==============================================================================
cat("\n========================================\n")
cat("CODELIST CREATION SUMMARY\n")
cat("========================================\n\n")

summary_df <- all_codelists %>%
  group_by(condition) %>%
  summarise(
    n_codes = n(),
    total_observations = sum(observations, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(condition)

for (i in 1:nrow(summary_df)) {
  cat(sprintf("%-35s %4d codes   %12s observations\n",
              summary_df$condition[i],
              summary_df$n_codes[i],
              format(summary_df$total_observations[i], big.mark = ",")))
}

cat("\nFiles written:\n")
cat("  codelist_atopic_dermatitis.csv\n")
cat("  codelist_vascular_dementia.csv\n")
cat("  codelist_eosinophilic_oesophagitis.csv\n")
cat("  codelist_psoriasis.csv\n")
cat("  codelist_hidradenitis_suppurativa.csv\n")
cat("  codelist_myocardial_infarction.csv\n")
cat("  codelist_wrist_fracture.csv\n")
cat("  codelist_all_conditions.csv\n")
