###############################################################################
# Codelist creation for CPRD Aurum Medical Dictionary
#
# Following the NIHR 10-step checklist for codelist development:
#   Skinner et al. (2023) https://openresearch.nihr.ac.uk/articles/4-20
#
# Target data source: CPRD Aurum (September 2025 release)
# Terminology: SNOMED CT (via CPRD Aurum medical dictionary)
#
# Conditions:
#   1. Atopic dermatitis
#   2. Vascular dementia
#   3. Eosinophilic oesophagitis
#   4. Psoriasis
#   5. Hidradenitis suppurativa
#   6. Myocardial infarction
#   7. Wrist fracture
#
# Concept definition: Include all codes that, when recorded in a person's
# medical record, would indicate that the person currently has the condition.
# This means diagnostic codes (active and historical) are included, while
# family history, screening tools, referrals, procedures, and adverse
# reactions to products are excluded.
###############################################################################

library(haven)
library(dplyr)
library(stringr)

# --- Load CPRD Aurum medical dictionary ---
# Replace the path below with the local path to your CPRD data directory
dict_path <- file.path(
  "path/to/data",
  "CPRDAurumMedical_2025_09.dta"
)
medical <- read_dta(dict_path)
medical <- medical %>% mutate(term_lower = tolower(term))

cat("Dictionary loaded:", nrow(medical), "terms\n\n")

###############################################################################
# Helper function: search dictionary with inclusion and exclusion regex
###############################################################################
build_codelist <- function(data,
                           include_regex,
                           exclude_regex = NULL,
                           condition_name) {

  # Step 6b: Apply search terms
  idx <- grepl(include_regex, data$term_lower, perl = TRUE)

  # Step 6d: Apply exclusion terms
  if (!is.null(exclude_regex)) {
    exc <- grepl(exclude_regex, data$term_lower, perl = TRUE)
    idx <- idx & !exc
  }

  result <- data %>%
    filter(idx) %>%
    select(medcodeid, term, snomedctconceptid, snomedctdescriptionid,
           originalreadcode, cleansedreadcode, observations) %>%
    arrange(desc(observations)) %>%
    mutate(condition = condition_name)

  cat(sprintf("%-35s: %d codes found\n", condition_name, nrow(result)))
  return(result)
}

###############################################################################
# 1. ATOPIC DERMATITIS
#
# Clinical concept: Atopic dermatitis (atopic eczema / Besnier's prurigo).
# A chronic, relapsing inflammatory skin condition with an immunological basis.
#
# Included: atopic dermatitis, atopic eczema, Besnier's prurigo, infantile
#   eczema, flexural eczema, endogenous eczema, constitutional eczema.
#   Also "allergic eczema" and "intrinsic eczema" which are used as synonyms
#   for atopic eczema in UK primary care.
#
# Excluded: non-atopic eczema subtypes (contact, seborrhoeic, varicose,
#   discoid, asteatotic, gravitational, stasis, venous, dyshidrotic,
#   nummular, pompholyx, occupational, phototoxic, psoriasiform),
#   family history, adverse reactions to products, assessment tools (POEM),
#   referrals, eczema herpeticum (secondary infection), education.
###############################################################################

atopic_dermatitis <- build_codelist(
  medical,
  include_regex = paste0(
    "atopic dermatitis|atopic eczema|",
    "besnier|",
    "infantile eczema|",
    "flexural eczema|",
    "endogenous eczema|",
    "constitutional eczema|",
    "allergic.*(intrinsic).*eczema|",
    "allergic eczema|",
    "intrinsic eczema"
  ),
  exclude_regex = paste0(
    "family history|\\bfh[:\\s]|\\bno fh|",
    "adverse reaction|",
    "assessment|\\bpoem\\b|score|",
    "referral|",
    "education|",
    "asthma|conjunctiv|cataract|rhinitis|",
    "diathesis|atopic allergy"
  ),
  condition_name = "Atopic dermatitis"
)

###############################################################################
# 2. VASCULAR DEMENTIA
#
# Clinical concept: Dementia resulting from cerebrovascular disease, including
# multi-infarct dementia, arteriosclerotic dementia, subcortical vascular
# dementia, and Binswanger's encephalopathy.
#
# Included: vascular dementia (all subtypes), multi-infarct dementia,
#   arteriosclerotic dementia, subcortical vascular/atherosclerotic dementia,
#   subcortical arteriosclerotic encephalopathy (Binswanger's disease).
#
# Excluded: other subcortical conditions (haemorrhage, heterotopia,
#   leucoencephalopathy not dementia-related, gliosis), CADASIL (genetic
#   condition, not vascular dementia per se), family history, referrals,
#   screening.
###############################################################################

vascular_dementia <- build_codelist(
  medical,
  include_regex = paste0(
    "vascular dementia|",
    "multi-infarct dementia|multi infarct dementia|\\bmid\\b.*multi-infarct|",
    "arteriosclerotic dementia|",
    "subcortical.*dementia|",
    "subcortical vascular|",
    "subcortical atherosclerotic|",
    "subcortical arteriosclerotic encephalopathy|",
    "multi-infarct state"
  ),
  exclude_regex = paste0(
    "family history|\\bfh[:\\s]|\\bno fh|",
    "referral|",
    "screening|",
    "suspected|",
    "subcortical.*ha?emorrhag|",
    "subcortical.*heterotopia|",
    "subcortical.*leucoencephalopathy|",
    "subcortical.*gliosis|",
    "subcortical.*infarcts and le[u]"
  ),
  condition_name = "Vascular dementia"
)

###############################################################################
# 3. EOSINOPHILIC OESOPHAGITIS
#
# Clinical concept: Eosinophilic oesophagitis (EoE), a chronic immune-
# mediated oesophageal disease characterised by eosinophil-predominant
# inflammation.
#
# Included: eosinophilic oesophagitis (both UK and US spellings),
#   food-induced eosinophilic oesophagitis.
#
# Excluded: family history.
###############################################################################

eosinophilic_oesophagitis <- build_codelist(
  medical,
  include_regex = "eosinophilic o?esophagitis",
  exclude_regex = "family history|\\bfh[:\\s]|\\bno fh",
  condition_name = "Eosinophilic oesophagitis"
)

###############################################################################
# 4. PSORIASIS
#
# Clinical concept: Psoriasis, a chronic immune-mediated inflammatory skin
# disease. All subtypes included (plaque, guttate, pustular, erythrodermic,
# inverse, nail, etc.) as well as psoriatic arthritis (which implies
# psoriasis).
#
# Included: all psoriasis subtypes, psoriatic arthritis, sebopsoriasis,
#   psoriasis with arthropathy, history of psoriasis.
#
# Excluded: parapsoriasis (separate condition), psoriasiform eruptions/
#   dermatitis (morphological descriptor, not psoriasis), family history,
#   adverse reactions to products, assessment/screening tools (PASI, PEST),
#   Bowen disease (psoriasiform variant is not psoriasis), education,
#   referrals, seborrhoea with psoriasiform elements (not psoriasis),
#   "juvenile psoriatic arthritis without psoriasis" (explicitly no
#   psoriasis).
###############################################################################

psoriasis <- build_codelist(
  medical,
  include_regex = paste0(
    "\\bpsoriasis\\b|\\bpsoriatic\\b|",
    "sebopsoriasis|",
    "seborrh[oe]+ic psoriasis"
  ),
  exclude_regex = paste0(
    "parapsoriasis|",
    "psoriasiform|",
    "family history|\\bfh[:\\s]|\\bno fh|",
    "adverse reaction|",
    "\\bpasi\\b|\\bpest\\b|",
    "psoriasis area and severity index|",
    "psoriasis epidemiology screening tool|",
    "assessment using|",
    "education for|",
    "referral|",
    "bowen|",
    "seborrhoea.*psoriasiform|",
    "without psoriasis|",
    "spongiotic psoriasiform"
  ),
  condition_name = "Psoriasis"
)

###############################################################################
# 5. HIDRADENITIS SUPPURATIVA
#
# Clinical concept: Hidradenitis suppurativa (HS), a chronic inflammatory
# skin disease of the hair follicles, also known as acne inversa.
#
# Included: hidradenitis suppurativa, hidradenitis (general - in UK primary
#   care this overwhelmingly refers to HS), suppurative hidradenitis,
#   hidradenitis axillaris, acne inversa, follicular occlusion triad
#   (includes HS as a component).
#
# Excluded: neutrophilic eccrine hidradenitis (a distinct chemotherapy-
#   related condition), family history.
###############################################################################

hidradenitis_suppurativa <- build_codelist(
  medical,
  include_regex = paste0(
    "hidradenitis suppurativa|",
    "suppurative hidradenitis|",
    "\\bhidradenitis\\b|",
    "hidradenitis axillaris|",
    "acne inversa|",
    "follicular occlusion triad"
  ),
  exclude_regex = paste0(
    "neutrophilic eccrine|",
    "family history|\\bfh[:\\s]|\\bno fh"
  ),
  condition_name = "Hidradenitis suppurativa"
)

###############################################################################
# 6. MYOCARDIAL INFARCTION
#
# Clinical concept: Myocardial infarction (MI), including acute MI, old/past
# MI, and recognised complications that confirm MI occurred.
#
# Included: acute MI (all subtypes and wall locations), STEMI, NSTEMI,
#   old/past MI, history of MI, silent MI, subsequent MI, postoperative MI,
#   heart attack, MI-related complications (post-MI syndrome, ruptures,
#   thrombi - these confirm the MI event occurred), aborted MI.
#
# Excluded: family history, observation/suspected MI, risk scores (TIMI),
#   diagnostic procedures (radioisotope scan), "not resulting in myocardial
#   infarction", ECG showing no MI, quality indicator exceptions,
#   anxiety/fear about heart attack, clinical management plans,
#   treatment protocols (insulin-glucose infusion), coronary arteriosclerosis
#   codes that mention MI only as context.
###############################################################################

myocardial_infarction <- build_codelist(
  medical,
  include_regex = paste0(
    "myocardial infarction|",
    "heart attack|",
    "\\bstemi\\b|",
    "\\bnstemi\\b|",
    "st segment elevation myocardial|",
    "non-st segment elevation myocardial|",
    "coronary thrombosis"
  ),
  exclude_regex = paste0(
    "family history|\\bfh[:\\s]|\\bno fh\\b|",
    "observation for suspected|",
    "not resulting in myocardial infarction|",
    "radioisotope scan|",
    "risk score|",
    "thrombolysis in myocardial infarction risk|",
    "ischaemia manifest on stress test|",
    "ecg:.*no myocardial|no myocardial infarction|",
    "excepted from.*quality|exception reporting.*quality|quality indicators|",
    "anxiety about.*heart attack|fear of.*heart attack|",
    "clinical management plan|",
    "insulin.glucose infusion|",
    "coronary arteriosclerosis in patient"
  ),
  condition_name = "Myocardial infarction"
)

###############################################################################
# 7. WRIST FRACTURE
#
# Clinical concept: Fractures of the wrist, including fractures of the
# distal radius, distal ulna, and carpal bones.
#
# Included: fractures of the wrist (all carpal bones: scaphoid, lunate,
#   triquetral, pisiform, hamate, capitate, trapezium, trapezoid),
#   distal radius fractures, distal/lower end of radius, Colles' fracture,
#   Smith's fracture, Barton fracture, Chauffeur's fracture, reversed
#   Colles', pathological fractures of distal radius/ulna, greenstick
#   fracture of distal radius, fracture-dislocations of the wrist,
#   sequelae and disorders following wrist fracture.
#
# Excluded: metacarpal fractures (hand, not wrist), fractures of other parts
#   of the radius (head, neck, shaft, proximal), treatment/procedure codes
#   (plaster casts, reductions, bone grafts, fixation), Bartonella (bacterium),
#   Barton forceps (obstetric), bone structure/anatomy codes, neoplasms,
#   non-fracture scaphoid codes.
###############################################################################

wrist_fracture <- build_codelist(
  medical,
  include_regex = paste0(
    "fracture.*(wrist|distal radius|distal ulna)|",
    "fracture.*lower.*end.*(radius|ulna)|",
    "fracture.*lower end of both ulna and radius|",
    "fracture of bone of.*(wrist|left wrist|right wrist)|",
    "fracture at wrist|",
    "fracture.*capitate.*wrist|",
    "fracture.*hamate.*wrist|",
    "fracture.*lunate.*wrist|",
    "fracture.*pisiform.*wrist|",
    "fracture.*scaphoid.*wrist|",
    "fracture.*trapezi.*wrist|",
    "fracture.*triquetral.*wrist|",
    "fracture.*navicular.*wrist|",
    "fracture.*(?<!meta)carpal bone|",
    "fracture.*carpal bones|",
    "hand fracture.*carpal|",
    "colles.*fracture|colles'|#.*colles|",
    "smith.*fracture|smith's fracture|",
    "barton.*fracture|barton's fracture|",
    "chauffeur.*fracture|",
    "fracture dislocation.*wrist|",
    "fracture subluxation.*wrist|",
    "fracture.dislocation.*(radiocarpal|midcarpal|mid carpal|carpometacarpal)|",
    "fracture.subluxation.*(radiocarpal|midcarpal|mid carpal|carpometacarpal|other carpal)|",
    "greenstick.*distal radius|",
    "pathological fracture.*distal radius|",
    "pathological fracture.*distal ulna|",
    "sequelae.*fracture.*wrist|",
    "disorder.*following fracture.*wrist|",
    "\\b#scaphoid\\b|\\b#radius.*lower.*colles|",
    "reversed? colles"
  ),
  exclude_regex = paste0(
    "family history|\\bfh[:\\s]|\\bno fh|",
    "application of.*plaster|application of.*cast|",
    "closed reduction|open reduction|",
    "percutaneous.*fixation|skeletal fixation|",
    "bone graft|",
    "bone structure|",
    "bartonell|",
    "barton forceps|",
    "neoplasm|",
    "synostosis|",
    "\\[so\\]scaphoid|",
    "antibody|igg|igm|",
    "metacarpal|",
    "fracture.*head of radius|",
    "fracture.*neck of radius|",
    "fracture.*shaft.*radius|",
    "fracture.*proximal.*radius|",
    "fracture.*radius and ulna.*nos|",
    "fracture of radius\\b(?!.*lower|.*distal|.*wrist)|",
    "closed fracture of radius\\b(?!.*lower|.*distal|.*wrist)"
  ),
  condition_name = "Wrist fracture"
)

###############################################################################
# Combine and export all codelists
###############################################################################

all_codelists <- bind_rows(
  atopic_dermatitis,
  vascular_dementia,
  eosinophilic_oesophagitis,
  psoriasis,
  hidradenitis_suppurativa,
  myocardial_infarction,
  wrist_fracture
)

# --- Summary table ---
cat("\n========================================\n")
cat("CODELIST SUMMARY\n")
cat("========================================\n")
summary_table <- all_codelists %>%
  group_by(condition) %>%
  summarise(
    n_codes = n(),
    total_observations = sum(observations, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(condition)
print(as.data.frame(summary_table))

# --- Export combined codelist ---
# Replace the path below with the local path to your desired output directory
output_dir <- "path/to/output"

write.csv(
  all_codelists,
  file.path(output_dir, "all_codelists_combined.csv"),
  row.names = FALSE
)

# --- Export individual codelists ---
conditions_list <- list(
  "atopic_dermatitis"         = atopic_dermatitis,
  "vascular_dementia"         = vascular_dementia,
  "eosinophilic_oesophagitis" = eosinophilic_oesophagitis,
  "psoriasis"                 = psoriasis,
  "hidradenitis_suppurativa"  = hidradenitis_suppurativa,
  "myocardial_infarction"     = myocardial_infarction,
  "wrist_fracture"            = wrist_fracture
)

for (name in names(conditions_list)) {
  write.csv(
    conditions_list[[name]],
    file.path(output_dir, paste0("codelist_", name, ".csv")),
    row.names = FALSE
  )
}

cat("\nCodelists saved to:", output_dir, "\n")
cat("  - all_codelists_combined.csv (combined file)\n")
cat("  - codelist_<condition>.csv (individual files)\n")

###############################################################################
# Print each codelist for review (Step 7)
###############################################################################
cat("\n\n========================================\n")
cat("DETAILED CODELIST OUTPUT FOR REVIEW\n")
cat("========================================\n")

for (name in names(conditions_list)) {
  cl <- conditions_list[[name]]
  cat("\n----------------------------------------\n")
  cat(toupper(gsub("_", " ", name)), "\n")
  cat(sprintf("  %d codes | %s total observations\n",
              nrow(cl), format(sum(cl$observations, na.rm = TRUE), big.mark = ",")))
  cat("----------------------------------------\n")
  for (i in seq_len(nrow(cl))) {
    cat(sprintf("  %-18s  %8d obs  %s\n",
                cl$medcodeid[i],
                cl$observations[i],
                cl$term[i]))
  }
}
