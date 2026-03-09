################################################################################
# Create codelists for CPRD Aurum
#
# Conditions: atopic dermatitis, vascular dementia, eosinophilic oesophagitis,
#             psoriasis, hidradenitis suppurativa, myocardial infarction,
#             wrist fracture
#
# Methodology follows: Costello et al. (2024) "A practical guide to the
# creation of clinical code lists for use in electronic health record research"
# https://openresearch.nihr.ac.uk/articles/4-20
#
# Steps applied:
#   1. Define clinical concept and inclusion/exclusion criteria
#   2. Search existing codelists (HDR UK Phenotype Library, OpenCodelists)
#   3. Prepare search terms (synonyms, related terms, exclusion terms)
#   4. Create codelist by searching CPRD Aurum medical dictionary
#   5. Apply exclusion filters
#   6. Check observation counts and flag for clinical review
#
# Data source: CPRD Aurum Medical Dictionary (September 2025 release)
# Dictionary columns: medcodeid, term, snomedctconceptid,
#                     snomedctdescriptionid, originalreadcode, cleansedreadcode,
#                     observations, emiscodecategoryid, release
################################################################################

library(haven)
library(dplyr)
library(stringr)
library(readr)

# --- Load CPRD Aurum medical dictionary ---
dict <- read_dta("CPRDAurumMedical_2025_09.dta")

cat("Dictionary loaded:", nrow(dict), "codes\n\n")

# --- Helper function: search dictionary and apply exclusions ---
search_codelist <- function(dict, include_regex, exclude_regex = NULL) {
  result <- dict %>%
    filter(str_detect(term, regex(include_regex, ignore_case = TRUE)))

  if (!is.null(exclude_regex)) {
    result <- result %>%
      filter(!str_detect(term, regex(exclude_regex, ignore_case = TRUE)))
  }

  result %>%
    select(medcodeid, term, snomedctconceptid, snomedctdescriptionid,
           originalreadcode, cleansedreadcode, observations,
           emiscodecategoryid) %>%
    arrange(desc(observations))
}


################################################################################
# 1. ATOPIC DERMATITIS
################################################################################
# Clinical concept: Codes indicating a diagnosis of atopic dermatitis/atopic
# eczema. Includes specific atopic eczema/dermatitis terms and generic eczema
# terms (as unqualified eczema in UK primary care typically refers to atopic
# eczema), excluding known non-atopic eczema subtypes.
#
# Chronic condition: "history of" codes retained as diagnosis persists.
#
# Search terms derived from: clinical synonyms, ICD-10 L20, SNOMED CT
# hierarchy under 200775004 (Atopic dermatitis).
################################################################################

cat("=== 1. ATOPIC DERMATITIS ===\n")

# Specific atopic terms (high specificity)
atopic_specific <- paste0(
  "atopic dermatit|atopic eczema|infantile eczema|flexural eczema|",
  "besnier|intrinsic eczema|endogenous eczema|neurodermatit|",
  "intrinsic dermatit|endogenous dermatit|flexural dermatit|",
  "allergic eczema|allergic \\(intrinsic\\) eczema|AD - Atopic dermatit"
)

# Broader eczema terms (lower specificity - need exclusions)
eczema_broad <- "\\beczema\\b"

# Non-atopic eczema/dermatitis types to exclude from the broad search
eczema_exclude <- paste0(
  "contact|seborrh|varicose|discoid|nummular|dyshidrotic|",
  "pompholyx|stasis|gravitational|perioral|photocontact|",
  "napkin|diaper|nappy|asteatotic|xerotic|xeroderma|",
  "herpeticum|eczematoid otitis|vesicular eczema.*foot|",
  "vesicular eczema.*feet|vesicular eczema.*hand|",
  "apron pattern|coin.shaped|",
  "adverse reaction|Diprobase|Eumovate|E45|Psoriasis And Eczema|",
  "\\[RFC\\]|reason for care|assessment using|POEM"
)

# Search: specific atopic terms (no eczema-type exclusions needed)
atopic_specific_codes <- search_codelist(dict, atopic_specific)
# Remove non-diagnosis terms from specific search too
atopic_specific_codes <- atopic_specific_codes %>%
  filter(!str_detect(term, regex(
    "family history|\\bFH:|referral|refer to|education|leaflet|advice|management plan|screening|risk of|adverse reaction|allergy|asthma|conjunctiv|cataract|\\[RFC\\]|assessment",
    ignore_case = TRUE
  )))

# Search: broader eczema terms with non-atopic exclusions
eczema_broad_codes <- search_codelist(dict, eczema_broad, eczema_exclude)
# Also remove non-diagnosis terms from broad search
eczema_broad_codes <- eczema_broad_codes %>%
  filter(!str_detect(term, regex(
    "family history|\\bFH:|referral|refer to|education|leaflet|advice|management plan|screening|risk of|adverse reaction|allergy|overlap",
    ignore_case = TRUE
  )))

# Combine and deduplicate
atopic_dermatitis <- bind_rows(atopic_specific_codes, eczema_broad_codes) %>%
  distinct(medcodeid, .keep_all = TRUE) %>%
  arrange(desc(observations))

cat("  Codes found:", nrow(atopic_dermatitis), "\n")
cat("  Total observations:", sum(atopic_dermatitis$observations, na.rm = TRUE), "\n\n")


################################################################################
# 2. VASCULAR DEMENTIA
################################################################################
# Clinical concept: Codes indicating a diagnosis of vascular dementia.
# Includes multi-infarct dementia, arteriosclerotic dementia, subcortical
# vascular dementia, and Binswanger's disease.
#
# Chronic, non-resolving condition: all diagnostic codes retained.
#
# Search terms derived from: ICD-10 F01, SNOMED CT hierarchy under
# 429998004 (Vascular dementia).
################################################################################

cat("=== 2. VASCULAR DEMENTIA ===\n")

vascular_dementia_include <- paste0(
  "vascular dementia|multi.infarct dementia|arteriosclerotic dementia|",
  "binswanger|subcortical.*vascular.*dementia|subcortical atherosclerotic dementia|",
  "cerebrovascular.*dementia|\\bVAD\\b.*vascular dementia|",
  "\\bMID\\b.*multi.infarct|ischaemic vascular dementia|",
  "ischemic vascular dementia|cortical vascular dementia"
)

vascular_dementia_exclude <- paste0(
  "family history|\\bFH:|referral|refer to|screening|",
  "assessment|risk of|education|leaflet|management plan|",
  "carer|information about|suspected|observation for"
)

vascular_dementia <- search_codelist(
  dict, vascular_dementia_include, vascular_dementia_exclude
)

# Remove "subcortical dementia" without "vascular" qualifier (too non-specific)
vascular_dementia <- vascular_dementia %>%
  filter(!(str_detect(term, regex("^subcortical dementia$", ignore_case = TRUE))))

cat("  Codes found:", nrow(vascular_dementia), "\n")
cat("  Total observations:", sum(vascular_dementia$observations, na.rm = TRUE), "\n\n")


################################################################################
# 3. EOSINOPHILIC OESOPHAGITIS
################################################################################
# Clinical concept: Codes indicating a diagnosis of eosinophilic oesophagitis
# (EoE). A chronic immune-mediated condition of the oesophagus.
#
# Chronic condition: all diagnostic codes retained.
#
# Search terms derived from: ICD-10 K20.0, SNOMED CT 235599003.
################################################################################

cat("=== 3. EOSINOPHILIC OESOPHAGITIS ===\n")

eoe_include <- "eosinophilic (o|e)esophagit"

eoe_exclude <- paste0(
  "family history|\\bFH:|referral|refer to|screening|",
  "risk of|education|leaflet|management plan|suspected|",
  "observation for|ulcer"
)

eosinophilic_oesophagitis <- search_codelist(dict, eoe_include, eoe_exclude)

cat("  Codes found:", nrow(eosinophilic_oesophagitis), "\n")
cat("  Total observations:", sum(eosinophilic_oesophagitis$observations, na.rm = TRUE), "\n\n")


################################################################################
# 4. PSORIASIS
################################################################################
# Clinical concept: Codes indicating a diagnosis of psoriasis (skin condition).
# Excludes psoriatic arthritis/arthropathy/spondylitis (distinct musculoskeletal
# conditions) and parapsoriasis (a separate condition).
#
# Chronic condition: "history of" codes retained.
#
# Search terms derived from: ICD-10 L40 (excluding L40.5), SNOMED CT hierarchy
# under 9014002 (Psoriasis).
################################################################################

cat("=== 4. PSORIASIS ===\n")

psoriasis_include <- "psoriasis|psoriatic"

psoriasis_exclude <- paste0(
  # Separate musculoskeletal conditions
  "psoriatic arthritis|psoriatic arthropathy|psoriatic spondylitis|",
  "psoriatic dactylitis|arthritis in psoriasis|arthropathy|",
  "spondyl|\\bPsA\\b|PA \\(Psoriatic arthritis\\)|",
  "Psoriatic Arthritis Impact|PsAID|",
  "iritis in psoriatic|juvenile arthritis in psoriasis|",
  "juvenile psoriatic arthritis|",
  # Parapsoriasis (separate condition)
  "parapsoriasis|",
  # Non-diagnostic terms
  "family history|\\bFH:|referral|refer to|screening|",
  "education|leaflet|advice|management plan|",
  "assessment using|\\bPEST\\b|\\bPASI\\b|",
  "Epidemiology Screening Tool|area and severity index|",
  "adverse reaction|\\[RFC\\]|reason for care|",
  "risk of|excepted from|exception reporting|",
  "psoriasis and eczema|Psoriasis And Eczema|",
  "psoriasis.eczema overlap|psoriasis with eczema"
)

psoriasis <- search_codelist(dict, psoriasis_include, psoriasis_exclude)

cat("  Codes found:", nrow(psoriasis), "\n")
cat("  Total observations:", sum(psoriasis$observations, na.rm = TRUE), "\n\n")


################################################################################
# 5. HIDRADENITIS SUPPURATIVA
################################################################################
# Clinical concept: Codes indicating a diagnosis of hidradenitis suppurativa
# (HS), also known as acne inversa. A chronic inflammatory skin condition
# affecting apocrine gland-bearing skin.
#
# Chronic condition: all diagnostic codes retained.
#
# Search terms derived from: ICD-10 L73.2, SNOMED CT 59393003.
################################################################################

cat("=== 5. HIDRADENITIS SUPPURATIVA ===\n")

hs_include <- "hidradenitis|acne inversa"

hs_exclude <- paste0(
  # Different condition (neutrophilic eccrine hidradenitis)
  "neutrophilic eccrine|",
  # Non-diagnostic terms
  "family history|\\bFH:|referral|refer to|screening|",
  "education|leaflet|management plan|risk of|suspected"
)

hidradenitis_suppurativa <- search_codelist(dict, hs_include, hs_exclude)

cat("  Codes found:", nrow(hidradenitis_suppurativa), "\n")
cat("  Total observations:", sum(hidradenitis_suppurativa$observations, na.rm = TRUE), "\n\n")


################################################################################
# 6. MYOCARDIAL INFARCTION
################################################################################
# Clinical concept: Codes indicating a current/active myocardial infarction
# (MI). Includes acute MI, STEMI, NSTEMI, coronary thrombosis (resulting in
# MI), silent MI, subsequent MI, and type 1/2 MI.
#
# Acute condition: "history of", "old MI", and "healed MI" codes EXCLUDED
# as they indicate past events, not a current MI. Complications following MI,
# ECG findings, risk scores, procedures, and administrative codes also excluded.
#
# Search terms derived from: ICD-10 I21-I22, SNOMED CT hierarchy under
# 22298006 (Myocardial infarction).
################################################################################

cat("=== 6. MYOCARDIAL INFARCTION ===\n")

mi_include <- paste0(
  "myocardial infarct|heart attack|\\bSTEMI\\b|\\bNSTEMI\\b|",
  "coronary thrombos|transmural infarct|subendocardial infarct|",
  "\\bAMI\\b.*myocardial|\\bMI\\b - .*myocardial|",
  "\\bMINOCA\\b"
)

mi_exclude <- paste0(
  # Past/historical events (not current MI)
  "history of|\\bH/O:|past history|personal history|old myocardial|",
  "old anterior|old inferior|old lateral|old posterior|old apical|",
  "healed myocardial|\\bold\\b.*infarct|",
  # Family history
  "family history|\\bFH:|\\bFH myocardial|No FH:|",
  # Not resulting in MI
  "not resulting in myocardial|no myocardial infarction|",
  "ECG: no myocardial|",
  # Complications following MI (separate conditions)
  "as current complication following|complication following|",
  "following acute myocardial|pericarditis.*myocardial|",
  "post.myocardial infarction syndrome|postmyocardial infarction syndrome|",
  "postmyocardial infarction pericardial|",
  "rupture.*after.*myocardial|rupture.*due to.*myocardial|",
  "mural thrombus.*following|thrombosis of atrium.*following|",
  "ventricular aneurysm.*following|ventricular thrombus following|",
  "mitral valve regurgitation due to|supraventricular tachycardia following|",
  "pulmonary embolism.*following|",
  "delayed postmyocardial|",
  # ECG findings and investigations
  "ECG:|radioisotope|inducible ischaemia|myocardial ischaemia manifest|",
  "stress test|new myocardial infarction compared|",
  "anterior myocardial infarction on electrocardiogram|",
  "inferior myocardial infarction on electrocardiogram|",
  # Administrative/management codes
  "observation for suspected|cause of death|clinical management plan|",
  "excepted from|exception reporting|\\[RFC\\]|reason for care|",
  "Thrombolysis In Myocardial Infarction risk|",
  "diabetes mellitus insulin-glucose|",
  # Fear/anxiety (not diagnosis)
  "fear of|anxiety about|",
  # Coronary arteriosclerosis with history (not current MI)
  "coronary arteriosclerosis in patient"
)

myocardial_infarction <- search_codelist(dict, mi_include, mi_exclude)

cat("  Codes found:", nrow(myocardial_infarction), "\n")
cat("  Total observations:", sum(myocardial_infarction$observations, na.rm = TRUE), "\n\n")


################################################################################
# 7. WRIST FRACTURE
################################################################################
# Clinical concept: Codes indicating a current fracture at the wrist.
# Anatomically includes: distal radius, distal ulna, and carpal bones
# (scaphoid, lunate, triquetral, pisiform, trapezium, trapezoid, capitate,
# hamate). Also includes named wrist fracture types (Colles', Smith's,
# Barton's, chauffeur's).
#
# EXCLUDES: metacarpal fractures (hand, not wrist), surgical procedures,
# sequelae/complications of healed fractures, history of fracture.
#
# Acute condition: "history of" codes excluded.
#
# Search terms derived from: ICD-10 S62.0-S62.1, S52.5-S52.6, SNOMED CT
# hierarchies for fracture of wrist region.
################################################################################

cat("=== 7. WRIST FRACTURE ===\n")

wrist_include <- paste0(
  # Explicit wrist fracture terms
  "fracture.*wrist|wrist.*fracture|fracture.*at wrist|",
  # Named wrist fracture types
  "colles|smith.s fracture|smith fracture|barton.*fracture|barton.s fracture|",
  "chauffeur.s fracture|",
  # Distal radius and ulna fractures
  "fracture.*distal.*radi|fracture.*distal.*ulna|",
  "fracture.*distal end of radi|fracture.*distal end of ulna|",
  "fracture.*distal epiphysis of radi|",
  "fracture.*lower end of radi|fracture.*lower end of ulna|",
  "fracture.*metaphysis of distal|",
  "greenstick fracture.*distal radi|",
  "extra.?articular fracture.*distal radi|",
  "intra.?articular fracture.*distal radi|",
  "pathological fracture.*distal radi|pathological fracture.*distal ulna|",
  "die.punch|fracture.*radial styloid|",
  # Carpal bone fractures
  "fracture.*carpal|fracture.*scaphoid|fracture.*lunate|",
  "fracture.*triquetral|fracture.*pisiform|fracture.*trapez|",
  "fracture.*capitate|fracture.*hamate|fracture.*navicular.*wrist|",
  "hand fracture.*carpal|",
  # Read code shorthand: # means fracture
  "#Scaphoid|#Carpal bones|#Radius/ulna.lower end"
)

wrist_exclude <- paste0(
  # Metacarpal fractures (hand, not wrist)
  "metacarpal|",
  # Surgical and procedural codes
  "reduction of fracture|fixation.*fracture|fracture.*fixation|",
  "arthroscopy|percutaneous.*fixation|application of.*plaster|plaster cast|",
  # Sequelae and complications of healed fractures
  "sequelae|disorder due to|malunion|nonunion|non.union|delayed union|",
  # Past fractures
  "history of|\\bH/O:|",
  # Scaphoid of foot (different anatomical site)
  "scaphoid.*foot|",
  # Non-diagnostic terms
  "family history|\\bFH:|referral|refer to|screening|",
  "education|leaflet|management plan|risk of|suspected|",
  "\\[X\\]Fracture of other carpal bone|",
  # Fracture at wrist AND hand level is too broad
  "fracture at wrist and hand level|fracture at wrist and/or hand level|",
  "fracture.*dislocation.*radioulnar|",
  "fracture.*head of radius"
)

wrist_fracture <- search_codelist(dict, wrist_include, wrist_exclude)

# Remove fracture-dislocation codes that are more dislocation than fracture
# but keep genuine fracture-dislocations of the wrist/carpal bones
wrist_fracture <- wrist_fracture %>%
  filter(!str_detect(term, regex(
    "fracture.*dislocation.*carpometacarpal|dislocation.*distal radioulnar",
    ignore_case = TRUE
  )))

cat("  Codes found:", nrow(wrist_fracture), "\n")
cat("  Total observations:", sum(wrist_fracture$observations, na.rm = TRUE), "\n\n")


################################################################################
# EXPORT CODELISTS
################################################################################

cat("=== EXPORTING CODELISTS ===\n\n")

# Create output directory
output_dir <- "codelists"
if (!dir.exists(output_dir)) dir.create(output_dir)

# List of codelists to export
codelists <- list(
  "atopic_dermatitis"          = atopic_dermatitis,
  "vascular_dementia"          = vascular_dementia,
  "eosinophilic_oesophagitis"  = eosinophilic_oesophagitis,
  "psoriasis"                  = psoriasis,
  "hidradenitis_suppurativa"   = hidradenitis_suppurativa,
  "myocardial_infarction"      = myocardial_infarction,
  "wrist_fracture"             = wrist_fracture
)

for (name in names(codelists)) {
  filepath <- file.path(output_dir, paste0(name, "_codelist.csv"))
  write_csv(codelists[[name]], filepath)
  cat(sprintf("  %-35s %4d codes -> %s\n", name, nrow(codelists[[name]]), filepath))
}

cat("\n")

################################################################################
# QUALITY CHECKS: Summary statistics for review
################################################################################

cat("=== QUALITY CHECK SUMMARY ===\n\n")

for (name in names(codelists)) {
  cl <- codelists[[name]]
  cat(sprintf("--- %s ---\n", toupper(gsub("_", " ", name))))
  cat(sprintf("  Total codes: %d\n", nrow(cl)))
  cat(sprintf("  Total observations: %s\n", format(sum(cl$observations, na.rm = TRUE), big.mark = ",")))
  cat(sprintf("  Codes with >0 observations: %d\n", sum(cl$observations > 0, na.rm = TRUE)))
  cat(sprintf("  Codes with 0 observations: %d\n", sum(cl$observations == 0, na.rm = TRUE)))

  # EMIS category breakdown
  cat("  EMIS categories present: ")
  cat(paste(sort(unique(cl$emiscodecategoryid)), collapse = ", "), "\n")

  # Top 5 codes by observation count
  cat("  Top 5 codes by observations:\n")
  top5 <- head(cl, 5)
  for (i in seq_len(nrow(top5))) {
    cat(sprintf("    %s (%s obs) [medcodeid: %s]\n",
                top5$term[i],
                format(top5$observations[i], big.mark = ","),
                top5$medcodeid[i]))
  }
  cat("\n")
}

cat("=== CODELIST CREATION COMPLETE ===\n")
cat("All codelists exported to the 'codelists/' directory.\n")
cat("Please review each codelist with a clinical expert before use.\n")
