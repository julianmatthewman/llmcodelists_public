library(ragnar)
library(tidyverse)
library(ellmer)
library(tictoc)

# Get CPRD browser
# Replace the path below with the local path to your CPRD data file
base_file <- "path/to/CPRDAurumMedical_2025_09.dta"


browser <- haven::read_dta(base_file)
dat <- browser |> select(originalreadcode, snomedctconceptid, text = term)

# Create store
store_location <- "aurum_medcpt_query.ragnar.duckdb"
store <- ragnar_store_create(
  store_location,
  embed = ragnar::embed_ollama(model = "oscardp96/medcpt-query:latest"),
  overwrite = FALSE,
  version = 1 #Set version to 1
)

# Create embedings
tic()
dat <- dat |> ragnar::embed_ollama(model = "oscardp96/medcpt-query:latest") #Create embedings on terms only
dat <- dat |> mutate(text = paste(originalreadcode, text)) #Then merge with the code
toc() # Takes 48 minutes

# Insert embedings into store
tic()
ragnar_store_insert(store, dat)
toc()

# Retreive chunks
ragnar_store_build_index(store)
text <- "What are the codes for acoustic neuroma?"
ragnar_retrieve(store, text, top_k = 10, deoverlap = FALSE) #deoverlap is needed
