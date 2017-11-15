library(magrittr)
suppressPackageStartupMessages(suppressMessages(suppressWarnings({
  model_index <- readr::read_csv(file.path("models", "model-index.csv"))
  model_accuracy <- readr::read_tsv(file.path("models", "model-accuracy.tsv"))
})))
source("features.R")

to_retry <- model_accuracy %>%
  dplyr::filter(classifier == "meta", accuracy < 0.5) %>%
  dplyr::left_join(model_index, by = c("classes", "question", "features", "discernatron_reliable"))

# Delete cached base learners:
file.remove(to_retry$base_learners)
# Delete cached meta learners:
file.remove(to_retry$meta_learner)
# Update model cache index:
readr::write_csv(model_index[!model_index$base_learners %in% to_retry$base_learners, ], file.path("models", "model-index.csv"))

# Now run: `Rscript models.R` again until those combos are re-created.
