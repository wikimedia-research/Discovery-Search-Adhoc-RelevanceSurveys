per_question <- readr::read_rds(file.path("data", "per_question.rds"))
source("features.R")

per_question$`If someone searched for '...', would they want to read this article?` %>%
  dplyr::filter(reliable) %>%
  { .[, c("query", "page_title_db", "Class", features$`survey-only`)] } %>%
  dplyr::rename(page_title = page_title_db, label = Class) %>%
  dplyr::mutate(label = as.numeric(label) - 1) %>%
  readr::write_csv(file.path("production", "example.csv"))

readr::read_csv(file.path("models", "keras-index-legacy.csv")) %>%
  dplyr::filter(
    question == "If someone searched for '...', would they want to read this article?",
    features == "survey-only", discernatron_reliable
  ) %>%
  dplyr::mutate(learner = sub("\\", "/", learner, fixed = TRUE)) %>%
  .$learner %>%
  file.copy(file.path("production", "relevance-classifier.h5"))

