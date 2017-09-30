library(magrittr)

questions <- data.frame(question_id = as.character(1:4), question = c(
  "If you searched for '...', would this article be a good result?",
  "Would you click on this page when searching for '...'?",
  "If someone searched for '...', would they want to read this article?",
  "If you searched for '...', would this article be relevant?"
), stringsAsFactors = FALSE)

scores <- readr::read_tsv("../data/discernatron_scores.tsv", col_types = "cccdi")
scores %<>%
  dplyr::mutate(
    relevance2 = factor(dplyr::case_when(
      score > 1 ~ "good",
      TRUE ~ "bad"
    ), c("bad", "good")),
    relevance3 = factor(dplyr::case_when(
      score < 1 ~ "bad",
      score < 2 ~ "okay",
      TRUE ~ "good"
    ), c("bad", "okay", "good")),
    relevance5 = factor(dplyr::case_when(
      score == 0 ~ "worst",
      score < 1 ~ "worse",
      score < 2 ~ "okay",
      score < 3 ~ "better",
      score == 3 ~ "best"
    ), c("worst", "worse", "okay", "better", "best"))
  ) %>%
  dplyr::rename(discernatron_score = score)

responses <- readr::read_tsv("../data/survey_responses.tsv.gz", col_types = "DccTiccc") %>%
  dplyr::left_join(questions, by = "question_id")

aggregates <- responses %>%
  dplyr::filter(survey_id == 1) %>%
  dplyr::group_by(question_id, query_id, page_id) %>%
  dplyr::summarize(
    times_asked = n(),
    user_score = (sum(choice == "yes") - sum(choice == "no") + 0.5 * sum(choice == "unsure") + 1) / (sum(choice %in% c("yes", "no", "unsure")) + 1),
    engagement = sum(choice %in% c("yes", "no", "unsure", "dismiss") / times_asked),
  ) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(scores, by = c("query_id", "page_id")) %>%
  dplyr::left_join(questions, by = "question_id")

set.seed(42)
training_proportion <- 0.8
per_question <- aggregates %>%
  dplyr::filter(times_asked > 1) %>%
  { split(., .$question) } %>%
  lapply(function(df) {
    df$obs_id <- 1:nrow(df)
    per_class <- split(df, df$relevance5)
    training_idxs <- unlist(lapply(per_class, function(x) {
      training_idx <- sample.int(nrow(x), training_proportion * nrow(x), replace = FALSE)
      return(x$obs_id[training_idx])
    }))
    df$set <- "test"
    df$set[df$obs_id %in% training_idxs] <- "train"
    df$obs_id <- NULL
    return(df)
  })

rm(responses, aggregates, questions)

# following instructions from http://topepo.github.io/caret/parallel-processing.html
library(doMC)
registerDoMC(cores = 4)
# install.packages(c("caret", "adabag", "MLmetrics")) # adabag for "AdaBoost.M1"
# upsampled_train <- caret::upSample(imbalanced_train, imbalanced_train$Class)
model_control <- caret::trainControl(
  # 10-fold cross-validation repeated 10 times:
  method = "repeatedcv", number = 10, repeats = 10,
  # Up-sample to correct for class imbalance:
  sampling = "up", summaryFunction = multiClassSummary,
  # Return predicted class probabilities when making predictions:
  classProbs = TRUE
)

# XGBoost
xgb_grid <- expand.grid(
  # tuning parameters to optimize:
  max_depth = c(3, 6, 12, 24),
  eta = c(0.1, 0.2, 0.3, 0.4),
  nrounds = c(50, 100, 150, 200),
  subsample = c(0.75, 0.9),
  # tuning parameters to hold constant:
  gamma = 0, min_child_weight = 1, colsample_bytree = 1
)
xgb_model <- caret::train(
  Class ~ user_score,
  data = dplyr::rename(dplyr::filter(per_question[[1]], set == "train"), Class = relevance5),
  trControl = model_control, method = "xgbTree", tuneGrid = xgb_grid, seeds = c(0, 42, 9001, 2015)
)
# all methods: http://topepo.github.io/caret/available-models.html
# can't use the methods listed at http://topepo.github.io/caret/train-models-by-tag.html#two-class-only
# The final values used for the model were:
#   nrounds = 200, max_depth = 3, eta = 0.3, gamma = 0, colsample_bytree = 1,
#   min_child_weight = 1, and subsample = 0.9

rf_grid <- expand.grid(mtry = 1)
rf_model <- caret::train(
  Class ~ user_score,
  data = dplyr::rename(dplyr::filter(per_question[[1]], set == "train"), Class = relevance5),
  trControl = model_control, method = "rf", tuneGrid = rf_grid
)
