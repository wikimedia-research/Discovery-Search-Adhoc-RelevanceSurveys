library(magrittr)
suppressPackageStartupMessages(suppressMessages(suppressWarnings({
  model_index <- readr::read_csv(file.path("models", "model-index.csv"))
  per_question <- readr::read_rds(file.path("data", "per_question.rds"))
})))
source("features.R")

model_accuracy <- cbind(model_index[, 3:6], xgbTree = 0.0, C5.0 = 0.0, nb = 0.0, multinom = 0.0, rf = 0.0, nnet = 0.0, meta = 0.0, dnn = 0.0)
pb <- progress::progress_bar$new(total = nrow(model_index))
for (i in 1:nrow(model_index)) {
  suppressPackageStartupMessages(suppressMessages(suppressWarnings({
      base_learners <- readr::read_rds(model_index$base_learners[i])
      meta_learner <- readr::read_rds(model_index$meta_learner[i])
  })))
  new_data <- dplyr::filter(per_question[[model_index$question[i]]], set == "test") %>%
    dplyr::rename_(.dots = list("Class" = paste0("relevance", model_index$classes[i])))
  base_predictions <- as.data.frame(lapply(base_learners, function(base_learner) {
    predictions <- predict(base_learner, new_data[, features[[model_index$features[i]]]])
    return(predictions)
  }))
  meta_prediction <- bnclassify:::predict.bnc_bn(meta_learner, base_predictions, prob = FALSE)
  predicted_classes <- cbind(base_predictions, meta = meta_prediction)
  accuracies <- predicted_classes %>%
    purrr::map_df(~ caret::confusionMatrix(data = .x, reference = new_data$Class)$overall["Accuracy"])
  model_accuracy[i, 5:ncol(model_accuracy)] <- accuracies
  pb$tick()
}; rm(
  base_learners, meta_learner, new_data,
  base_predictions, meta_prediction,
  predicted_classes, accuracies, pb
)
model_accuracy <- tidyr::gather(
  model_accuracy, "classifier", "accuracy",
  -c(classes, question, features, discernatron_reliable)
)
readr::write_tsv(model_accuracy, file.path("models", "model-accuracy.tsv"))
