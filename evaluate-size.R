library(magrittr)
suppressPackageStartupMessages(suppressMessages(suppressWarnings({
  keras_index_1 <- readr::read_csv(file.path("models", "keras-index-legacy.csv")) %>%
    dplyr::select(-accuracy) %>%
    dplyr::mutate(learner = sub("\\", "/", learner, fixed = TRUE)) %>%
    dplyr::rename(`keras-1` = learner)
  keras_index_2 <- readr::read_csv(file.path("models", "keras-index.csv")) %>%
    dplyr::select(-accuracy) %>%
    dplyr::mutate(learner = sub("\\", "/", learner, fixed = TRUE)) %>%
    dplyr::rename(`keras-2` = learner)
  model_index <- readr::read_csv(file.path("models", "model-index.csv")) %>%
    dplyr::select(-c(timestamp, last_i)) %>%
    dplyr::left_join(keras_index_1, by = c("question", "features", "discernatron_reliable")) %>%
    dplyr::left_join(keras_index_2, by = c("question", "features", "discernatron_reliable")) %>%
    dplyr::filter(features == "survey-only")
  per_question <- readr::read_rds(file.path("data", "per_question.rds"))
  per_question %<>% lapply(function(df) {
    df %>%
      dplyr::filter(set == "test") %>%
      dplyr::mutate(responses = ceiling(times_asked * engagement))
  })
  rm(keras_index_1, keras_index_2)
})))
source("features.R")

predictions <- list()
pb <- progress::progress_bar$new(total = nrow(model_index))
for (i in 1:nrow(model_index)) {
  suppressPackageStartupMessages(suppressMessages(suppressWarnings({
    base_learners <- readr::read_rds(model_index$base_learners[i])
    meta_learner <- readr::read_rds(model_index$meta_learner[i])
  })))
  actual_classes <- per_question[[model_index$question[i]]]$Class
  new_data <- per_question[[model_index$question[i]]][, features[[model_index$features[i]]]]
  base_predictions <- as.data.frame(lapply(base_learners, predict, newdata = new_data))
  meta_prediction <- bnclassify:::predict.bnc_bn(meta_learner, base_predictions, prob = FALSE)
  keras_1_model <- keras::load_model_hdf5(model_index$`keras-1`[i])
  keras_1_preds <- keras::predict_classes(keras_1_model, as.matrix(new_data)) %>%
    as.numeric %>%
    factor(0:1, c("irrelevant", "relevant"))
  keras_2_model <- keras::load_model_hdf5(model_index$`keras-2`[i])
  keras_2_preds <- keras::predict_classes(keras_2_model, as.matrix(new_data)) %>%
    as.numeric %>%
    factor(0:1, c("irrelevant", "relevant"))
  predicted_classes <- cbind(
    as.data.frame(lapply(base_predictions, function(preds) { preds == actual_classes})),
    meta = meta_prediction == actual_classes,
    "keras-1" = keras_1_preds == actual_classes,
    "keras-2" = keras_2_preds == actual_classes
  )
  predictions[[i]] <- cbind(
    per_question[[model_index$question[i]]][, c("question", "reliable", "times_asked", "responses", "prop_unsure")],
    predicted_classes
  )
  pb$tick()
  rm(base_learners, meta_learner, new_data, base_predictions, meta_prediction)
  rm(keras_1_model, keras_1_preds, keras_2_model, keras_2_preds, predicted_classes)
}; rm(i, pb)
predictions <- do.call(rbind, predictions)

readr::write_csv(predictions, file.path("models", "per-page-accuracy.csv"))
