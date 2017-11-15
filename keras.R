library(keras)

per_question <- readr::read_rds(file.path("data", "per_question.rds"))
source("features.R")

accuracies <- expand.grid(
  discernatron_reliable = c(TRUE, FALSE),
  question = names(per_question),
  features = names(features),
  accuracy = 0.0,
  stringsAsFactors = FALSE
)

for (reliability in c(TRUE, FALSE)) {
  for (q in names(per_question)) {
    for (f in names(features)) {

      # Data prep:
      temp_data <- dplyr::filter(per_question[[q]], reliable == reliability) %>%
        dplyr::rename(Class = relevance2) %>%
        dplyr::mutate(Class = as.numeric(factor(Class, c("bad", "good"))) - 1) %>%
        { .[, c("Class", "set", features[[f]])] }
      safe_feature_names <- features[[f]] %>%
        gsub("-", "_", ., fixed = TRUE) %>%
        gsub(":", ".", ., fixed = TRUE)
      names(temp_data) <- c("Class", "set", safe_feature_names)
      x_train <- dplyr::select(dplyr::filter(temp_data, set == "train"), -c(Class, set))
      y_train <- matrix(temp_data$Class[temp_data$set == "train"], ncol = 1)
      x_test <- dplyr::select(dplyr::filter(temp_data, set == "test"), -c(Class, set))
      y_test <- matrix(temp_data$Class[temp_data$set == "test"], ncol = 1)

      # Deal with factor predictors:
      factors <- vapply(temp_data[, safe_feature_names], is.factor, TRUE) %>% { names(.)[.] }
      if (length(factors) > 0) {
        x_train <- as.formula(paste0("~ ", paste0(factors, collapse = " + "))) %>%
          model.matrix(x_train) %>%
          cbind(x_train[, setdiff(names(x_train), factors)]) %>%
          as.matrix
        x_test <- as.formula(paste0("~ ", paste0(factors, collapse = " + "))) %>%
          model.matrix(x_test) %>%
          cbind(x_test[, setdiff(names(x_test), factors)]) %>%
          as.matrix
      } else {
        x_train <- as.matrix(x_train)
        x_test <- as.matrix(x_test)
      }

      # Shuffle training data because validation (dev) set is the last 20%:
      shuffled_idx <- sample(1:nrow(x_train), nrow(x_train), replace = FALSE)
      x_train <- x_train[shuffled_idx, ]
      y_train <- y_train[shuffled_idx, ]

      # Construct and train a Sequential model:
      model <- keras_model_sequential()
      model %>%
        layer_dense(units = 256, input_shape = ncol(x_train)) %>%
        layer_activation('relu') %>%
        layer_dropout(0.5) %>%
        layer_dense(units = 128) %>%
        layer_activation('relu') %>%
        layer_dropout(0.2) %>%
        layer_dense(units = 64) %>%
        layer_activation('relu') %>%
        layer_dropout(0.1) %>%
        layer_dense(units = 1) %>%
        layer_activation('sigmoid')
      model %>% compile(
        loss = 'binary_crossentropy',
        optimizer = optimizer_rmsprop(lr = 0.01),
        metrics = c('binary_accuracy')
      )
      history <- model %>% fit(
        x_train, y_train,
        epochs = 30, batch_size = 128,
        validation_split = 0.2,
        class_weight = list("0" = 0.4, "1" = 0.6),
        verbose = 2
      )
      plot(history)

      # Save model accuracy:
      idx <- which(
        accuracies$discernatron_reliable == reliability &
          accuracies$question == q & accuracies$features == f
      )
      accuracies[idx, "accuracy"] <- evaluate(model, x_test, y_test)$binary_accuracy

      # TODO: save weights & architectures

    }
  }
}; rm(model, safe_feature_names, shuffled_idx, x_train, y_train, x_test, y_test, history, idx)

readr::write_csv(accuracies, file.path("models", "keras-accuracies.csv"))
