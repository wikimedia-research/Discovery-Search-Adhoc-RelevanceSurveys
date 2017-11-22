library(keras)

.DEBUG <- FALSE

per_question <- readr::read_rds(file.path("data", "per_question.rds"))
source("features.R")

index <- file.path("models", "keras-index-legacy.csv")
if (file.exists(index)) {
  learnt <- readr::read_csv(index)
} else {
  learnt <- NULL
}

for (reliability in c(TRUE, FALSE)) {
  for (q in names(per_question)) {
    for (f in names(features)) {
      # debug: reliability=TRUE;q=names(per_question)[1];f=names(features)[3]
      if (!is.null(learnt)) {
        if (any(
          learnt$question == q & learnt$features == f &
          learnt$discernatron_reliable == reliability
        )) {
          next
        }
      }
      # Data prep:
      temp_data <- dplyr::filter(per_question[[q]], reliable == reliability) %>%
        dplyr::mutate(Class = as.numeric(Class) - 1) %>%
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
        x_train <- x_train[, setdiff(colnames(x_train), "(Intercept)")]
        x_test <- as.formula(paste0("~ ", paste0(factors, collapse = " + "))) %>%
          model.matrix(x_test) %>%
          cbind(x_test[, setdiff(names(x_test), factors)]) %>%
          as.matrix
        x_test <- x_test[, setdiff(colnames(x_test), "(Intercept)")]
      } else {
        x_train <- as.matrix(x_train)
        x_test <- as.matrix(x_test)
      }; rm(temp_data)
      # Shuffle before validation split:
      shuffled_idx <- sample.int(nrow(x_train), nrow(x_train), replace = FALSE)
      x_train <- x_train[shuffled_idx, ]; y_train <- y_train[shuffled_idx, , drop = FALSE]

      # Construct and train a Sequential model:
      model <- keras_model_sequential()
      model %>%
        layer_dense(units = 256, activation = 'relu', input_shape = ncol(x_train)) %>%
        layer_dropout(0.5) %>%
        layer_dense(units = 128, activation = 'relu') %>%
        layer_dropout(0.2) %>%
        layer_dense(units = 64, activation = 'relu') %>%
        layer_dropout(0.1) %>%
        layer_dense(units = 1, activation = 'sigmoid')
      model %>% compile(
        loss = 'binary_crossentropy',
        optimizer = optimizer_rmsprop(lr = 0.01),
        metrics = 'accuracy'
      )
      history <- model %>% fit(
        x_train, y_train,
        epochs = 30, batch_size = 128,
        validation_split = 0.2,
        class_weight = list("0" = 0.4, "1" = 0.6),
        verbose = 2
      )
      # plot(history)

      # Save model accuracy, architecture, and weights:
      if (!.DEBUG) {
        cache <- tempfile("keras-cache_", "models", ".h5")
        save_model_hdf5(model, cache)
        readr::write_csv(dplyr::data_frame(
          question = q, features = f, discernatron_reliable = reliability,
          accuracy = round(evaluate(model, x_test, y_test)$acc, 4),
          learner = cache
        ), path = index, append = file.exists(index))
      }

    }
  }
}
