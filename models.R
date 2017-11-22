# Setup:
.DEBUG <- FALSE
library(magrittr)
log_filename <- paste0("caret-", format(Sys.time(), "%Y%m%d%H%M%S"), ".log")
ts <- function() {
  return(format(Sys.time(), "%Y-%m-%d %I:%M%p %Z", tz = "America/Los_Angeles"))
}
log_message <- function(msg, log) {
  message(msg)
  if (!.DEBUG) readr::write_lines(msg, log, append = TRUE)
}

split_path <- file.path("data", "per_question.rds")
if (file.exists(split_path) && !.DEBUG) {
  log_message(glue::glue("Update at {ts()}: Loading already split data from {split_path}"), log_filename)
  per_question <- readr::read_rds(split_path)
} else {
  log_message(glue::glue("Update at {ts()}: Loading data"), log_filename)
  source("data.R")
  log_message(glue::glue("Update at {ts()}: Additional data processing"), log_filename)

  aggregates <- responses %>%
    dplyr::filter(survey_id == 1) %>%
    dplyr::group_by(question_id, query_id, page_id) %>%
    dplyr::summarize(
      times_asked = n(),
      user_score = (sum(choice == "yes") - sum(choice == "no")) / (sum(choice %in% c("yes", "no")) + 1),
      prop_unsure = sum(choice == "unsure") / (sum(choice %in% c("yes", "no", "unsure")) + 1),
      engagement = sum(choice %in% c("yes", "no", "unsure", "dismiss") / times_asked)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(questions, by = "question_id") %>%
    dplyr::left_join(pages, by = "page_id") %>%
    dplyr::left_join(queries, by = "query_id") %>%
    dplyr::inner_join(scores, by = c("query_id", "page_id")) %>%
    dplyr::rename(discernatron_score = score)

  # Create pageview-based features:
  log_message(glue::glue("Update at {ts()}: Creating pageview-based features"), log_filename)
  platform_wday_traffic <- pageviews %>%
    dplyr::mutate(platform = dplyr::if_else(platform == "desktop", "desktop", "mobile")) %>%
    dplyr::group_by(page_id, platform, date) %>%
    dplyr::summarize(views = sum(views)) %>%
    dplyr::mutate(wday = tolower(lubridate::wday(date, label = TRUE, abbr = FALSE))) %>%
    dplyr::group_by(page_id, platform, wday) %>%
    dplyr::summarize(views = median(views)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      traffic = factor(dplyr::case_when(
        views <= 1 ~ "no traffic",
        views <= 10 ~ "low traffic",
        views <= 100 ~ "medium traffic",
        views <= 1000 ~ "high traffic",
        TRUE ~ "very high traffic"
      ), c("no traffic", "low traffic", "medium traffic", "high traffic", "very high traffic")),
      platform_wday = paste0(platform, ":", wday)
    ) %>%
    dplyr::select(-c(views, platform, wday)) %>%
    tidyr::spread(platform_wday, traffic, fill = "no traffic")
  platform_traffic <- pageviews %>%
    dplyr::group_by(page_id, platform) %>%
    dplyr::summarize(views = median(views)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(traffic = factor(dplyr::case_when(
      views <= 1 ~ "no traffic",
      views <= 10 ~ "low traffic",
      views <= 100 ~ "medium traffic",
      views <= 1000 ~ "high traffic",
      TRUE ~ "very high traffic"
    ))) %>%
    dplyr::select(-views) %>%
    tidyr::spread(platform, traffic, fill = "no traffic")
  wday_traffic <- pageviews %>%
    dplyr::group_by(page_id, date) %>%
    dplyr::summarize(views = sum(views)) %>%
    dplyr::mutate(wday = tolower(lubridate::wday(date, label = TRUE, abbr = FALSE))) %>%
    dplyr::group_by(page_id, wday) %>%
    dplyr::summarize(views = median(views)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(traffic = factor(dplyr::case_when(
      views <= 1 ~ "no traffic",
      views <= 10 ~ "low traffic",
      views <= 100 ~ "medium traffic",
      views <= 1000 ~ "high traffic",
      TRUE ~ "very high traffic"
    ), c("no traffic", "low traffic", "medium traffic", "high traffic", "very high traffic"))) %>%
    dplyr::select(-views) %>%
    tidyr::spread(wday, traffic, fill = "no traffic")
  overall_traffic <- pageviews %>%
    dplyr::group_by(page_id, date) %>%
    dplyr::summarize(views = sum(views)) %>%
    dplyr::summarize(views = median(views)) %>%
    dplyr::mutate(
      traffic = factor(dplyr::case_when(
        views <= 1 ~ "no",
        views <= 10 ~ "low",
        views <= 100 ~ "medium",
        views <= 1000 ~ "high",
        TRUE ~ "very high"
      ), c("no", "low", "medium", "high", "very high")),
      lviews = log10(views + 1),
      znorm_views = round((views - mean(views)) / sd(views), 4),
      znorm_lviews = round((lviews - mean(lviews)) / sd(lviews), 4),
      scaled_views = round((views / max(views)) - 0.5, 4),
      scaled_lviews = round((lviews / max(lviews)) - 0.5, 4),
      lviews = round(lviews, 4)
    )

  augmented_aggregates <- aggregates %>%
    dplyr::left_join(platform_wday_traffic, by = "page_id") %>%
    dplyr::left_join(platform_traffic, by = "page_id") %>%
    dplyr::left_join(wday_traffic, by = "page_id") %>%
    dplyr::left_join(overall_traffic, by = "page_id")

  rm(responses, pageviews, aggregates, pages, scores, questions, queries)
  rm(platform_wday_traffic, platform_traffic, wday_traffic, overall_traffic)
  log_message(glue::glue("Update at {ts()}: Splitting up data into training & testing sets"), log_filename)
  set.seed(42)
  training_proportion <- 0.8
  per_question <- augmented_aggregates %>%
    dplyr::filter(times_asked > 1) %>%
    { split(., .$question) } %>%
    lapply(function(df) {
      df$obs_id <- 1:nrow(df)
      per_reliability <- split(df, df$reliable)
      training_idxs <- lapply(per_reliability, function(x) {
        # use 80% of total data for training (remaining 20% will be used for evaluation):
        training_idx <- sample.int(nrow(x), training_proportion * nrow(x), replace = FALSE)
        return(x$obs_id[training_idx])
      })
      df$set <- "test"
      df$set[df$obs_id %in% unlist(training_idxs)] <- "train"
      df$obs_id <- NULL
      return(df)
    })
  readr::write_rds(per_question, split_path, compress = "gz")
  log_message(glue::glue("Update at {ts()}: Split data saved to {split_path}"), log_filename)
}

# Sets of features:
log_message(glue::glue("Update at {ts()}: Constructing features sets & checking for validity"), log_filename)
source("features.R")
if (!all(unlist(lapply(features, function(feats) all(feats %in% names(per_question[[1]])))))) {
  log_message(glue::glue("Error at {ts()}: Feature set misspecification, cannot proceed"), log_filename)
  stop(
    "data does not have the following feature(s): ",
    paste0(unlist(lapply(features, function(feats) feats[!feats %in% names(per_question[[1]])])), collapse = ", ")
  )
}

log_message(glue::glue("Update at {ts()}: Making parameter grids for tuning"), log_filename)
meta_params <- list(
  "xgbTree" = expand.grid(
    # XGBoost tuning parameters to optimize:
    max_depth = c(4, 8, 16, 32, 64, 128),
    eta = c(0.1, 0.2, 0.3, 0.4),
    nrounds = c(50, 100, 150, 200),
    subsample = c(0.75, 0.9),
    # XGBoost tuning parameters to hold constant:
    gamma = 0, min_child_weight = 1, colsample_bytree = 1
  ),
  "C5.0" = expand.grid(
    # C5.0
    trials = c(1, 5, 10, 25, 50, 75, 100),
    model = "tree", winnow = TRUE
  ),
  "nb" = expand.grid(
    # Naive Bayes
    fL = c(0, 0.3, 0.5, 0.8, 1, 2),
    adjust = c(1, 1.5, 2),
    usekernel = TRUE # held constant
  ),
  "multinom" = expand.grid(
    # Penalized Multinomial Regression
    decay = c(0, 1e-4, 1e-3, 1e-2, 1e-1, 3e-1, 5e-1, 7e-1)
  ),
  "rf" = expand.grid(
    # Random Forest
    mtry = 2:5
  ),
  "nnet" = expand.grid(
    size = seq(3, 33, 3),
    decay = c(1e-4, 1e-3, 1e-2, 1e-1)
  ),
  "dnn" = expand.grid(
    layer1 = c(24, 32),
    layer2 = c(8, 16),
    layer3 = 4,
    hidden_dropout = c(0, 0.1, 0.2, 0.5),
    visible_dropout = 0
  )
)

library(caret)

cv_fit <- function(method, covars, data) {
  tuning_idx <- if (.DEBUG) 1 else 1:nrow(meta_params[[method]])
  model_control <- trainControl(
    # 5-fold cross-validation:
    method = "repeatedcv", number = ifelse(.DEBUG, 2, 5), repeats = 1,
    # Up-sample to correct for class imbalance:
    sampling = "up", summaryFunction = caret::multiClassSummary,
    # Return predicted probabilities and track progress:
    classProbs = TRUE, verboseIter = TRUE, allowParallel = FALSE
  )
  if (method %in% c("multinom", "nnet")) {
    model <- train(
      Class ~ ., data = data[, c("Class", covars)],
      trControl = model_control, na.action = na.omit,
      method = method, tuneGrid = meta_params[[method]][tuning_idx,, drop = FALSE],
      trace = FALSE # suppress nnet optimizatin info
    )
  } else if (method == "dnn") {
    model <- train(
      Class ~ ., data = data[, c("Class", covars)],
      trControl = model_control, na.action = na.omit,
      method = method, tuneGrid = meta_params[[method]][tuning_idx,, drop = FALSE],
      momentum = 0.9, learningrate = 0.05, numepochs = 150,
      learningrate_scale = 0.95, # learning rate will be mutiplied by this after every iter
      activationfun = "tanh",    # better than sigm & has a centering effect on neurons
      batchsize = 512            # batches of training data that are used to calculate error and update coefficients
    )
  } else {
    model <- train(
      Class ~ ., data = data[, c("Class", covars)],
      trControl = model_control, na.action = na.omit,
      method = method, tuneGrid = meta_params[[method]][tuning_idx,, drop = FALSE]
    )
  }
  return(model)
}

if (!dir.exists("models")) dir.create("models")
index_path <- file.path("models", "model-index.csv")
if (file.exists(index_path)) {
  log_message(glue::glue("Update at {ts()}: Loading index of cached models from {index_path}"), log_filename)
  # Used for checking if any given combo was already done:
  model_index <- readr::read_csv(index_path); i <- max(model_index$last_i)
  # Reason: sometimes the process gets killed for no reason, so rather than having to start
  # all over each time, this is used to continue.
  log_message(glue::glue("Update at {ts()}: Resuming tuning & training process"), log_filename)
} else {
  log_message(glue::glue("Update at {ts()}: Starting tuning & training process"), log_filename)
  model_index <- NULL; i <- 0
}
models <- names(meta_params); names(models) <- models
total_models <- prod(c(
  reliabily_states = 2,
  questions = length(per_question),
  feature_sets = length(features),
  learners = length(models) + 1
))
for (reliability in c(TRUE, FALSE)) {
  for (q in names(per_question)) {
    base_data <- dplyr::filter(per_question[[q]], set == "train", reliable == reliability)
    for (f in names(features)) {
      # debug: reliability=TRUE;q=names(per_question)[1];f=names(features)[2]
      # Check if the combo has already been done & cached:
      if (!is.null(model_index)) {
        if (any(
          model_index$question == q & model_index$features == f &
          model_index$discernatron_reliable == reliability
        )) {
          next
        }
      }
      # First Level
      base_learners <- lapply(models, function(model) {
        # Train each base learner on random 85% of the data:
        feats <- paste0(" - ", paste0(features[[f]], collapse = "\n - "))
        progression <- sprintf("%.4f%%", 100 * i / total_models); i <<- i + 1
        log_message(
          glue::glue("Update at {ts()} ({progression}):\nTuning & training a base learner '{model}' to predict {ifelse(reliability, 'reliably', 'unreliably')}-determined relevance labels on data from question \"{q}\" with the following features:\n{feats}\n\n"),
          log_filename
        )
        set.seed(which(model %in% models)) # ensures same random subset for each combo
        return(cv_fit(model, features[[f]], dplyr::sample_frac(base_data, 0.85)))
      })
      # Second Level
      predicted_classes <- as.data.frame(lapply(base_learners, function(base_learner) {
        predictions <- predict(base_learner, base_data[, features[[f]]])
        return(predictions)
      }))
      meta_data <- cbind(Class = base_data$Class, predicted_classes); i <<- i + 1
      # The super learner is a Bayesian network classifier using Naive Bayes:
      meta_learner <- bnclassify::bnc("nb", "Class", meta_data, smooth = 0.3)
      if (!.DEBUG) {
        # Write base learners and meta learner to disk:
        base_cache <- tempfile("model-cache-base_", "models", ".rds")
        readr::write_rds(base_learners, base_cache, compress = "gz")
        log_message(glue::glue("Base learners saved to {base_cache}"), log_filename)
        meta_cache <- tempfile("model-cache-meta_", "models", ".rds")
        readr::write_rds(meta_learner, meta_cache, compress = "gz")
        log_message(glue::glue("Meta learner saved to {meta_cache}"), log_filename)
        # Write filename pointers (to cached learners) to an index:
        readr::write_csv(
          dplyr::data_frame(
            timestamp = lubridate::now(tzone = "America/Los_Angeles"),
            last_i = i, question = q, features = f,
            discernatron_reliable = reliability,
            base_learners = base_cache, meta_learner = meta_cache
          ),
          append = file.exists(file.path("models", "model-index.csv")),
          path = file.path("models", "model-index.csv")
        )
      }
      rm(base_learners, meta_learner)
    }
  }
}; log_message(glue::glue("Update at {ts()}\nDone tuning & training."), log_filename)

# all methods: http://topepo.github.io/caret/available-models.html
# can't use the methods listed at http://topepo.github.io/caret/train-models-by-tag.html#two-class-only
