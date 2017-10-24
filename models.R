library(magrittr)
log_filename <- paste0("caret-", format(Sys.time(), "%Y%m%d%H%M%S"), ".log")
ts <- function() {
  return(format(Sys.time(), "%Y-%m-%d %I:%M%p %Z", tz = "America/Los_Angeles"))
}
log_message <- function(msg, log) {
  message(msg)
  readr::write_lines(msg, log, append = TRUE)
}

split_path <- file.path("data", "per_question.rds")
if (file.exists(split_path)) {
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
      prop_unsure = sum(choice == "unsure") / (sum(choice %in% c("yes", "no", "unsure")) + 1) - 0.5,
      engagement = sum(choice %in% c("yes", "no", "unsure", "dismiss") / times_asked) - 0.5
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(questions, by = "question_id") %>%
    dplyr::left_join(pages, by = "page_id") %>%
    dplyr::left_join(queries, by = "query_id") %>%
    dplyr::inner_join(scores, by = c("query_id", "page_id")) %>%
    dplyr::rename(discernatron_score = score)
  # dplyr::mutate(reliable = as.integer(reliable))

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
      )),
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
    ))) %>%
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
      )),
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
        # use 80% of ^THAT 80% will be used as training data for base learners at level 1:
        training_lvl1 <- sample(training_idx, training_proportion * length(training_idx), replace = FALSE)
        # use remaining 20% of the training data for a super-learner at level 2:
        training_lvl2 <- setdiff(training_idx, training_lvl1)
        return(list(lvl1 = x$obs_id[training_lvl1], lvl2 = x$obs_id[training_lvl2]))
      })
      df$set <- "test"
      df$set[df$obs_id %in% unlist(lapply(training_idxs, function(x) { return(x$lvl1) }))] <- "train1"
      df$set[df$obs_id %in% unlist(lapply(training_idxs, function(x) { return(x$lvl2) }))] <- "train2"
      df$obs_id <- NULL
      return(df)
    })
  readr::write_rds(per_question, split_path, compress = "gz")
  log_message(glue::glue("Update at {ts()}: Split data saved to {split_path}"), log_filename)
}

# Sets of features:
log_message(glue::glue("Update at {ts()}: Making features sets"), log_filename)
features <- list(
  `survey-only` = c(
    "user_score", "prop_unsure", "engagement"
  ),
  `survey & page info` = c(
    "user_score", "prop_unsure", "engagement",
    "page_size", "is_category", "is_talk", "is_file", "is_list", "has_prefix"
  ),
  `survey & pageviews` = c(
    "user_score", "prop_unsure", "engagement",
    "traffic"
  ),
  `survey, page info, and pageviews` = c(
    "user_score", "prop_unsure", "engagement",
    "page_size", "is_category", "is_talk", "is_file", "is_list", "has_prefix",
    "traffic"
  ),
  `survey, page info, and pageviews-by-weekday` = c(
    "user_score", "prop_unsure", "engagement",
    "page_size", "is_category", "is_talk", "is_file", "is_list", "has_prefix",
    "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"
  ),
  `survey, page info, and pageviews-by-platform` = c(
    "user_score", "prop_unsure", "engagement",
    "page_size", "is_category", "is_talk", "is_file", "is_list", "has_prefix",
    "desktop", "mobile-app", "mobile-web"
  ),
  `survey, page info, pageviews-by-weekday, and pageviews-by-platform` = c(
    "user_score", "prop_unsure", "engagement",
    "page_size", "is_category", "is_talk", "is_file", "is_list", "has_prefix",
    "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday",
    "desktop", "mobile-app", "mobile-web"
  ),
  `survey, page info, and pageviews-by-platform-and-weekday` = c(
    "user_score", "prop_unsure", "engagement",
    "page_size", "is_category", "is_talk", "is_file", "is_list", "has_prefix",
    "desktop:friday", "desktop:monday", "desktop:saturday", "desktop:sunday", "desktop:thursday", "desktop:tuesday", "desktop:wednesday",
    "mobile:friday", "mobile:monday", "mobile:saturday", "mobile:sunday", "mobile:thursday", "mobile:tuesday", "mobile:wednesday"
  ),
  `survey, z-norm page info, and z-norm pageviews` = c(
    "user_score", "prop_unsure", "engagement",
    "is_category", "is_talk", "is_file", "is_list", "has_prefix",
    "z_loglength", "znorm_lviews"
  ),
  `survey, normalized page info, and normalized pageviews` = c(
    "user_score", "prop_unsure", "engagement",
    "is_category", "is_talk", "is_file", "is_list", "has_prefix",
    "scaled_loglength", "scaled_lviews"
  )
)

# lapply(features, function(feats) all(feats %in% names(augmented_aggregates)))

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
    size = seq(1, 15, 2),
    decay = c(1e-4, 1e-3, 1e-2, 1e-1)
  )
)

library(caret)

cv_fit <- function(method, covars, data) {
  model_control <- trainControl(
    # 5-fold cross-validation repeated twice:
    method = "repeatedcv", number = 5, repeats = 2,
    # Up-sample to correct for class imbalance:
    sampling = "up", summaryFunction = caret::multiClassSummary,
    # Return predicted probabilities and track progress:
    classProbs = TRUE, verboseIter = TRUE, allowParallel = FALSE
  )
  if (method %in% c("multinom", "nnet")) {
    model <- train(
      Class ~ ., data = data[, c("Class", covars)],
      trControl = model_control, na.action = na.omit,
      method = method, tuneGrid = meta_params[[method]],
      trace = FALSE # suppress nnet optimizatin info
    )
  } else {
    model <- train(
      Class ~ ., data = data[, c("Class", covars)],
      trControl = model_control, na.action = na.omit,
      method = method, tuneGrid = meta_params[[method]]
    )
  }
  return(model)
}

if (!dir.exists("models")) dir.create("models")
index_path <- file.path("models", "model-index.csv")
if (file.exists(index_path)) {
  log_message(glue::glue("Update at {ts()}: Loading index of cached models from {index_path}"), log_filename)
  # Used for checking if any given combo was already done:
  model_index <- readr::read_csv(index_path, col_types = "icclcc")
  # Reason: sometimes the process gets killed for no reason, so rather than having to start
  # all over each time, this is used to continue.
  log_message(glue::glue("Update at {ts()}: Resuming tuning & training process"), log_filename)
} else {
  log_message(glue::glue("Update at {ts()}: Starting tuning & training process"), log_filename)
  model_index <- NULL
}
models <- names(meta_params); names(models) <- models; i <- 0
total_models <- prod(c(
  classes = 3, reliabily_states = 2,
  questions = length(per_question),
  feature_sets = length(features),
  base_learners = length(models)
))
for (k in c(2, 3, 5)) {
  for (reliability in c(TRUE, FALSE)) {
    for (q in names(per_question)) {
      for (f in names(features)) {
        # debug: k=2;reliability=T;q=names(per_question)[1];f=names(features)[1]
        # Check if the combo has already been done & cached:
        if (!is.null(model_index)) {
          if (any(
            model_index$classes == k &
            model_index$question == q &
            model_index$features == f &
            model_index$discernatron_reliable == reliability
          )) {
            next
          }
        }
        # First Level
        base_learners <- lapply(models, function(model) {
          feats <- paste0(" - ", paste0(features[[f]], collapse = "\n - "))
          progression <- sprintf("%.4f%%", 100 * i / total_models); i <<- i + 1
          log_message(
            glue::glue("Update at {ts()} ({progression}):\nTuning & training a base learner '{model}' to predict {k} {ifelse(reliability, 'reliably', 'unreliably')}-determined relevance labels on data from question \"{q}\" with the following features:\n{feats}\n\n"),
            log_filename
          )
          base_data <- dplyr::filter(per_question[[q]], set == "train1", reliable == reliability) %>%
            dplyr::rename_(.dots = list("Class" = paste0("relevance", k)))
          return(cv_fit(model, features[[f]], base_data))
        })
        # Second Level
        new_data <- dplyr::filter(per_question[[q]], set == "train2") %>%
          dplyr::rename_(.dots = list("Class" = paste0("relevance", k)))
        predicted_classes <- as.data.frame(lapply(base_learners, function(base_learner) {
          predictions <- predict(base_learner, new_data[, features[[f]]])
          return(predictions)
        }))
        meta_data <- cbind(Class = new_data$Class, predicted_classes)
        # The super learner is a Bayesian network classifier using Naive Bayes
        meta_learner <- bnclassify::bnc("nb", "Class", meta_data, smooth = 0.3)
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
            classes = k, question = q, features = f,
            discernatron_reliable = reliability,
            base_learners = base_cache,
            meta_learner = meta_cache
          ),
          append = file.exists(file.path("models", "model-index.csv")),
          path = file.path("models", "model-index.csv")
        )
        rm(base_learners, meta_learner)
      }
    }
  }
}; log_message(glue::glue("Update at {ts()}\nDone tuning & training."), log_filename)

# all methods: http://topepo.github.io/caret/available-models.html
# can't use the methods listed at http://topepo.github.io/caret/train-models-by-tag.html#two-class-only
