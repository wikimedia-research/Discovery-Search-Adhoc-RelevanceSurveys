source("data.R")

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
  dplyr::rename(discernatron_score = score) %>%
  dplyr::mutate(reliable = as.integer(reliable))

# Create pageview-based features:
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

# par(mfrow = c(2, 2))
# hist(overall_traffic$znorm_views, main = "Centered & scaled pageviews")
# hist(overall_traffic$znorm_lviews, main = "Centered & scaled log10(pageviews)")
# hist(overall_traffic$scaled_views, main = "Normalized pageviews")
# hist(overall_traffic$scaled_lviews, main = "Normalized log10(pageviews)")
# par(mfrow = c(1, 1))

augmented_aggregates <- aggregates %>%
  dplyr::left_join(platform_wday_traffic, by = "page_id") %>%
  dplyr::left_join(platform_traffic, by = "page_id") %>%
  dplyr::left_join(wday_traffic, by = "page_id") %>%
  dplyr::left_join(overall_traffic, by = "page_id")

rm(responses, pageviews, aggregates, pages, scores, questions, queries, base_dir)
rm(platform_wday_traffic, platform_traffic, wday_traffic, overall_traffic)

set.seed(42)
training_proportion <- 0.8
per_question <- augmented_aggregates %>%
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

# Sets of features:
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
  )
)

# lapply(features, function(feats) all(feats %in% names(augmented_aggregates)))

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
    mtry = c(2, 3, 4, 5)
  ),
  "nnet" = expand.grid(
    size = seq(1, 15, 2),
    decay = c(0, 1e-4, 1e-3, 1e-2, 1e-1)
  )
)

cv_fit <- function(method, covars, data, folds = 10, reps = 5) {
  # following instructions from http://topepo.github.io/caret/parallel-processing.html
  # library(doMC); registerDoMC(cores = 4)
  # install.packages(c("caret", "MLmetrics", "e1071", "xgboost"))
  # upsampled_train <- caret::upSample(imbalanced_train, imbalanced_train$Class)
  model_control <- caret::trainControl(
    # 10-fold cross-validation repeated twice:
    method = "repeatedcv", number = folds, repeats = reps,
    # Up-sample to correct for class imbalance:
    sampling = "up", summaryFunction = caret::multiClassSummary,
    # Return predicted probabilities and track progress:
    classProbs = TRUE, verboseIter = TRUE, allowParallel = FALSE
  )
  model <- caret::train(
    Class ~ ., data = data[, c("Class", covars)],
    trControl = model_control, na.action = na.omit,
    method = method, tuneGrid = meta_params[[method]],
    trace = FALSE # suppress nnet optimizati info
  )
  return(model)
}

log_filename <- paste0("caret-", format(Sys.time(), "%Y%m%d%H%M%S"), ".log")
ts <- function() format(Sys.time(), "%Y-%m-%d %I:%M%p")
models <- lapply(c("2 classes" = 2, "3 classes" = 3, "5 classes" = 5), function(k) {
  lapply(per_question, function(question) {
    lapply(features, function(covars) {
      feats <- paste0("- ", paste0(covars, collapse = "\n- "))
      methods <- names(meta_params)
      names(methods) <- methods
      lapply(methods, function(method) {
        msg <- glue::glue("# Update at {ts()}\nTuning & training a model to predict {k} classes using {method} on data with the following features:\n{feats}\n\n")
        message(msg); readr::write_lines(msg, log_filename, append = TRUE)
        question %>%
          dplyr::filter(set == "train") %>%
          dplyr::rename_(.dots = list("Class" = paste0("relevance", k))) %>%
          cv_fit(method, covars, .)
      })
    })
  })
})
readr::write_lines(
  glue::glue("# Update at {ts()}\nFinished tuning & training. Saving results to disk..."),
  log_filename, append = TRUE
)
save(
  list = c("models", "features"),
  file = file.path(base_dir, "tuned_models.RData"),
  compress = "gzip"
)
readr::write_lines("Done.", log_filename, append = TRUE)

# all methods: http://topepo.github.io/caret/available-models.html
# can't use the methods listed at http://topepo.github.io/caret/train-models-by-tag.html#two-class-only
