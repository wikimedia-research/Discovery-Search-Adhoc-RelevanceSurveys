base_dir <- ifelse(dir.exists("data"), "data", "../data")

questions <- data.frame(question_id = 1L:4L, question = c(
  "If you searched for '...', would this article be a good result?",
  "Would you click on this page when searching for '...'?",
  "If someone searched for '...', would they want to read this article?",
  "If you searched for '...', would this article be relevant?"
), stringsAsFactors = FALSE)

queries <- readr::read_tsv(file.path(base_dir, "search_queries.tsv"), col_types = "ic")
pages <- readr::read_tsv(file.path(base_dir, "page_info.tsv"), col_types = "icci") %>%
  dplyr::mutate(
    is_category = as.integer(grepl("^Category:", page_title_api)),
    is_talk = as.integer(grepl("^(Talk|Wikipedia talk):", page_title_api)),
    is_file = as.integer(grepl("^File:", page_title_api)),
    has_prefix = as.integer(grepl("^[A-Za-z\\s]+:[A-Z0-9]", page_title_api)),
    is_list = as.integer(grepl("^List of", page_title_api)),
    loglength = round(log10(page_length), 4),
    z_loglength = round((loglength - mean(loglength)) / sd(loglength), 4),
    scaled_loglength = round((loglength / max(loglength)) - 0.5, 4),
    page_size = factor(dplyr::case_when(
      page_length < 1e3 ~ "tiny",
      page_length < 1e4 ~ "small",
      page_length < 5e4 ~ "medium",
      page_length < 1e5 ~ "large",
      TRUE ~ "huge"
    ))
  )

scores <- readr::read_tsv(file.path(base_dir, "discernatron_scores.tsv"), col_types = "iidli")
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
  )

responses <- readr::read_tsv(file.path(base_dir, "survey_responses.tsv.gz"), col_types = "DicTiiic")

pageviews <- readr::read_tsv(file.path(base_dir, "daily_pageviews.tsv.gz"), col_types = "Dici")
