# https://phabricator.wikimedia.org/P5957
# discernatron <- readLines("https://phabricator.wikimedia.org/paste/raw/5957/") %>%
#   paste0(collapse = "") %>%
#   jsonlite::fromJSON(simplifyVector = FALSE) %>%
#   purrr::map_dfr(~ dplyr::data_frame(
#     query = names(.x$scores),
#     page_title = .x$title,
#     score = as.numeric(unlist(.x$scores)),
#     daily_pvs = as.integer(ceiling(.x$daily_views))
#   ), .id = "page_id")

# https://discernatron.wmflabs.org/scores/all?json=1
raw_scores <- jsonlite::fromJSON(readLines("data/all.json"), simplifyVector = FALSE)
scores <- purrr::map_df(raw_scores$scores, ~ dplyr::data_frame(
  query = .x$query,
  page_title = .x$title,
  score = as.numeric(.x$score),
  reliable = .x$reliable == "1",
  n_scores = as.numeric(.x$num_scores)
))

pages <- readr::read_tsv("data/page_info.tsv")
queries <- readr::read_tsv("data/search_queries.tsv")

scores <- dplyr::left_join(scores, queries, by = "query")
scores <- dplyr::left_join(scores, pages, by = c("page_title" = "page_title_db"))
scores <- dplyr::filter(scores, !is.na(query_id), !is.na(page_id))
scores <- scores[, c("query_id", "page_id", "score", "reliable", "n_scores")]

readr::write_tsv(scores, "data/discernatron_scores.tsv")
