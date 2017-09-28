if (!grepl("^stat1", Sys.info()["nodename"])) {
  message("Creating an auto-closing SSH tunnel in the background...")
  # See https://gist.github.com/scy/6781836 for more info.
  system("ssh -f -o ExitOnForwardFailure=yes stat1006.eqiad.wmnet -L 3307:analytics-store.eqiad.wmnet:3306 sleep 10")
  library(RMySQL)
  con <- dbConnect(MySQL(), host = "127.0.0.1", group = "client", dbname = "log", port = 3307)
} else {
  con <- wmf::mysql_connect("log")
}

library(magrittr)
library(glue)

start_date <- as.Date("2017-09-06") + 1
end_date <- as.Date("2017-09-21")
# end_date <- start_date # for debugging

query <- "SELECT
  query,
  question,
  ts,
  session_id,
  article_id AS page_id,
  pages.page_title AS page_title,
  choice
FROM (
  SELECT
    event_mwSessionId AS session_id,
    timestamp AS ts,
    event_articleId AS article_id,
    event_choice AS choice,
    event_query AS query,
    CASE event_question
      WHEN 'wikimediaevents-humanrel-question-a' THEN \"Would you click on this page when searching for '...'?\"
      WHEN 'wikimediaevents-humanrel-question-b' THEN \"If you searched for '...', would this article be a good result?\"
      WHEN 'wikimediaevents-humanrel-question-c' THEN \"If you searched for '...', would this article be relevant?\"
      WHEN 'wikimediaevents-humanrel-question-d' THEN \"If someone searched for '...', would they want to read this article?\"
    END AS question
  FROM log.HumanSearchRelevance_17073843
  WHERE LEFT(timestamp, 8) = '{yyyymmdd}'
    AND wiki = 'enwiki'
    AND INSTR(userAgent, '\"is_bot\": false') > 0 -- remove possible bots
) AS events
LEFT JOIN enwiki.page AS pages
  ON events.article_id = pages.page_id;"

results <- do.call(rbind, lapply(
  seq(start_date, end_date, by = "day"),
  function(date) {
    message("Fetching data from ", format(date, "%d %B %Y"))
    yyyymmdd <- format(date, "%Y%m%d")
    query <- glue(query)
    result <- wmf::mysql_read(query, "log", con)
    result$ts <- lubridate::ymd_hms(result$ts)
    result$page_id %<>% as.character
    result$page_title %<>% gsub("_", " ", ., fixed = TRUE)
    result %<>%
      dplyr::group_by(session_id) %>%
      dplyr::arrange(ts) %>%
      dplyr::mutate(survey_id = cumsum(!is.na(choice))) %>%
      dplyr::ungroup()
    return(result)
  }
))

wmf::mysql_close(con)

if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

# https://phabricator.wikimedia.org/P5957
discernatron <- readLines("https://phabricator.wikimedia.org/paste/raw/5957/") %>%
  paste0(collapse = "") %>%
  jsonlite::fromJSON(simplifyVector = FALSE) %>%
  purrr::map_dfr(~ dplyr::data_frame(
    query = names(.x$scores),
    page_title = .x$title,
    score = as.numeric(unlist(.x$scores)),
    daily_pvs = ceiling(.x$daily_views)
  ), .id = "page_id")

# Double check for validity:
any(!unique(results$page_id) %in% unique(discernatron$page_id)) # FALSE, good

discernatron_pages <- dplyr::distinct(discernatron, page_id, page_title)
results_pages <- dplyr::distinct(results, page_id, page_title)
pages <- dplyr::left_join(discernatron_pages, results_pages, by = "page_id") %>%
  dplyr::mutate(discrep = page_title.x != page_title.y) %>%
  dplyr::select(page_id, page_title = page_title.y)
discernatron$page_title <- NULL; results$page_title <- NULL
discernatron %<>% dplyr::left_join(pages, by = "page_id")

questions <- unique(results$question); dump("questions", "")

queries <- dplyr::distinct(discernatron, query) %>%
  dplyr::arrange(query) %>%
  dplyr::mutate(query_id = as.numeric(factor(query))) %>%
  dplyr::select(query_id, query)
readr::write_tsv(queries, file.path("data", "search_queries.tsv"))

results %<>% dplyr::left_join(queries, by = "query")
results$query <- NULL
discernatron %<>% dplyr::left_join(queries, by = "query")
discernatron$query <- NULL

discernatron %<>% dplyr::select(c(query_id, page_id, page_title, score, daily_pvs))
readr::write_tsv(discernatron, file.path("data", "discernatron_scores.tsv"))

results$question_id <- as.numeric(factor(results$question, questions))
results$question_id <- as.numeric(factor(results$question, questions))
results$question <- NULL
results %<>%
  dplyr::mutate(date = as.Date(ts)) %>%
  dplyr::arrange(date, query_id, session_id, ts, survey_id, question_id, page_id, choice) %>%
  dplyr::select(c(date, query_id, session_id, ts, survey_id, question_id, page_id, choice))

readr::write_tsv(results, file.path("data", "survey_responses.tsv"))
system("gzip --force data/survey_responses.tsv")

# Numbers:
# nrow(queries)
# nrow(pages)
# data.table::uniqueN(results[, c("date", "session_id")])
# nrow(results)
# sum(results$choice %in% c("yes", "no", "unsure"))
# sum(results$choice == "timeout")
# sum(results$choice == "dismiss")
