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
  CONVERT(pages.page_title USING utf8) AS page_title,
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
    result <- {
      result %>%
        dplyr::group_by(session_id) %>%
        dplyr::arrange(ts) %>%
        dplyr::mutate(survey_id = cumsum(!is.na(choice))) %>%
        dplyr::ungroup()
    }
    return(result)
  }
))

wmf::mysql_close(con)

if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

questions <- unique(results$question); dump("questions", "")

queries <- results %>%
  dplyr::arrange(query) %>%
  dplyr::distinct(query) %>%
  dplyr::mutate(query_id = as.numeric(factor(query))) %>%
  dplyr::select(query_id, query)
readr::write_tsv(queries, file.path("data", "search_queries.tsv"))

results %<>% dplyr::left_join(queries, by = "query")
results$query <- NULL

# pages <- results %>%
#   dplyr::arrange(page_id, page_title) %>%
#   dplyr::distinct(page_id, page_title)

# Finish processing events:
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

# API accepts 50 page IDs at a time max, so we need to separate them into batches:
n_pages <- length(unique(pages$page_id))
n_batches <- ceiling(n_pages / 50)
batches <- as.vector(vapply(1:n_batches, rep.int, numeric(50), times = 50))
pages$batch <- batches[1:n_pages]

# Verify that we ended up with at most 50 pages per batch:
max(table(pages$batch)) # ok, good

url <- "https://en.wikipedia.org/w/api.php"

# Utility function for using in the purrr::map_df call below:
json2df <- function(page) {
  return(dplyr::bind_rows(
    lapply(page$pageviews, . %>% data.frame(pvs = .)),
    .id = "date"
  ))
}

ua <- httr::user_agent("MPopov (WMF) | GitHub: wikimedia-research/Discovery-Search-Adhoc-RelevanceSurveys")

results <- do.call(rbind, lapply(unique(pages$batch), function(i) {
  message("Processing batch ", i, "...")
  page_ids <- paste0(pages$page_id[pages$batch == i], collapse = "|")
  # Using POST instead of GET because otherwise the URL would too long due to 50 IDs
  response <- httr::POST(
    url, body = list(
      action = "query",
      prop = "info",
      format = "json",
      pageids = page_ids
    ), ua
  )
  content <- httr::content(response)
  result <- purrr::map_df(
    content$query$pages,
    ~ data.frame(
      page_title = .x$title,
      page_length = .x$length,
      stringsAsFactors = FALSE
    ),
    .id = "page_id"
  )
  return(result)
}))

results %<>% dplyr::rename(page_title_api = page_title)
pages %<>% dplyr::rename(page_title_db = page_title)
pages %<>% dplyr::left_join(results, by = "page_id")
pages %<>% dplyr::select(-batch)
pages %<>% dplyr::arrange(page_id)
# View(pages[pages$page_title_db != pages$page_title_api, ])
readr::write_tsv(pages, file.path("data", "page_info.tsv"))
