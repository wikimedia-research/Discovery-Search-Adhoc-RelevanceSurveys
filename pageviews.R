library(magrittr)

pages <- readr::read_tsv(file.path("data", "page_info.tsv"))
pages$page_title <- gsub(" ", "_", pages$page_title_api, fixed = TRUE)

ua <- httr::user_agent("MPopov (WMF) | GitHub: wikimedia-research/Discovery-Search-Adhoc-RelevanceSurveys")
get_pageviews <- function(articles, access, start, end) {
  titles <- urltools::url_encode(articles)
  # start <- "20170901"; end <- "20170930"
  results <- do.call(rbind, lapply(titles, function(title) {
    message(glue::glue("Fetching {access} pageviews for \"{title}\"..."))
    url <- glue::glue("https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/en.wikipedia.org/{access}/user/{title}/daily/{start}/{end}")
    result <- NULL
    try({
      response <- httr::GET(url, ua)
      content <- httr::content(response)
      result <- as.data.frame(do.call(rbind, purrr::map(content$items, ~ do.call(cbind, .x))), stringsAsFactors = FALSE)
      result$timestamp <- as.Date(result$timestamp, "%Y%m%d00")
      result$views <- as.integer(result$views)
    })
    if (is.null(result)) {
      message(glue::glue("Something went wrong with fetching {access} pageviews for \"{title}\"!"))
      result <- dplyr::data_frame(
        project = "en.wikipedia", article = urltools::url_decode(title),
        granularity = "daily", timestamp = as.Date(start, "%Y%m%d"),
        access = access, agent = "user", views = as.integer(NA)
      )
    }
    Sys.sleep(1 / 200) # helps with the 200 calls/sec rate limiting
    return(result)
  }))
  return(results)
}

desktop_pvs <- get_pageviews(pages$page_title, "desktop", "20170901", "20170930")
mobile_app_pvs <- get_pageviews(pages$page_title, "mobile-app", "20170901", "20170930")
mobile_app_retries <- get_pageviews(
  mobile_app_pvs$article[is.na(mobile_app_pvs$views)],
  "mobile-app", "20170901", "20170930"
)
mobile_app_pvs <- rbind(mobile_app_pvs[!is.na(mobile_app_pvs$views), ], mobile_app_retries)
mobile_web_pvs <- get_pageviews(pages$page_title, "mobile-web", "20170901", "20170930")
mobile_web_retries <- get_pageviews(
  mobile_web_pvs$article[is.na(mobile_web_pvs$views)],
  "mobile-app", "20170901", "20170930"
)
mobile_web_pvs <- rbind(mobile_web_pvs[!is.na(mobile_web_pvs$views), ], mobile_web_retries)
pvs <- {
  desktop_pvs[, c("article", "access", "timestamp", "views")] %>%
    rbind(mobile_app_pvs[, c("article", "access", "timestamp", "views")]) %>%
    rbind(mobile_web_pvs[, c("article", "access", "timestamp", "views")]) %>%
    dplyr::rename(page_title = article, platform = access, date = timestamp) %>%
    dplyr::left_join(pages[, c("page_id", "page_title")], by = "page_title") %>%
    dplyr::select(date, page_id, platform, views) %>%
    dplyr::arrange(date, page_id, platform)
}
readr::write_tsv(pvs, file.path("data", "daily_pageviews.tsv"))
system("gzip --force data/daily_pageviews.tsv")
