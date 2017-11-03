features <- list(
  `survey-only` = c(
    "user_score", "prop_unsure", "engagement"
  ),
  `survey & page info` = c(
    "user_score", "prop_unsure", "engagement",
    "page_size", "is_category", "is_talk", "is_file", "is_list"
  ),
  `survey & traffic` = c(
    "user_score", "prop_unsure", "engagement",
    "traffic"
  ),
  `survey, page info, and traffic` = c(
    "user_score", "prop_unsure", "engagement",
    "page_size", "is_category", "is_talk", "is_file", "is_list",
    "traffic"
  ),
  `survey, page info, and traffic-by-weekday` = c(
    "user_score", "prop_unsure", "engagement",
    "page_size", "is_category", "is_talk", "is_file", "is_list",
    "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"
  ),
  `survey, page info, and traffic-by-platform` = c(
    "user_score", "prop_unsure", "engagement",
    "page_size", "is_category", "is_talk", "is_file", "is_list",
    "desktop", "mobile-app", "mobile-web"
  ),
  `survey, page info, traffic-by-weekday, and traffic-by-platform` = c(
    "user_score", "prop_unsure", "engagement",
    "page_size", "is_category", "is_talk", "is_file", "is_list",
    "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday",
    "desktop", "mobile-app", "mobile-web"
  ),
  `survey, page info, and traffic-by-platform-and-weekday` = c(
    "user_score", "prop_unsure", "engagement",
    "page_size", "is_category", "is_talk", "is_file", "is_list",
    "desktop:friday", "desktop:monday", "desktop:saturday", "desktop:sunday", "desktop:thursday", "desktop:tuesday", "desktop:wednesday",
    "mobile:friday", "mobile:monday", "mobile:saturday", "mobile:sunday", "mobile:thursday", "mobile:tuesday", "mobile:wednesday"
  ),
  `survey, page info, pagesize, and pageviews` = c(
    "user_score", "prop_unsure", "engagement",
    "is_category", "is_talk", "is_file", "is_list",
    "page_length", "views"
  ),
  `survey, page info, log10-pagesize, and log10-pageviews` = c(
    "user_score", "prop_unsure", "engagement",
    "is_category", "is_talk", "is_file", "is_list",
    "loglength", "lviews"
  ),
  `survey, page info, standardized pagesize, and standardized pageviews` = c(
    "user_score", "prop_unsure", "engagement",
    "is_category", "is_talk", "is_file", "is_list",
    "z_length", "znorm_views"
  ),
  `survey, page info, normalized pagesize, and normalized pageviews` = c(
    "user_score", "prop_unsure", "engagement",
    "is_category", "is_talk", "is_file", "is_list",
    "scaled_length", "scaled_views"
  ),
  `survey, page info, standardized log10-pagesize, and standardized log10-pageviews` = c(
    "user_score", "prop_unsure", "engagement",
    "is_category", "is_talk", "is_file", "is_list",
    "z_loglength", "znorm_lviews"
  ),
  `survey, page info, normalized log10-pagesize, and normalized log10-pageviews` = c(
    "user_score", "prop_unsure", "engagement",
    "is_category", "is_talk", "is_file", "is_list",
    "scaled_loglength", "scaled_lviews"
  )
)
