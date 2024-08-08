# get reports from API
meldee_get_reports <- function() {
  # retrieve reports from API ----
  url <- Sys.getenv("API")
  response <- httr2::request(url) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)
  
  # split report content into columns
  content <- strsplit(response$post_content, split = "\n")
  
  # transform to dataframe
  reports <- data.frame(
    datetime = as.POSIXct(response$post_date_gmt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"),
    uuid = sapply(content, `[`, 2),
    response = sapply(content, `[`, 1),
    user_agent = sapply(content, `[`, 5),
    serial_number = as.numeric(sapply(content, `[`, 3))
  )
  
  # adjust timezone
  attr(reports$datetime, "tzone") <- "Europe/Berlin"
  
  # clean duplicate reports
  reports <- reports[order(reports$uuid,
                           reports$response,
                           reports$user_agent,
                           reports$datetime), ]
  
  # calculate time differences between submissions
  reports$time_diff <- c(NA, diff(reports$datetime))
  
  # mark duplicates within 120 seconds
  reports$duplicate <- c(FALSE, with(
    reports[-1, ],
    (uuid == reports$uuid[-nrow(reports)]) &
      (response == reports$response[-nrow(reports)]) &
      (user_agent == reports$user_agent[-nrow(reports)]) &
      (time_diff <= 120)
  ))
  
  
  # remove duplicates
  reports <- reports[!reports$duplicate, ]
  
  # drop helper columns
  reports <- reports[, !names(reports) %in% c("time_diff", "duplicate")]
  
  # clean response codes
  categories <- c(
    "Ja, dieser Mülleimer ist voll." = "Mülleimer voll",
    "Nein, Hundekotbeutel fehlen." = "Hundekotbeutel fehlen",
    "Nein, wilder Müll liegt hier." = "wilder Müll am Mülleimer",
    "Ein anderes Problem." = "anderes Problem"
  )
  reports$response <- categories[reports$response]
  
  # order by date and time of report
  reports <- reports[order(reports$datetime, decreasing = TRUE), ]
  
  return(reports)
}
