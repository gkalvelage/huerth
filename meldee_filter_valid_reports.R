# check if reported UUID is valid
meldee_filter_valid_reports <- function(reports, bin_metadata) {
  # extract history of UUIDs for all features
  uuid_history_list <- bin_metadata$properties$uuid_history
  
  # create logical vector to check if reported UUID is valid
  valid_uuids <- sapply(reports$uuid, function(report_uuid) {
    any(sapply(uuid_history_list, function(history)
      report_uuid %in% history))
  })
  
  # keep only valid reports
  valid_reports <- reports[valid_uuids, ]
  
  return(valid_reports)
}