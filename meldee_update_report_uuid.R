# replace historical UUID with current UUID, i.e. combine reports for bin
meldee_update_report_uuid <- function(reports, bin_metadata) {
  # extract current UUIDs and history of UUIDs
  current_uuids <- bin_metadata$properties$uuid
  uuid_history_list <- bin_metadata$properties$uuid_history
  
  # update UUIDs in reports based on their history
  reports$uuid <- sapply(reports$uuid, function(report_uuid) {
    
    # find index of feature that contains report UUID in its history
    matching_index <- which(sapply(uuid_history_list, function(history)
      report_uuid %in% history))
    
    if (length(matching_index) > 0) {
      # replace with the first matching current UUID
      return(current_uuids[matching_index[1]])
    }
    
    # return original UUID if no match is found
    return(report_uuid)
  })
  
  return(reports)
}