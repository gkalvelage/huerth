# transform metadata to data frame of bins
meldee_get_bins <- function(bin_metadata) {
  # extract properties
  uuid_list <- bin_metadata$properties$uuid
  bin_id_list <- bin_metadata$properties$bin_id
  
  # extract coordinates
  coords <- do.call(rbind,
                    lapply(bin_metadata$geometry$coordinates, function(coord) {
                      data.frame(lon = coord[1], lat = coord[2])
                    }))
  
  # create dataframe
  df <- data.frame(
    uuid = uuid_list,
    bin_id = bin_id_list,
    coords,
    stringsAsFactors = FALSE
  )
  
  return(df)
}