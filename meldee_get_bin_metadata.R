# get metadata for all bins
meldee_get_bin_metadata <- function() {
  # key generated once
  key <- Sys.getenv("KEY")
  class(key) <- "AsIs" # class required
  
  # read encrypted data
  encrypted <- readLines(
    "https://raw.githubusercontent.com/gkalvelage/huerth/main/inputs/bins_encrypted.geojson"
  )
  
  # decrypt using Github secret
  decrypted <- httr2::secret_decrypt(encrypted, key)
  
  # transform to dataframe ----
  
  # read geojson
  geojson <- jsonlite::fromJSON(decrypted)$features
  
  # extract properties
  uuid <- geojson$properties$uuid
  bin_id <- geojson$properties$bin_id
  
  # extract coordinates
  coords <- do.call(rbind,
                    lapply(geojson$geometry$coordinates, function(x)
                      data.frame(lon = x[1], lat = x[2])))
  # create dataframe
  bins <- cbind(uuid, bin_id, coords)
  
  # relabel missing bin id (temporary fix)
  bins$bin_id[bins$bin_id == "offen"] <- "ohne Nr."
  
  return(bins)
}