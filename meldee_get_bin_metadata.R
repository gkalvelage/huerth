# get metadata for all bins
meldee_get_bin_metadata <- function(path) {
  # key generated once
  key <- Sys.getenv("KEY")
  class(key) <- "AsIs" # class required
  
  # read encrypted data
  encrypted <- readLines(path)
  
  # decrypt using Github secret
  decrypted <- httr2::secret_decrypt(encrypted, key)
  
  # read features from GeoJSON
  bins <- jsonlite::fromJSON(decrypted)$features
  
  return(bins)
}