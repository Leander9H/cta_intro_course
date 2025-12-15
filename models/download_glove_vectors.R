
model_path <- file.path("models")
url <- "https://nlp.stanford.edu/data/wordvecs/glove.2024.wikigiga.50d.zip"
dest <- file.path(model_path, basename(url))
if (!file.exists(dest)) {
  require(curl)
  
  h <- new_handle()
  handle_setopt(
    h,
    followlocation = TRUE,
    timeout = 0,          # no overall timeout
    connecttimeout = 60,  # allow slow connects
    low_speed_time = 300, # only fail if too slow for too long
    low_speed_limit = 1
  )
  
  # set timeout to 5 minutes
  curl_download(url, destfile = dest, handle = h)
}
message("Model downloaded to: ", dest)