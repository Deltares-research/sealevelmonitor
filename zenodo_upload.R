
library(httr)
library(jsonlite)

# Set your Zenodo token and API URL
zenodo_token <- Sys.getenv("ZENODO_TOKEN")
zenodo_url <- "https://zenodo.org/api/deposit/depositions"

# Metadata for your upload
metadata <- list(
  metadata = list(
    title = "Dutch Sea Level Monitor",
    upload_type = "dataset",
    description = "Data and scripts for the yearly Dutch Sea Level Monitor",
    creators = list(
      list(name = "Stolte, Willem", affiliation = "Deltares"),
      list(name = "Dees, Nathalie", affiliation = "Deltares")
    )
  )
)

# Create a new deposition
res <- POST(
  zenodo_url,
  add_headers(Authorization = paste("Bearer", zenodo_token)),
  body = metadata,
  encode = "json"
)

deposition <- content(res)
bucket_url <- deposition$links$bucket

# Upload a file
file_path <- "data/my_dataset.csv"
file_name <- basename(file_path)
PUT(
  url = paste0(bucket_url, "/", file_name),
  add_headers(Authorization = paste("Bearer", zenodo_token)),
  body = upload_file(file_path)
)

# Publish the deposition
POST(
  url = paste0(zenodo_url, "/", deposition$id, "/actions/publish"),
  add_headers(Authorization = paste("Bearer", zenodo_token))
)
