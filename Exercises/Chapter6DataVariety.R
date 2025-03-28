library(ggplot2)
library(readr)
library(lubridate)
library(dplyr)
library(checkthat)
library(ggpmisc)

# define a URL with data of interest
# in this case annual mean CO2 levels at Mauna Loa
url <- "https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_annmean_mlo.csv"

# read in the data directly from URL
df <- read.table(
  url,
  header = TRUE,
  sep = ","
)


# load the library
library("MODISTools")

# list all available products
products <- MODISTools::mt_products()

# print the first few lines
# of available products
print(head(products))

# download a demo dataset
# specifying a location, a product,
# a band (subset of the product)
# and a date range and a geographic
# area (1 km above/below and left/right).
# Data is returned internally and the
# progress bar of the download is shown.
subset <- MODISTools::mt_subset(
  product = "MOD11A2",
  lat = 40,
  lon = -110,
  band = "LST_Day_1km",
  start = "2004-01-01",
  end = "2004-02-01",
  km_lr = 1,
  km_ab = 1,
  internal = TRUE,
  progress = TRUE
)

# print the dowloaded data
print(head(subset))


# set API URL endpoint
# for the total sand content
url <- "https://thredds.daac.ornl.gov/thredds/ncss/ornldaac/1247/T_SAND.nc4"

# formulate query to pass to httr
query <- list(
  "var" = "T_SAND",
  "south" = 32,
  "west" = -81,
  "east" = -80,
  "north" = 34,
  "disableProjSubset" = "on",
  "horizStride" = 1,
  "accept" = "netcdf4"
)

# download data using the
# API endpoint and query data
status <- httr::GET(
  url = url,
  query = query,
  httr::write_disk(
    path = file.path(tempdir(), "T_SAND.nc"),
    overwrite = TRUE
  )
)

# to visualize the data
# we need to load the {terra}
#library

library("terra")

sand <- terra::rast(file.path(tempdir(), "T_SAND.nc"))
terra::plot(sand)


url1 <- "https://raw.githubusercontent.com/geco-bern/agds_book/refs/heads/main/book/data/demo_1.csv"

# read in the data directly from URL
df1 <- read.table(
  url1,
  header = TRUE,
  sep = ","
)

url2 <- "https://raw.githubusercontent.com/geco-bern/agds_book/refs/heads/main/book/data/demo_2.csv"

# read in the data directly from URL
df2 <- read.table(
  url2,
  header = TRUE,
  sep = ""
)

url3 <- "https://raw.githubusercontent.com/geco-bern/agds_book/refs/heads/main/book/data/demo_3.csv"

# read in the data directly from URL
df3 <- read.table(
  url3,
  header = TRUE,
  sep = ";",
  skip = 3
)

df_all <- bind_rows(df1,df2,df3)

write.table(
  x = df_all,
  file = file.path(tempdir(), "your_file.csv"),
  sep = ",",
  row.names = FALSE
)

df <- read.table(
  file.path(tempdir(), "your_file.csv"),
  header = TRUE,
  sep = ","
)

library("jsonlite")

jsonlite::write_json(
  x = df,
  path = file.path(tempdir(), "your_json_file.json")
)
