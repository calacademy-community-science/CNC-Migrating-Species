library(duckdb)
library(tidyverse)
library(terra)
library(viridis)
library(leaflet)

dcon <- dbConnect(duckdb(
  dbdir = "~/Data/Occurrences/GBIF/gbif.duckdb",
  read_only = T
))
dcon |> dbExecute("install spatial; load spatial; install httpfs; load httpfs;")

spec_names <- c(
  "Galeocerdo cuvier",
  "Dermochelys coriacea",
  "Danaus plexippus",
  "Phoeniconaias minor",
  "Pandion haliaetus",
  "Apus apus"
) |>
  paste0(collapse = "','")


dcon |> dbExecute(
  sprintf(
    "
    COPY (
  SELECT DISTINCT species, year, geom, decimallatitude,
  decimallongitude, coordinateuncertaintyinmeters, institutioncode
  FROM gbif
  WHERE gbif.species IN ('%s')
  )
  TO '%s' (FORMAT PARQUET);",
    spec_names,
    "~/Projects/random-requests/random-requests/cnc-migration/occurrences.parquet"
  )
)



## Testing
con <- dbConnect(duckdb())
con |> dbExecute("
CREATE VIEW occ AS
SELECT *
FROM PARQUET_SCAN('~/Projects/random-requests/random-requests/cnc-migration/occurrences.parquet');")

occurrences <- con |>
  tbl("occ") |>
  mutate(
    latitude = round(decimallatitude, 1),
    longitude = round(decimallongitude, 1)
  ) %>%
  filter(coordinateuncertaintyinmeters < 300 | is.na(coordinateuncertaintyinmeters)) |>
  count(longitude, latitude, species) %>%
  mutate(n = log(n)) |>
  collect() |>
  na.omit()

con |>
  tbl("occ") |>
  count()
con |>
  tbl("occ") |>
  filter(institutioncode == "iNaturalist") |>
  count()
cu <- con |> tbl("occ") |> 
  select(coordinateuncertaintyinmeters) |> 
  collect() 

ggplot(cu, aes(x = coordinateuncertaintyinmeters)) + 
  geom_histogram() +
  xlim(0, 10000)

# Convert the occurrences data into a terra raster
rast_data <- rast(occurrences, crs = "epsg:4326", type = "xyz") |>
  project("epsg:3857")

plot(rast_data[["n"]])
values(rast_data[["n"]]) |> as.integer() |> unique()
plot(rast_data, col = viridis(1e3), legend = FALSE, maxcell = 6e6, colNA = "black", axes = FALSE)

(res(rast_data)[1] |> round(-3)) / 1000


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  # addTiles(rast_data, opacity = 0.8, attribution = NULL) %>%
  addRasterImage(rast_data["n"], colors = "Spectral") #|>
# addLegend(pal = pal, values = values(rast_data), opacity = 0.8, title = "Log Count", position = "bottomright")
plot(rast_data_reduced)
