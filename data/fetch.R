pacman::p_load(readr, sf)

dir.create("data")

geography <- st_read("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/arcgis/rest/services/FS_VCGI_OPENDATA_Boundary_BNDHASH_poly_counties_SP_v1/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson", quiet = TRUE) |>
    st_transform(4326)  

rsv <- read_csv("https://data.cdc.gov/api/v3/views/45cq-cw4i/query.csv")
measles <- read_csv("https://data.cdc.gov/api/v3/views/akvg-8vrb/query.csv")
covid <- read_csv("https://data.cdc.gov/api/v3/views/j9g8-acpt/query.csv")
flu_a <- read_csv("https://data.cdc.gov/api/v3/views/ymmh-divb/query.csv")
mpox <- read_csv("https://data.cdc.gov/api/v3/views/xpxn-rzgz/query.csv")


saveRDS(geography, "data/geography.rds")

saveRDS(rsv, "data/rsv.rds")
saveRDS(measles, "data/measles.rds")
saveRDS(covid, "data/covid.rds")
saveRDS(flu_a, "data/flu_a.rds")
saveRDS(mpox, "data/mpox.rds")
