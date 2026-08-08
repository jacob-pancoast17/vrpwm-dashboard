pacman::p_load(readr, sf)

dir.create("data")

data_unfiltered <- read_csv("https://data.cdc.gov/api/v3/views/45cq-cw4i/query.csv")
geography <- st_read("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/arcgis/rest/services/FS_VCGI_OPENDATA_Boundary_BNDHASH_poly_counties_SP_v1/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson", quiet = TRUE) |>
    st_transform(4326)  

saveRDS(data_unfiltered, "data/nwss.rds")
saveRDS(geography, "data/geography.rds")