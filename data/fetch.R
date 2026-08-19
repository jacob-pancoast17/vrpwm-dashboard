library(readr)
library(sf)
library(tidyverse)

dir.create("data")

geography <- st_read("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/arcgis/rest/services/FS_VCGI_OPENDATA_Boundary_BNDHASH_poly_counties_SP_v1/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson", quiet = TRUE) |>
    st_transform(4326)  

rsv <- read_csv("https://data.cdc.gov/api/v3/views/45cq-cw4i/query.csv?$where=state_territory='vt'&$limit=500000") |>
    filter(state_territory == 'vt') |>
    select(site, 
        counties_served, 
        sample_collect_date,
        pcr_target_avg_conc, 
        lod_sewage, 
        state_territory)

covid <- read_csv("https://data.cdc.gov/api/v3/views/j9g8-acpt/query.csv?$where=state_territory='vt'&$limit=500000") |>
    filter(state_territory == 'vt')|>
    select(site, 
        counties_served, 
        sample_collect_date,
        pcr_target_avg_conc, 
        lod_sewage, 
        state_territory)
        
flu_a <- read_csv("https://data.cdc.gov/api/v3/views/ymmh-divb/query.csv?$where=state_territory='vt'&$limit=500000") |>
    filter(state_territory == 'vt')|>
    select(site, 
        counties_served, 
        sample_collect_date,
        pcr_target_avg_conc, 
        lod_sewage, 
        state_territory)

saveRDS(geography, "data/geography.rds")

saveRDS(rsv, "data/rsv.rds")
saveRDS(covid, "data/covid.rds")
saveRDS(flu_a, "data/flu_a.rds")
