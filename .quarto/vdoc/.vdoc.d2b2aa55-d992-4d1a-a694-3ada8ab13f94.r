#
#
#
#
#
#
#
#
#
#
#| context: setup
#| echo: false
pacman::p_load(tidyverse,
               sf,
               shiny)

data_unfiltered <- read_csv("https://data.cdc.gov/api/v3/views/45cq-cw4i/query.csv")

#geography <- read_sf("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/arcgis/rest/services/FS_VCGI_OPENDATA_Boundary_BNDHASH_poly_counties_SP_v1/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

data <- data_unfiltered |>
    group_by(sample_collect_date) |>
    mutate(year = year(sample_collect_date)) |>
    filter(state_territory == 'vt')

data |>
    summarise(unique(year))

years <- data_unfiltered |>
    filter(state_territory == 'vt') |>
    mutate(year = year(sample_collect_date)) |>
    select(year) |>
    distinct() |>
    arrange(desc(year))

# county <- data_unfiltered |>
#     filter(state_territory == 'vt') |>
#     count(counties_served, name = "num") |>
#     mutate(CNTYNAME = toupper(counties_served))

# spatial <- geography |>
#     left_join(county, by = "CNTYNAME")

#
#
#

# Input for graph year
selectInput("year", "Year:", choices = c(2026, 2025, 2024, 2023))

plotOutput("plot")

#
#
#
#| context: server

library(tidyverse)

output$plot <- renderPlot({

    # Filter data
    filtered_data = data |>
    filter(year == input$year)

    # Return graph
    ggplot(
    data = filtered_data
    ) +
    geom_line(
        mapping = aes(
        x = sample_collect_date,
        y = pcr_target_avg_conc
        )
    ) + labs(
        title = input$year
    )
})

#
#
#
# ggplot(
#     data = spatial
# ) +
#     geom_sf(
#         aes(
#             fill = num
#         )
#     ) +
#     theme_void()
#
#
#
#
#
