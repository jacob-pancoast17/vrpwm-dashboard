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
               shiny,
               leaflet)

data_unfiltered <- read_csv("https://data.cdc.gov/api/v3/views/45cq-cw4i/query.csv")

geography <- st_read("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/arcgis/rest/services/FS_VCGI_OPENDATA_Boundary_BNDHASH_poly_counties_SP_v1/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson", quiet = TRUE) |>
    st_transform(4326)

years <- data_unfiltered |>
    filter(state_territory == 'vt') |>
    mutate(year = year(sample_collect_date)) |>
    select(year) |>
    distinct() |>
    arrange(desc(year))

#
#
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
    filtered_data <- data_unfiltered |>
    group_by(sample_collect_date) |>
    mutate(year = year(sample_collect_date)) |>
    filter(state_territory == 'vt') |>
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

min_date <- data_unfiltered |>
    filter(sample_collect_date == min(sample_collect_date)) |>
    select(sample_collect_date) |>
    distinct() |>
    pull(sample_collect_date)

dateInput("date", "Date:", min = min_date)

leafletOutput("map")

#
#
#
#| context: server

filtered_data <- reactive({
    # Step 1 - Group data

    grouped <- data_unfiltered |>

        # Vermont only
        filter(state_territory == 'vt') |>

        # Group by each site
        group_by(site, counties_served) |>

        # Rename county column
        rename(county = counties_served)   


#grouped |> select(county, site, sample_collect_date, pcr_target_avg_conc, lod_sewage) |> filter(pcr_target_avg_conc < lod_sewage, pcr_target_avg_conc > 0)

    # Step 2 - Validate data

    validated <- grouped |> 

        # Log transform the concentrations
        mutate(pcr_target_avg_conc = log1p(pcr_target_avg_conc),
        pcr_target_avg_conc_z_score = scale(pcr_target_avg_conc)) |>

        # Remove outliers
        filter(pcr_target_avg_conc_z_score <= 4)

    validated

    # Step 3 - Determine low levels for each site

    low_levels <- validated |>

        # Only grab data from last 2 years
        filter(sample_collect_date > (if_else(
            month(max(sample_collect_date)) < 8,

            # If we are before August of curr year, use last year's August 1st
            make_date(year = year(max(sample_collect_date)) - 1, month = 8, day = 1),

            # If we are past August 1st of curr year, use this year's August 1st
            make_date(year = year(max(sample_collect_date)), month = 8, day = 1)) - years(2)
            )
        ) |>

        summarise(

            # 10th percentile
            baseline = quantile(pcr_target_avg_conc, probs = 0.1, na.rm = TRUE),

            # Std dev of concs (using 8 weeks of data, hence 56 days)
            std_dev = sd(pcr_target_avg_conc, na.rm = TRUE)

        )

        low_levels

    # Step 4 // 5 - Compare current levels to baselines // Identify average and median values

    validated |>
        
        # Find the date closest to the input date to use as the "current amount of virus"
        summarise(
            
            # date of interest (DOT)
            date_of_interest = max(sample_collect_date[sample_collect_date <= input$date]),

            start_of_dot_week = floor_date(date_of_interest, "week", week_start = 7),

            week_of_interest_avg_conc = mean(
                pcr_target_avg_conc[sample_collect_date <= date_of_interest & sample_collect_date >= start_of_dot_week]
                )
            
            ) |>

        left_join(low_levels, by = c("county", "site")) |>

        group_by(county, site) |>

        summarise(wval = (week_of_interest_avg_conc - baseline) / std_dev) |>

        ungroup() |>

        group_by(county) |>

        select(county, wval) |>

        summarise(wval = median(wval)) |>

        mutate(county = toupper(county)) |>

        ungroup() 
})

output$map <- renderLeaflet({

        # Step 6 - Categorize values based on CDC cutoffs

        spatial <- geography |>

        left_join(filtered_data(), by = join_by(CNTYNAME == county)) |>

        mutate(
            category = cut(
                wval, 
                breaks = c(
                    0, 
                    2.5, 
                    5.2, 
                    8, 
                    11, 
                    Inf
                    ), 
                labels = c(
                    "Very Low", 
                    "Low", 
                    "Moderate", 
                    "High", 
                    "Very High"), 
                include.lowest = TRUE
                )
            )

    palette <- colorFactor(
        palette = c(
            "#bfc2d9", 
            "#969fc9", 
            "#6d7db8", 
            "#425ba6", 
            "#153993"
            ),
        levels = c(
            "Very Low", 
            "Low", 
            "Moderate", 
            "High", 
            "Very High"
            ),
        na.color = "#9c9c9c"
    )

    leaflet(
        spatial,
        options = leafletOptions(
            scrollWheelZoom = FALSE,
            zoomControl = FALSE
            )
        ) |>

        addPolygons(
            fillColor = ~palette(category),
            fillOpacity = 0.8,
            color = "white",
            weight = 1,
            label = ~paste0(CNTYNAME, ": ", category),
            highlightOptions = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE)
        )

    })

output$coverage <- renderText({

    paste0("Median WVAL: ", round(median(filtered_data()$wval, na.rm = TRUE), digits = 2))

    paste0(selected_id)
    

})

#
#
#
#
#

textOutput("coverage")

#
#
#

selected_id <- reactiveVal(NULL)

observeEvent(input$map_shape_click, {
    click <- input$map_shape_click

    if (!(is.null(selected_id())) & selected_id() == click$id) {
        selected_id(NULL)
    } else {
        selected_id(click$id)
    }
})

#
#
#
#
#
