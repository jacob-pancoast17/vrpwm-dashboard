library(tidyverse)
library(sf)
library(shiny)
library(shinyWidgets)
library(bslib)
library(bsicons)

theme <- bs_theme(version = 5, primary = "#237a21") |>
  bs_add_variables(
    "h2-font-size"  = "1.6rem",
    "h3-font-size"  = "1.1rem",
    "h4-font-size"  = "1.05rem",
    "h5-font-size"  = "0.75rem",
    .where = "defaults"
  ) |>
  bs_add_rules(sass::sass_file("custom.scss"))

rsv <- readRDS("data/rsv.rds")
flu_a <- readRDS("data/flu_a.rds")
covid <- readRDS("data/covid.rds")

geography <- readRDS("data/geography.rds")

ui <- page_sidebar(
    theme = theme,
    title = span(h1("Vermont Respiratory Pathogens Wastewater Monitoring Dashboard"), style = "color: #106314; font-weight: bold"),
    sidebar = sidebar(
        h4("Last updated 8/30/2026",
          popover(
            bs_icon(
              "question-circle"
            ),
            title = "Data Fetching",
            span("The CDC updates their timeline of wastewater sample data for select pathogens every Friday evening. Every Saturday, this website is updated to reflect the latest data.")
        )),
        # Select pathogen
        selectInput(
            "disease",
            "Pathogen",
            choices = c("RSV", "Influenza A", "COVID-19"),
            selected = "RSV"
            ),
        # Select year
        selectInput(
            "season", 
            "Season", 
            choices = 0, # PLACEHOLDER DURING INIT
            selected = 0, # PLACEHOLDER DURING INIT
            selectize = FALSE
            ),
        # Select week
        sliderInput(
            "week", 
            "Week ending", 
            min = 1, 
            max = 52, # PLACEHOLDER DURING INIT
            value = 52, # PLACEHOLDER DURING INIT
            step = 1,
            ticks = FALSE
            ),
        h4("Learn more about the data pipeline",
          popover(
              bs_icon(
                  "question-circle"
              ),
              title = "Data Pipeline",
              span("For participating NWSS sites across each county, wastewater viral activity levels (WVALs) are calculated following the standard WVAL calculation protocol outlined at https://www.cdc.gov/wastewater/about/wval.html.",
                   br(),
                   br(),
                   "WVALs are critical in normalizing the data, which allows for comparison between sites that may differ in sample collection methods, testing methods, and testing frequency.")
          )
        ),
        h5("Source code: https://github.com/jacob-pancoast17/vrpwm-dashboard")
    ),
    layout_columns(
        layout_columns(
            col_widths = 12,
            layout_columns(
                card(htmlOutput("state_summary")),
                card(htmlOutput("county_summary"))
            ),
            card(plotOutput("heatmap"))
        ),
        card(plotOutput("map", click = "map_click"))
    ),
    span(
      h4("Created by Jacob Pancoast"), h5("Undergraduate at the University of Vermont"), h5("https://jacob-pancoast17.github.io/")
    )
)

server <- function(input, output, session) {

    #####################################################
    # HTML UIs
    #####################################################

    output$year <- renderUI({
        HTML(paste0(
            "<h3>", "Year", "</h3>"
        ))
    })

    output$week <- renderUI({
        HTML(paste0(
            "<h3>", "Week", "</h3>",
            "<h5>", "ending ", selected_date(), "</h5>"
        ))
    })

    #####################################################
    # Reactive to user input
    #####################################################

    selected_data <- reactive({

        req(input$disease)

        switch(
            input$disease,
            "RSV" = rsv,
            "Influenza A" = flu_a,
            "COVID-19" = covid
        )

    })

    seasons_of_data <- reactive({

        selected_data() |>
            mutate(
                year = year(sample_collect_date),
                season_start = if_else(
                    month(sample_collect_date) <= 6,
                    year - 1,
                    year
                ),
                season = paste0(season_start, "-", season_start + 1)
            ) |>
            distinct(season, season_start) |>
            arrange(desc(season_start)) |>
            pull(season)

    })

    selected_date <- reactive({

        req(input$season, input$week)

        season_beginning = make_date(
            year = selected_y1(),
            month = 7,
            day = 1
        )

        ceiling_date(season_beginning + weeks(input$week - 1), unit = "week") - 1

    })

    selected_id <- reactiveVal("CHITTENDEN")

    selected_y1 <- reactive({

        strsplit(input$season, "-")[[1]][1]

    })

    selected_y2 <- reactive({

        strsplit(input$season, "-")[[1]][2]

    })

    min_date <- reactive({

        # Grab the minimum date from the entire data set
        selected_data() |>
            summarise(
                min_date = ceiling_date(min(sample_collect_date), unit = "week"),
            ) |>
            pull(min_date)

    })

    max_date <- reactive({

        # Grab max date
        selected_data() |>
            summarise(
                max_date = max(sample_collect_date)
            ) |>
            pull(max_date)

    })

    min_week <- reactive({

        # If selected season is the first one in the data, use that number of weeks
        if (input$season == min(seasons_of_data())) {
            as.numeric((min_date() - ceiling_date(
                make_date(
                    year = selected_y1(),
                    month = 7,
                    day = 1
                    ),
                    "week"
                )) / 7 + 1)
            } else { # Otherwise we are in a year that has a beginning so just use 1
                1
                }

    })

    max_week <- reactive({

        req(selected_y1(), selected_y2())

        # If selected year is the current, unfinished year, use that number of weeks
        if (input$season == max(seasons_of_data()))
        {
          # Subtract the start date from the max date and divide by 7
            as.numeric(ceiling((max_date() - floor_date(
                make_date(
                    year = selected_y1(),
                    month = 7,
                    day = 1
                ),
                "week"
            )) / 7))
            } else { # Otherwise use 52 weeks for a past full year
                52
                }
    })

    wval_colors <- reactive({

        switch(
            input$disease,
            "RSV" = {
                c("Very Low" = "#d3ead2", 
                "Low" = "#a4c4a3", 
                "Moderate" = "#789f75", 
                "High" = "#4c7c49", 
                "Very High" = "#20591d")
            },
            "Influenza A" = {
                c("Very Low" = "#bfc2d9", 
                "Low" = "#969fc9", 
                "Moderate" = "#6d7db8", 
                "High" = "#425ba6", 
                "Very High" = "#153993")
            },
            "COVID-19" = {
                c("Very Low" = "#ead2d5", 
                "Low" = "#bd9da1", 
                "Moderate" = "#906b6f", 
                "High" = "#663c40", 
                "Very High" = "#3c1016")
            }
        )

    })

    #####################################################
    # Data pipeline (from CDC)
    #####################################################

    filtered_data_step_1_2 <- reactive({
        # Step 1 - Group data

        grouped <- selected_data() |>

            # Group by each site
            group_by(site, counties_served) |>

            # Rename county column
            rename(county = counties_served)   

        # Step 2 - Validate data

        validated <- grouped |> 

            # Log transform the concentrations
            mutate(pcr_target_avg_conc = log1p(pcr_target_avg_conc),
            pcr_target_avg_conc_z_score = scale(pcr_target_avg_conc)) |>

            # Remove outliers
            filter(pcr_target_avg_conc_z_score <= 4)
        
    })

    filtered_data_step_3 <- reactive({
        # Step 3 - Determine low levels for each site

        filtered_data_step_1_2() |>

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
    })

    filtered_data_step_4_5_for_individ <- reactive({

        # Step 4 // 5 - Compare current levels to baselines // Identify average and median values

        filtered_data_step_1_2() |>
            
            # Find the date closest to the input date to use as the "current amount of virus"
            summarise(
                
                # date of interest (DOT)
                date_of_interest = max(sample_collect_date[sample_collect_date <= selected_date() & !is.na(pcr_target_avg_conc)]),

                start_of_dot_week = floor_date(date_of_interest, "week", week_start = 7),

                week_of_interest_avg_conc = mean(
                    pcr_target_avg_conc[sample_collect_date <= date_of_interest & sample_collect_date >= start_of_dot_week], 
                    na.rm = TRUE
                    )
                
                ) |>

            left_join(filtered_data_step_3(), by = c("county", "site")) |>

            group_by(county, site) |>

            summarise(wval = exp((week_of_interest_avg_conc - baseline) / std_dev)) |>

            ungroup() |>

            group_by(county) |>

            summarise(wval = median(wval, na.rm = TRUE),
                num_sites = n_distinct(site, na.rm = TRUE)) |>

            mutate(county = toupper(county)) |>

            ungroup() 
    })

    filtered_data_step_4_5_for_heatmap <- reactive({

        # Step 4 // 5 - Compare current levels to baselines // Identify average and median values

        filtered_data_step_1_2() |>

            ungroup() |>

            mutate(
                # all months
                month = format(sample_collect_date, "%Y-%m")
            ) |>
            
            # Find the date closest to the input date to use as the "current amount of virus"
            summarise(
                avg_concentration = mean(pcr_target_avg_conc, na.rm = TRUE),

                .by = c(month, county, site)
                
                ) |>

            left_join(filtered_data_step_3(), by = c("county", "site")) |>

            group_by(county, site, month) |>

            summarise(wval = exp((avg_concentration - baseline) / std_dev)) |>

            ungroup() |>

            group_by(county, month) |>

            summarise(wval = median(wval),
                num_sites = n_distinct(site, na.rm = TRUE)) |>

            ungroup()

    })

    #####################################################
    # Update when data changes
    #####################################################

    # Update slider
    observe({

        req(input$season, input$season != 0)
        
        updateSliderInput(
            session,
            "week",
            label = paste0("Week ending ", selected_date()),
            max = max_week(),
            min = min_week(),
            value = min(input$week, max_week())
            )

    })

    # Update select input
    observe({

        updateSelectInput(
            session,
            "season",
            choices = seasons_of_data(), 
            selected = if (input$season %in% seasons_of_data()) input$season else max(seasons_of_data())
            )

    })

    # Update map on click
    observeEvent(input$map_click, {
        pt <- sf::st_sfc(sf::st_point(c(input$map_click$x, input$map_click$y)),
                        crs = sf::st_crs(geography))
        hit <- geography$CNTYNAME[lengths(sf::st_intersects(geography, pt)) > 0]

        if (length(hit) == 1) {
            if (!is.null(selected_id()) && selected_id() == hit) selected_id(NULL) else selected_id(hit)
        }
    })

    #####################################################
    # Outputs
    #####################################################

    ## FIGURES

    # WVAL heatmap
    output$heatmap <- renderPlot({

        season_start <- selected_y1()

        # Filter data in this range
        filtered_data <- selected_data() |>

            # In the year range
            filter(
                sample_collect_date >= make_date(
                    year = selected_y1(),
                    month = 7,
                    day = 1
                ),
                sample_collect_date <= make_date(
                    year = selected_y2(),
                    month = 6,
                    day = 30
                )
            ) |>

            mutate(
                month = format(sample_collect_date, "%Y-%m")
                ) |>

            summarise(
                concentration = mean(pcr_target_avg_conc, na.rm = TRUE),
                .by = c(month, counties_served)
            ) |>

            rename(county = counties_served) |>

            ungroup() |>

            left_join(filtered_data_step_4_5_for_heatmap(), by = c("county", "month")) |>

            mutate(
                wval = factor(
                    categorize_wval(wval),
                    levels = c(
                        "Very Low",
                        "Low",
                        "Moderate",
                        "High",
                        "Very High"
                    )
                )
            )

        # Return graph
        ggplot(
        data = filtered_data
        ) +
        geom_tile(
            mapping = aes(
            x = month,
            y = county,
            fill = as.factor(wval)
            )
        ) + labs(
            title = paste0(
                input$season,
                " Season"
                ),
            y = "County",
            x = "Month",
            fill = "Activity Level"
        ) +
        scale_fill_manual(
            values = wval_colors(),
            na.value = "#6b6969"
        ) + theme_minimal() +
        theme(
            text = element_text(
                family = "Segoe UI"
            ),
            plot.title = element_text(
                hjust = 0.5,
                face = "bold",
                size = 1.1 * 16,
                color = "#808080"
            ),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
        )
    })

    # Vermont counties map
    output$map <- renderPlot({

        # Step 6 - Categorize values based on CDC cutoffs

        spatial <- geography |>

        left_join(filtered_data_step_4_5_for_individ(), by = join_by(CNTYNAME == county)) |>

        mutate(
            wval = factor(
                categorize_wval(wval),
                levels = c(
                    "Very Low",
                    "Low",
                    "Moderate",
                    "High",
                    "Very High"
                )
            )
        )

    map <- ggplot(spatial) +
        geom_sf(
            aes(
                fill = as.factor(wval),
            ),
            color = 'white',
            linewidth = 0.3
        ) +
        scale_fill_manual(
            values = wval_colors(),
            na.value = "#6b6969",
            name = "Activity Level"
        ) +
        theme_void()

    if (!is.null(selected_id())) {
        map <- map + 
            geom_sf(
                data = spatial |>
                    filter(CNTYNAME == selected_id()),
                    fill = NA,
                    color = 'black',
                    linewidth = 1
            )
    }

    map

    })

    ## HTML OUTPUTS

    # State summary card
    output$state_summary <- renderUI({

        # Filter and categorize (MAKE THIS A FUNCTION)
        median_wval <- filtered_data_step_4_5_for_individ() |>
            summarise(wval = round(median(wval, na.rm = TRUE), digits = 2)) |>
            pull(wval)
        
        median_wval <- categorize_wval(median_wval)

        # HTML for rendering
        HTML(paste0(
            "<h3>", "State", "</h3>",
            "<h2>", "VERMONT", "</h2>",
            "<h4>", selected_date(), "</h4>",
            "<p>", "Activity: ", median_wval, "</p>"
        ))
        
    })

    # County summary card
    output$county_summary <- renderUI ({

        # Filter and categorize (MAKE THIS A FUNCTION)
        selected_wval <- filtered_data_step_4_5_for_individ() |>
            filter(county == selected_id()) |>
            pull(wval)

        selected_wval <- categorize_wval(selected_wval)

        if (length(selected_wval) == 0) {
            selected_wval = NA
        }

        num_sites <- filtered_data_step_4_5_for_individ() |>
            filter(county == selected_id()) |>
            pull(num_sites)
        
        if (length(num_sites) == 0) {
            num_sites = 0
        }

        # HTML for rendering
        HTML(paste0(
            "<h3>", "County", "</h3>",
            "<h2>", selected_id(), "</h2>",
            "<h4>", selected_date(), "</h4>",
            "<p>", "Activity: ", selected_wval, " (", num_sites, " sites)", "</p>"
            ))

    })

    #####################################################
    # Functions
    #####################################################

    categorize_wval <- function(wval) {
        return(
            case_when(
                wval > 11   ~ "Very High",
                wval > 8    ~ "High",
                wval > 5.2  ~ "Moderate",
                wval > 2.5  ~ "Low",
                wval > -Inf ~ "Very Low",
                is.na(wval) ~ "Not available",
                TRUE        ~ "ERROR"
            )
        )
    }
}

shinyApp(ui, server)
