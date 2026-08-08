library(shiny)
library(tidyverse)

ui <- fluidPage(

    # Input for graph year
    selectInput("year", "Year:", choices = c(2026, 2025, 2024, 2023)),

    # The graph
    plotOutput("plot")
)

server <- function(input, output, session) {

    data <- read_csv("vt_rsv.csv") |>
    group_by(sample_collect_date) |>
    mutate(year = year(sample_collect_date)) |>
    filter(year == 2026)

    output$plot <- renderPlot({
        ggplot(
            data = data()
        ) + geom_line(
            mapping = aes(
                x = sample_collect_date,
                y = pcr_target_avg_conc
            )
        )
    })
}

shinyApp(ui, server)
