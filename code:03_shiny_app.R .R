library(shiny)
library(tidyverse)
library(janitor)

# Load and clean data
df <- read_csv("data/health_dataset.csv") %>% clean_names()

ui <- fluidPage(
  titlePanel("🌍 Health & Well-Being Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "range",
        "Filter countries by Obesity Rate (%)",
        min = floor(min(df$obesity_rate, na.rm = TRUE)),
        max = ceiling(max(df$obesity_rate, na.rm = TRUE)),
        value = c(floor(min(df$obesity_rate, na.rm = TRUE)), 
                  ceiling(max(df$obesity_rate, na.rm = TRUE))),
        step = 1
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Dataset", tableOutput("data_table")),
        tabPanel("Scatter Plot", plotOutput("scatter_plot"))
      )
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    df %>%
      filter(obesity_rate >= input$range[1],
             obesity_rate <= input$range[2])
  })
  
  output$data_table <- renderTable({
    filtered_data()
  })
  
  output$scatter_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = obesity_rate, y = mental_health_index)) +
      geom_point(color = "blue", size = 2) +
      geom_smooth(method = "lm", color = "red") +
      ggtitle("Obesity Rate vs Mental Health Index") +
      xlab("Obesity Rate (%)") +
      ylab("Mental Health Index")
  })
}

shinyApp(ui = ui, server = server)
