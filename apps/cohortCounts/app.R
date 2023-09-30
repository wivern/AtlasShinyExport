library(shiny)
library(dplyr)
library(echarts4r)

source("read_data.R")
cohort_name <- readr::read_file(file.path("data", "cohort_name.txt"))
app_data <- read_data(path = "data")

# get the list of unique data source keys
data_sources <- list.files("data", pattern = ".json$") %>% 
  stringr::str_remove("_by_(person|event).json$") %>% 
  stringr::str_unique()

ui <- fluidPage(

  titlePanel("Cohort Inclusion Report"),
    p(em(cohort_name), style = "margin-bottom: 15px"),

  sidebarLayout(
    sidebarPanel(
      selectInput("datasource", "Data Source", choices = data_sources)
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("By Person", echarts4rOutput('treemapByPerson')),
        tabPanel("By Event", echarts4rOutput('treemapByEvent'))
      )
    )
  )
)

server <- function(input, output) {
  
  output$treemapByPerson <- renderEcharts4r(
    app_data[[input$datasource]][["person"]]$treemap_table %>% 
      e_charts() %>% 
      e_treemap() 
  )
  
  output$treemapByEvent <- renderEcharts4r(
    app_data[[input$datasource]][["event"]]$treemap_table %>% 
      e_charts() %>% 
      e_treemap() 
  )
}

shinyApp(ui = ui, server = server)
