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
  
  fluidRow(
    column(width = 6, titlePanel("Cohort Inclusion Report")),
    column(width = 6, selectInput("datasource", "Data Source", choices = data_sources), style = "margin-top: 15px")
  ),
  
  p(em(cohort_name), style = "margin-bottom: 15px"),
  
  fluidRow(column(width = 6, offset = 5,
    shinyWidgets::radioGroupButtons(
      inputId = "level",
      label = "",
      selected = "person",
      individual = TRUE,
      choiceNames = c("By Person", "By Event"),
      choiceValues = c("person", "event")
    )
  )),
  
  fluidRow(column(width = 12, textOutput("upper_summary_text"))),

  fluidRow(
    column(width = 6, gt::gt_output("inclusion_table")),
    column(width = 6, echarts4rOutput('treemap'))
  ),
  
  fluidRow(column(width = 12, textOutput("lower_summary_text"))),
)

server <- function(input, output) {
  
  # when a box on the treemap is clicked this reactive will store the associated IDs as numbers
  rows_to_highlight <- reactive({
    if(!isTruthy(input$box_click$name) || input$box_click$name == "None") return(FALSE)
    as.numeric(stringr::str_split(input$box_click$name, ",")[[1]])
  })
  
  output$summary_table <- gt::render_gt(
    app_data[[input$datasource]][[input$level]]$summary_table %>% 
      gt::gt() %>% 
      gt::fmt_number(columns = dplyr::matches("count"), decimals = 0)
  )
  
  output$inclusion_table <- gt::render_gt(
    app_data[[input$datasource]][[input$level]]$inclusion_table %>% 
      gt::gt() %>% 
      gt::fmt_number(columns = dplyr::matches("count"), decimals = 0) %>% 
      {if (isTruthy(input$box_click$name))
        gt::tab_style(.,
          style = list(gt::cell_fill(color = "lightblue")),
          locations = gt::cells_body(rows = rows_to_highlight())
        ) else .}
  )
  
  output$treemap <- renderEcharts4r({
    app_data[[input$datasource]][[input$level]]$treemap_table %>% 
      e_charts() %>% 
      e_treemap(roam = F) %>% 
      e_on(query = ".", 
           handler = "function(params) {Shiny.setInputValue('box_click', {name: params.name});}")
  })
  
  output$upper_summary_text <- renderText({
    s <- app_data[[input$datasource]][[input$level]]$summary_table 
    glue::glue("Initial Count: {format(s$initial_index_events, big.mark=',', scientific=FALSE)}")
  })
  
  output$lower_summary_text <- renderText({
    s <- app_data[[input$datasource]][[input$level]]$summary_table 
    glue::glue("Final Count: {format(s$final_index_events, big.mark=',', scientific = FALSE)} ({s$percent_included})")
  })
}

shinyApp(ui = ui, server = server)





