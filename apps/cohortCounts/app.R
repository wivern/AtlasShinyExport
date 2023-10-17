library(shiny)
library(dplyr)
library(echarts4r)
library(reactable)

source("read_data.R")
cohort_name <- readr::read_file(file.path("data", "cohort_name.txt"))
app_data <- read_data(path = "data")

# get the list of unique data source keys
data_sources <- list.files("data", pattern = ".json$") %>% 
  stringr::str_remove("_by_(person|event).json$") %>% 
  stringr::str_unique()

ui <- fluidPage(
  shinyjs::useShinyjs(),
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
  
  fluidRow(column(width = 12,
    "Having", 
    tags$div(style="display:inline-block", selectInput("any_all", "", c("any", "all"), selectize = F, width = "80px")),
    "of selected criteria",
    tags$div(style="display:inline-block", selectInput("passed_failed", "", c("passed", "failed"), selectize = F, width = "100px"))
  )),

  fluidRow(
    column(width = 6, reactableOutput("inclusion_table")),
    column(width = 6, textOutput("count_in_selected_subset_text"), echarts4rOutput('treemap'))
  ),
  
  fluidRow(column(width = 12, textOutput("lower_summary_text"))),
)

server <- function(input, output) {
  
  # when a box on the treemap is clicked this reactive will store the associated IDs as numbers
  # rows_to_highlight <- reactive({
  #   if(!isTruthy(input$box_click$name) || input$box_click$name == "None") return(FALSE)
  #   as.numeric(stringr::str_split(input$box_click$name, ",")[[1]])
  # })
  
  output$count_in_selected_subset_text <- renderText({
    if(!isTruthy(input$box_click$name)) return(NULL)
    req(input$box_click$name)
    
    x <- app_data[[input$datasource]][[input$level]]$treemap_table %>% 
      mutate(total = sum(value), percent = round(100*value/total, 2)) %>% 
      filter(name == input$box_click$name) %>% 
      mutate(value = format(value, big.mark=',', scientific = FALSE),
             percent = paste0(percent, "%"))
      
    if (input$box_click$name == "None") {
      glue::glue("Number of {input$level}s not matching any inclusion rules: {x$value} ({x$percent})")
    } else {
      glue::glue("Number of {input$level}s matching inclusion rules [{input$box_click$name}]: {x$value} ({x$percent})")
    }
  })
  
  # output$summary_table <- gt::render_gt(
  #   app_data[[input$datasource]][[input$level]]$summary_table %>% 
  #     gt::gt() %>% 
  #     gt::fmt_number(columns = dplyr::matches("count"), decimals = 0)
  # )
  
  output$inclusion_table <- renderReactable({
    app_data[[input$datasource]][[input$level]]$inclusion_table %>% 
      {reactable(., selection = "multiple", onClick = "select", defaultSelected = seq_len(nrow(.)))} 
  })
    
  selected_rows <- reactive(getReactableState("inclusion_table", "selected"))
  
      # gt::gt() %>% 
      # gt::fmt_number(columns = dplyr::matches("count"), decimals = 0) %>% 
      # {if (isTruthy(input$box_click$name))
      #   gt::tab_style(.,
      #     style = list(gt::cell_fill(color = "lightblue")),
      #     locations = gt::cells_body(rows = rows_to_highlight())
      #   ) else .}
  
  treemap_table <- reactive({
    # app_data$SYNPUF_110k$person$treemap_table %>% # for testing
    app_data[[input$datasource]][[input$level]]$treemap_table %>%
      mutate(ids = stringr::str_replace(name, "None", "0") %>% stringr::str_split(",") %>% lapply(as.integer)) %>% 
      mutate(include_in_summary = case_when(
        input$any_all == "any" && input$passed_failed == "passed" ~ purrr::map_lgl(.data$ids, ~any(. %in% selected_rows())),
        input$any_all == "all" && input$passed_failed == "passed" ~ purrr::map_lgl(.data$ids, ~all(. %in% selected_rows())),
        input$any_all == "any" && input$passed_failed == "failed" ~ purrr::map_lgl(.data$ids, ~any(!(selected_rows() %in% .))),
        input$any_all == "all" && input$passed_failed == "failed" ~ purrr::map_lgl(.data$ids, ~all(!(selected_rows() %in% .))),
        TRUE ~ TRUE
      ))
    
  })
  
  output$treemap <- renderEcharts4r({
    shinyjs::runjs("Shiny.setInputValue('box_click', {name: false})")
    
    app_data[[input$datasource]][[input$level]]$treemap_table %>% 
      e_charts() %>% 
      e_treemap(roam = F) %>% 
      e_toolbox_feature(feature = "saveAsImage") %>% 
      # e_on(query = ".", handler = "function(params) {Shiny.setInputValue('box_click', {name: params.name});}") %>% 
      e_on(query = ".", handler = "function(params) {Shiny.setInputValue('box_click', {name: params.name});}", event = "mouseover") %>% 
      e_on(query = ".", handler = "function(params) {Shiny.setInputValue('box_click', {name: false});}", event = "mouseout")
  })
  
  output$upper_summary_text <- renderText({
    s <- app_data[[input$datasource]][[input$level]]$summary_table 
    glue::glue("Initial Count: {format(s$initial_index_events, big.mark=',', scientific=FALSE)}")
  })
  
  output$lower_summary_text <- renderText({
    denominator <- sum(treemap_table()$value)
    numerator <- treemap_table() %>% filter(include_in_summary) %>% pull(value) %>% sum()
    percent_included <- scales::label_percent()(numerator / denominator)
    # s <- app_data[[input$datasource]][[input$level]]$summary_table 
    glue::glue("Final Count: {format(numerator, big.mark=',', scientific = FALSE)} ({percent_included})")
  })
}

shinyApp(ui = ui, server = server)





