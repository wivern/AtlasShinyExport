
library(shiny)
library(dplyr)

# create a dataframe with all the app data in it using saved webapi json files
treemapData <- lapply(list.files("data"), jsonlite::read_json) # %>% 
  # combine data into a dataframe

ui <- fluidPage(

    titlePanel("Cohort Inclusion Report"),
    p(em("[PIONEER FUP] Newly diagnosed prostate cancer initiated delayed curative trt"), 
      style = "margin-bottom: 15px"),

    sidebarLayout(
        sidebarPanel(
            selectInput("datasource", "Data Source",
                        choices = c("STARR-DEID 1% LITE 08/13/2023" = "starr1pct",
                                    "STARR-DEID LITE 08/13/2023" = "starr",
                                    "SynPUF 2M" = "synpuf"))
        ),

        mainPanel(
          tabsetPanel(
            tabPanel("By Person", treemapOutput('treemapByPerson')),
            tabPanel("By Event", treemapOutput('treemapByEvent'))
        )
    )
  )
)

server <- function(input, output) {
  output$treemapByPerson <- renderTreemap(
    treemapData %>% 
      filter(group = "by person") %>% 
      treemap(treemapDatainclude_table = TRUE)
  )
  
  output$treemapByEvent <- renderTreemap(
    treemapData %>% 
      filter(group = "by person") %>% 
      treemap(treemapDatainclude_table = TRUE)
  )
}

shinyApp(ui = ui, server = server)
