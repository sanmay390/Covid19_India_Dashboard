library(highcharter)
library(shiny)
library(dplyr)
library(DT)
dataset2<-read.csv("covid_india.csv")
dataset1<-data.frame(dataset2)


ui <- fluidPage(

  titlePanel("covid-19 India Dashboard "),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("states", "States", choices = unique(dataset1$Name_of_State), selected = unique(dataset1$Name_of_State)[1])
    ),
    
    mainPanel(
      tableOutput("view"),
      highchartOutput("plot")
    )
  )
)



server <- function(input, output) {
  
  reactivedf <- reactive({
    filtereddf <- dataset1 %>%
      dplyr::filter(Name_of_State == input$states)
    filtereddf
  })
  
  output$plot <- renderHighchart({
    highchart() %>%
      hc_add_series(type = "column",color = "blue", reactivedf()$Active_Cases, name = "active") %>%
      hc_add_series(type = "column",color = "green", reactivedf()$Cured, name = "cured") %>%
      hc_add_series(type = "column",color = "red", reactivedf()$Deaths, name = "deaths") %>%
      hc_xAxis(labels = list(enabled = FALSE)) %>%
      hc_title(text = input$states)
  })
  
  output$view <- renderTable({
    reactivedf()
  })
}


shinyApp(ui, server)