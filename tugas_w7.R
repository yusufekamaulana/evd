library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(psych)
library(DT)
library(ggplot2)
library(hrbrthemes)
library(gridExtra)
library(leaflet)

df <- read.csv("Crab_combined.csv")
snow <- df[df$name == "snow crab", ]
hair <- df[df$name == "hair crab", ]
southern_tanner <- df[df$name == "southern Tanner crab", ]

ui <- shinyUI(
  dashboardPage(
    title = "Team J",
    skin = "black",
    dashboardHeader(
      title = HTML("<strong>Team J</strong>")
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Summary", tabName = "Summary", icon = icon("table")),
        menuItem("Visualization", tabName = "visualization", icon = icon("line-chart")),
        menuItem("Map Visualization", tabName = "mapvisualization", icon = icon("map"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "Summary",
                selectInput(inputId = "typeExploration",
                            label = "Choose a type of crab:",
                            choices = c("Hair", "Snow", "Southern Tanner")),
                sliderInput(inputId = "yearExploration", "Time Period:",
                            min = 1980, max = 2018,
                            value = c(1980, 2000),
                            step = 1, sep = "", width = '90%'),
                DTOutput("table", width = '90%')
        ),
        
        tabItem(tabName = "visualization",
                selectInput(inputId = "type",
                            label = "Choose a type of crab:",
                            choices = c("Hair", "Snow", "Southern Tanner")),
                
                sliderInput(inputId = "year", "Year:",
                            min = 1980, max = 2018,
                            value = 1980,
                            step = 1, sep = "", width = "90%"),
                
                plotOutput("linechart_haul", width = "90%"),
                
                div(style = "margin-top: 20px;", column(width = 12)),
                
                box(title = "Identify Characteristic of Crab", width = 11),
                
                plotOutput("boxplot", width = "90%", height = "900px"),
        ),
        
        tabItem(tabName = "mapvisualization",
                selectInput(inputId = "type_map",
                            label = "Choose a type of crab:",
                            choices = c("Hair", "Snow", "Southern Tanner")),
                
                sliderInput(inputId = "year_map", "Year:",
                            min = 1980, max = 2018,
                            value = c(1980, 1985),
                            step = 1, sep = "", width = "90%"),
                
                leafletOutput("map")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  typeInput <- reactive({
    switch(input$typeExploration,
           "Hair" = hair,
           "Snow" = snow,
           "Southern Tanner" = southern_tanner)
  })
  
  output$table <- renderDataTable({
    dataset <- typeInput()
    dataset <- dataset %>%
      select(-id, -latitude, -longitude, -sex, -name) %>%
      filter(year >= input$yearExploration[1] & year <= input$yearExploration[2]) %>%
      select(-year)
    
    summary_stats <- describe(dataset)
    summary_stats <- summary_stats %>%
      select(min, max, mean, median) %>%
      mutate(mean = round(mean, 1), median = round(median, 1))
    summary_stats <- datatable(summary_stats, rownames = TRUE,
                               options = list(dom = 't', searching = FALSE, paging = FALSE))
    summary_stats
  })
  
  output$linechart_haul <- renderPlot({
    dataset <- df %>%
      group_by(name, year, sex) %>%
      summarise(haul = sum(haul)) %>%
      filter(year >= 1980)
    if (input$type == "Snow") {
      snow <- dataset %>%
        filter(name == 'snow crab')
      ggplot(snow, aes(x = year, y = haul, group = sex, color = sex)) +
        geom_line() + ggtitle("Snow Crab's HAUL 1980-2018") +
        theme_ipsum() + ylab("HAUL")
    } else if (input$type == "Hair") {
      hair <- dataset %>%
        filter(name == 'hair crab')
      ggplot(hair, aes(x = year, y = haul, group = sex, color = sex)) +
        geom_line() + ggtitle("Hair Crab's HAUL 1980-2018") +
        theme_ipsum() + ylab("HAUL")
    } else if (input$type == "Southern Tanner") {
      southern <- dataset %>%
        filter(name == 'southern Tanner crab')
      ggplot(southern, aes(x = year, y = haul, group = sex, color = sex)) +
        geom_line() + ggtitle("Southern Tanner Crab's HAUL 1980-2018") +
        theme_ipsum() + ylab("HAUL")
    }
  })
  
  output$boxplot <- renderPlot({
    typeInput <- reactive({
      switch(input$type,
             "Hair" = "hair crab",
             "Snow" = "snow crab",
             "Southern Tanner" = "southern Tanner crab")
    })
    
    dataset <- df %>%
      filter(year == input$year & name == typeInput())
    
    bt_depth <- ggplot(dataset, aes(x = sex, y = bottom_depth, fill = sex)) +
      geom_boxplot(outlier.shape = NA) +
      theme_minimal() +
      scale_fill_manual(values = c("male" = "lightblue", "female" = "indianred"))
    
    bt_temp <- ggplot(dataset, aes(x = sex, y = bottom_temperature, fill = sex)) +
      geom_boxplot(outlier.shape = NA) +
      theme_minimal() +
      scale_fill_manual(values = c("male" = "lightblue", "female" = "indianred"))
    
    srfc_temp <- ggplot(dataset, aes(x = sex, y = surface_temperature, fill = sex)) +
      geom_boxplot(outlier.shape = NA) +
      theme_minimal() +
      scale_fill_manual(values = c("male" = "lightblue", "female" = "indianred"))
    
    combined_plot <- grid.arrange(bt_depth, bt_temp, srfc_temp, ncol = 1)
    combined_plot
  })
  
  output$map <- renderLeaflet({
    typeInput <- reactive({
      switch(input$type_map,
             "Hair" = "hair crab",
             "Snow" = "snow crab",
             "Southern Tanner" = "southern Tanner crab")})
    
    dataset<- df %>%
      filter(year >= input$year_map[1] & year <= input$year_map[2]) %>%
      filter(name==typeInput())
    
    leaflet(dataset) %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      setView(lat=58, lng=-170 , zoom=5) %>%
      addCircleMarkers(~longitude, ~latitude, 
                       color = "orange", stroke=FALSE, 
                       radius = sqrt(dataset$haul)*0.6)
  })
}

shinyApp(ui, server)
  
    