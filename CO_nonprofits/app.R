

library(shiny)
library(tidyverse)
library(sf)
library(leaflet)

pc_filtered <- readRDS("data/pc_filtered.RDS")
county_refined <- readRDS("data/county_refined.RDS")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Colorado Public Charities"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("var", "Choose Variable:", choices = c("Major Group" = "NTMAJ10",
                                                               "Contributions" = "CONT",
                                                               "Revenue" = "TOTREV",
                                                               "Expenses" = "EXPS")),
            hr(),
            p(em("This is annual data for the year 2018 from the NCCS Data Archive."))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput("map")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  pc <- reactive({
    pc_filtered %>% 
      mutate(variable = input$var)
    
  })
  
  pal <- reactive({
    
    if(input$var == "Major Group")
    return(colorFactor(palette = "Set1", domain = pc()$variable)(pc()$variable))
    
    if(input$var == "Contributions")
    return(colorQuantile(palette = "RdYlBu", reverse = TRUE, domain = pc()$variable,
                         n = 7)(pc()$variable))
    
    if(input$var == "Revenue")
    return(colorQuantile(palette = "RdYlBu", 
                         reverse = TRUE,domain = pc()$variable, n = 10)(pc()$variable))
    
    if(input$var == "Expenses")
    return(colorQuantile(palette = "RdYlBu", 
                         reverse = TRUE, domain = pc()$variable, n = 10)(pc()$variable))
    
    
    
  })
 
    
 
  
leg_title <- reactive({
  
  if(input$var == "Major Group")
  return(NULL)
  
  if(input$var == "Contributions")
  return("Annual Contributions Percentile")
  
  if(input$var == "Revenue")
  return("Annual Revenue Percentile")
  
  if(input$var == "Expenses")
  return("Annual Expenses Percentile")
  
  
  
})
    
    

  

    output$map <- renderLeaflet({
      leaflet() %>% 
        addTiles() %>% 
        addPolygons(data = county_refined, col = "black", fillOpacity = 0, opacity = 0.9,
                    popup = paste("County:", county_refined$NAME)) %>% 
        addCircleMarkers(data = pc(), radius = 6, opacity = 1,
                         color = "black",
                         stroke = TRUE,
                         weight = 0.2,
                         fillOpacity = 1, 
                         fillColor = pal(),
                         popup = paste("Name:", pc()$NAME, "<br>",
                                       "Address:", pc()$full_address, "<br>",
                                       "Major Group:", pc()$NTMAJ10, "<br>",
                                       "Contributions:", paste0("$", pc()$CONT), "<br>",
                                       "Revenue:", paste0("$",pc()$TOTREV), "<br>",
                                       "Expenses:", paste0("$",pc()$EXPS), "<br>",
                                       "Year: 2018")) %>%
        addLegend(pal = pal(), values = pc()$variable, opacity = 0.9,
                  title = leg_title(),
                  if(input$var == "NTMAJ10")
          {labFormat = function(type, cuts, p) {  # Here's the trick
          paste0(c("Arts, Culture, and Humanities", "Education", "Environment",
                   "Health", "Human Services", "International", "Mutual Benefit",
                   "Public and Societal Benefit", "Religion", "Unknown"))}
                  })
      
    })
      
    
      
      
    
}

# Run the application 
shinyApp(ui = ui, server = server)
