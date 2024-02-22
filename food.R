library(shiny)
library(leaflet)
library(dplyr)
library(shinyWidgets)

# Sample data frame of recipes with coordinates for demonstration
recipes <- data.frame(
  country = c("Italy", "Japan", "Mexico"),
  dish = c("Pizza Margherita", "Sushi", "Tacos"),
  ingredients = c("Flour, Tomatoes, Mozzarella", "Rice, Fish, Seaweed", "Corn, Beef, Salsa"),
  time = c("2 hours", "1 hour", "30 minutes"),
  lat = c(41.9028, 35.6895, 19.4326), # Sample latitudes
  lng = c(12.4964, 139.6917, -99.1332), # Sample longitudes
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  titlePanel("Culinary Explorer - Discover Recipes Around the World"),
  sidebarLayout(
    sidebarPanel(
      textInput("ingredientSearch", "Search by Ingredient"),
      actionButton("search", "Search")
      # Additional filters can be added here
    ),
    mainPanel(
      leafletOutput("map"),
      dataTableOutput("recipeTable")
    )
  )
)

server <- function(input, output, session) {
  # Initial map render without markers
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>% 
      setView(lng = 0, lat = 0, zoom = 2) # Set a default view
  })
  
  # Observe search button click
  observeEvent(input$search, {
    # Filter recipes based on ingredient search
    filteredRecipes <- if(input$ingredientSearch != ""){
      recipes %>% filter(grepl(input$ingredientSearch, ingredients, ignore.case = TRUE))
    } else {
      recipes
    }
    
    # Update the map with markers for filtered recipes
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(data = filteredRecipes, ~lng, ~lat, popup = ~paste(country, "-", dish))
    
    # Update the recipe table
    output$recipeTable <- renderDataTable({
      filteredRecipes
    })
  })
}

shinyApp(ui, server)
