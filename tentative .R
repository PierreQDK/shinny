"**Code R pour Shinny¨**"

library(readxl)
library(shiny)

data <-read_xlsx("/Users/pierrequintindekercadio/Desktop/shinny/TAUX CHOMAGE FRANCE _ ESPAGNE T4 2024.xlsx")


View(data)




ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .title {
        text-align: center;
        font-size: 50px;
        font-weight: bold;
        margin-top: 20px;
      }
    "))
  ),
  div("Comparaison des départements", class = "title")
)

server <- function(input, output, session) {
}

shinyApp(ui, server)





library(shiny)
library(leaflet)
library(sf)

# Charger les départements de France (GeoJSON)
departements <- st_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements.geojson")

# Texte d'explication des départements (exemple)
descriptions <- list(
  "Paris" = "Paris est la capitale de la France, célèbre pour la Tour Eiffel et son patrimoine culturel.",
  "Rhône" = "Le département du Rhône est connu pour Lyon, la gastronomie et le vignoble du Beaujolais.",
  "Gironde" = "La Gironde est célèbre pour Bordeaux et ses vins prestigieux.",
  "Bouches-du-Rhône" = "Situé en Provence, il comprend Marseille et ses calanques magnifiques."
)

ui <- fluidPage(
  titlePanel("Carte des Départements Français"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_dep", "Sélectionner un département :", 
                  choices = departements$nom, selected = NULL, multiple = FALSE),
      textOutput("description")  # Zone pour afficher la description
    ),
    
    mainPanel(
      leafletOutput("carte", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  
  output$carte <- renderLeaflet({
    leaflet(departements) %>%
      addPolygons(
        fillColor = "lightblue",
        color = "black",
        weight = 1,
        layerId = ~nom,
        highlight = highlightOptions(weight = 3, color = "red", fillOpacity = 0.7),
        label = ~nom
      ) %>%
      setView(lng = 2.2137, lat = 46.2276, zoom = 6) # Centrer sur la France
  })
  
  observeEvent(input$selected_dep, {
    selected_pol <- departements[departements$nom == input$selected_dep, ]
    
    leafletProxy("carte") %>%
      clearShapes() %>%
      addPolygons(data = departements,
                  fillColor = "lightblue",
                  color = "black",
                  weight = 1,
                  label = ~nom) %>%
      addPolygons(data = selected_pol,
                  fillColor = "red",
                  color = "black",
                  weight = 2,
                  fillOpacity = 0.5)
  })
  
  # Affichage du texte dynamique en fonction du département sélectionné
  output$description <- renderText({
    dep <- input$selected_dep
    if (is.null(dep) || dep == "") {
      return("Sélectionnez un département pour voir sa description.")
    }
    return(descriptions[[dep]] %||% "Aucune description disponible pour ce département.")
  })
}

shinyApp(ui, server)

