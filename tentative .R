"**Code R pour Shinny¨**"

library(readxl)
library(shiny)
library(leaflet)
library(sf)

data <-read_xlsx("/Users/pierrequintindekercadio/Desktop/shinny/TAUX CHOMAGE FRANCE _ ESPAGNE T4 2024.xlsx")


View(data)



library(shiny)
library(leaflet)
library(sf)

# Charger les départements de France (GeoJSON)
departements <- st_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements.geojson")

ui <- fluidPage(
  # Titre centré en grand
  tags$head(
    tags$style(HTML("
      .title {
        text-align: center;
        font-size: 36px;
        font-weight: bold;
        margin-bottom: 20px;
      }
      .container {
        display: flex;
        justify-content: space-between;
        align-items: flex-start;
      }
      .description {
        width: 40%;
        font-size: 18px;
        padding: 20px;
      }
      .map-container {
        width: 55%;
        height: 600px;
      }
    "))
  ),
  
  # Titre principal
  div("Comparaison des Départements", class = "title"),
  
  # Conteneur principal avec description à gauche et carte à droite
  div(class = "container",
      div(class = "description",
          h3("Description du projet"),
          p("Ce projet vise à comparer les départements français à travers différentes 
             statistiques et caractéristiques régionales. Sélectionnez un département 
             sur la carte pour afficher ses informations."),
          p("Ajoutez ici plus de détails sur les objectifs de votre projet et 
             les données utilisées.")
      ),
      div(class = "map-container",
          leafletOutput("carte", height = "100%")
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
        highlight = highlightOptions(weight = 3, color = "red", fillOpacity = 0.7),
        label = ~paste(code, "-", nom),  # Afficher "Numéro - Nom" au survol
        popup = ~paste("Département n°", code, "<br>", "Nom :", nom) # Popup au clic
      ) %>%
      setView(lng = 2.2137, lat = 46.2276, zoom = 6) # Centrage sur la France
  })
}

shinyApp(ui, server)


