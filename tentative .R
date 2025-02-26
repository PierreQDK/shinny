"**Code R pour Shinny¨**"

library(readxl)
library(shiny)
library(leaflet)
library(sf)

data <-read_xlsx("/Users/pierrequintindekercadio/Desktop/shinny/TAUX CHOMAGE FRANCE _ ESPAGNE T4 2024.xlsx")


View(data)



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








# Page 2  : Chomage francais 

data <- data %>%
  mutate(Code = trimws(as.character(Code)))

# Ajouter des valeurs fixes pour la Corse (2A = 6.1, 2B = 7)
if (!"2A" %in% data$Code) {
  data <- data %>%
    add_row(Code = "2A", Libellé = "Corse-du-Sud", Chomage = 6.1)
}
if (!"2B" %in% data$Code) {
  data <- data %>%
    add_row(Code = "2B", Libellé = "Haute-Corse", Chomage = 7)
}

# Charger le fichier des départements en GeoJSON
geojson_url <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements.geojson"
departements_sf <- st_read(geojson_url)

# Nettoyer les codes dans le GeoJSON
departements_sf <- departements_sf %>%
  mutate(code = trimws(as.character(code)))

# Vérifier les différences entre les deux codes
print(setdiff(departements_sf$code, data$Code))
print(setdiff(data$Code, departements_sf$code))

# Joindre les données
departements_sf <- left_join(departements_sf, data, by = c("code" = "Code"))

# Vérifier si la colonne Chomage est présente
if (!"Chomage" %in% colnames(departements_sf)) {
  stop("La colonne 'Chomage' n'est pas présente après la jointure. Vérifiez les codes.")
}

# Assigner manuellement les valeurs pour la Corse si nécessaire
departements_sf <- departements_sf %>%
  mutate(
    Chomage = case_when(
      code == "2A" ~ 6.1,  # Corse-du-Sud
      code == "2B" ~ 7,    # Haute-Corse
      TRUE ~ Chomage       # Autres départements
    )
  )

# Palette discrète pour les catégories de taux de chômage
bins <- c(0, 6.5, 8.5, 12.4, Inf)
labels <- c("Moins de 6,5%", "De 6,5% à moins de 8,5%", "De 8,5% à moins de 12,4%", "12,4% ou plus")
colors <- c("#f2dede", "#f9bfbf", "#e47676", "#a50f15")

pal <- colorBin(palette = colors, bins = bins, na.color = "transparent", domain = departements_sf$Chomage)

ui <- fluidPage(
  titlePanel("Indicateur Socio-Économique"),
  
  fluidRow(
    column(6,  # Colonne pour le texte et la carte de l'Île-de-France
           h3("Analyse du Taux de Chômage"),
           p("Cette application affiche une carte des départements français avec le taux de chômage en pourcentage."),
           p("Le département avec le taux de chômage le plus élevé est :"),
           h4(highest_chomage),
           div(class = "idf-map-container",
               h4("Focus sur l'Île-de-France"),
               leafletOutput("idf_carte", height = "350px")
           )
    ),
    column(6, id = "carte-container",  # Colonne pour la carte principale
           leafletOutput("carte", height = "700px")
    )
  )
)

server <- function(input, output, session) {
  
  # Carte principale fixe
  output$carte <- renderLeaflet({
    leaflet(departements_sf, options = leafletOptions(zoomControl = FALSE, dragging = FALSE)) %>%
      addPolygons(
        fillColor = ~pal(Chomage),
        color = "black",
        weight = 1,
        fillOpacity = 0.8,
        label = ~paste(nom, "<br> Taux de chômage : ", round(Chomage, 1), "%"),
        popup = ~paste("<strong>", nom, "</strong><br/>Taux de chômage : ", round(Chomage, 1), "%")
      ) %>%
      addLegend(
        position = "topright",
        pal = pal,
        values = ~Chomage,
        title = "Taux de Chômage (%)",
        labFormat = labelFormat(suffix = " %"),
        opacity = 1
      ) %>%
      setView(lng = 2.2137, lat = 46.2276, zoom = 6)
  })
  
  # Carte secondaire (Île-de-France)
  output$idf_carte <- renderLeaflet({
    leaflet(departements_sf %>% filter(code %in% selected_departements),
            options = leafletOptions(zoomControl = FALSE, dragging = FALSE)) %>%
      addPolygons(
        fillColor = ~pal(Chomage),
        color = "black",
        weight = 1,
        fillOpacity = 0.8,
        label = ~paste(nom, "<br> Taux de chômage : ", round(Chomage, 1), "%"),
        popup = ~paste("<strong>", nom, "</strong><br/>Taux de chômage : ", round(Chomage, 1), "%")
      ) %>%
      setView(lng = 2.35, lat = 48.85, zoom = 9)
  })
}

shinyApp(ui, server)





library(shiny)
library(leaflet)
library(sf)
library(dplyr)

# Charger et nettoyer les données
data <- data %>%
  mutate(Code = trimws(as.character(Code)))

# Ajouter les valeurs fixes pour la Corse
if (!"2A" %in% data$Code) {
  data <- data %>% add_row(Code = "2A", Libellé = "Corse-du-Sud", Chomage = 6.1)
}
if (!"2B" %in% data$Code) {
  data <- data %>% add_row(Code = "2B", Libellé = "Haute-Corse", Chomage = 7)
}

# Charger le fichier GeoJSON
geojson_url <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements.geojson"
departements_sf <- st_read(geojson_url)

departements_sf <- departements_sf %>%
  mutate(code = trimws(as.character(code)))

# Vérifier les différences entre les codes
print(setdiff(departements_sf$code, data$Code))
print(setdiff(data$Code, departements_sf$code))

# Joindre les données
departements_sf <- left_join(departements_sf, data, by = c("code" = "Code"))

# Vérifier si la colonne Chomage est présente
if (!"Chomage" %in% colnames(departements_sf)) {
  stop("La colonne 'Chomage' n'est pas présente après la jointure. Vérifiez les codes.")
}

# Déterminer le département avec le taux de chômage le plus élevé
highest_chomage <- departements_sf %>% filter(!is.na(Chomage)) %>% 
  arrange(desc(Chomage)) %>% 
  slice(1) %>% 
  pull(nom)

# Palette pour la carte
bins <- c(0, 6.5, 8.5, 12.4, Inf)
labels <- c("Moins de 6,5%", "De 6,5% à moins de 8,5%", "De 8,5% à moins de 12,4%", "12,4% ou plus")
colors <- c("#f2dede", "#f9bfbf", "#e47676", "#a50f15")

pal <- colorBin(palette = colors, bins = bins, na.color = "transparent", domain = departements_sf$Chomage)

ui <- fluidPage(
  titlePanel("Indicateur Socio-Économique"),
  
  fluidRow(
    column(6,  
           h3("Analyse du Taux de Chômage"),
           p("Cette application affiche une carte des départements français avec le taux de chômage en pourcentage."),
           p("Le département avec le taux de chômage le plus élevé est :"),
           h4(highest_chomage),
           div(class = "idf-map-container",
               h4("Focus sur l'Île-de-France"),
               leafletOutput("idf_carte", height = "350px")
           )
    ),
    column(6, id = "carte-container", 
           leafletOutput("carte", height = "700px")
    )
  )
)

server <- function(input, output, session) {
  
  output$carte <- renderLeaflet({
    leaflet(departements_sf) %>%
      addPolygons(
        fillColor = ~pal(Chomage),
        color = "black",
        weight = 1,
        fillOpacity = 0.8,
        label = ~paste(nom, "<br> Taux de chômage :", round(Chomage, 1), "%"),
        popup = ~paste("<strong>", nom, "</strong><br/>Taux de chômage :", round(Chomage, 1), "%")
      ) %>%
      addLegend(
        position = "topright",
        pal = pal,
        values = ~Chomage,
        title = "Taux de Chômage (%)",
        labFormat = labelFormat(suffix = " %"),
        opacity = 1
      ) %>%
      setView(lng = 2.2137, lat = 46.2276, zoom = 6)
  })
  
  output$idf_carte <- renderLeaflet({
    selected_departements <- c("75", "77", "78", "91", "92", "93", "94", "95")
    leaflet(departements_sf %>% filter(code %in% selected_departements)) %>%
      addPolygons(
        fillColor = ~pal(Chomage),
        color = "black",
        weight = 1,
        fillOpacity = 0.8,
        label = ~paste(nom, "<br> Taux de chômage :", round(Chomage, 1), "%"),
        popup = ~paste("<strong>", nom, "</strong><br/>Taux de chômage :", round(Chomage, 1), "%")
      ) %>%
      setView(lng = 2.35, lat = 48.85, zoom = 9)
  })
}

shinyApp(ui, server)


