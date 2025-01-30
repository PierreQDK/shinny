library(readxl)
library(shiny)
library(leaflet)
library(sf)
library(tidyverse)
data <-read_xlsx("/Users/pierrequintindekercadio/Desktop/shinny/TAUX CHOMAGE FRANCE _ ESPAGNE T4 2024.xlsx")


#View(data)

str(data)

data <- data %>%
  mutate(Revenu = as.numeric(gsub("[^0-9.]", "", Revenu)))



data <- data %>%
  mutate(
    Code = trimws(as.character(Code)),
    Revenu = gsub("[^0-9.]", "", Revenu),  # Supprimer tout caractère non numérique
    Revenu = as.numeric(Revenu)  # Convertir la colonne en numérique
  )

# Vérification des valeurs après conversion
print(head(data$Revenu))  # Affiche les premières valeurs de la colonne Revenu
print(summary(data$Revenu))  # Vérifie la distribution des valeurs

# Ajouter des valeurs fixes pour la Corse (2A = 22000, 2B = 21000)
if (!"2A" %in% data$Code) {
  data <- data %>%
    add_row(Code = "2A", Libellé = "Corse-du-Sud", Revenu = 22000)
}
if (!"2B" %in% data$Code) {
  data <- data %>%
    add_row(Code = "2B", Libellé = "Haute-Corse", Revenu = 21000)
}

# Charger le fichier des départements en GeoJSON
geojson_url <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements.geojson"
departements_sf <- st_read(geojson_url)

# Nettoyer les codes dans le GeoJSON
departements_sf <- departements_sf %>%
  mutate(code = trimws(as.character(code)))

# Joindre les données
departements_sf <- left_join(departements_sf, data, by = c("code" = "Code"))

# Vérifier si la colonne Revenu est présente
if (!"Revenu" %in% colnames(departements_sf)) {
  stop("La colonne 'Revenu' n'est pas présente après la jointure. Vérifiez les codes.")
}

# Palette discrète pour les catégories de revenus par habitant
bins <- c(0, 15000, 20000, 25000, 30000, Inf)
labels <- c("Moins de 15k", "15k - 20k", "20k - 25k", "25k - 30k", "30k et plus")
colors <- c("#f7fbff", "#deebf7", "#9ecae1", "#3182bd", "#08519c")

pal <- colorBin(palette = colors, bins = bins, na.color = "transparent", domain = departements_sf$Revenu)

ui <- fluidPage(
  titlePanel("Indicateur Socio-Économique : Revenu par Habitant"),
  
  fluidRow(
    column(6,  # Colonne pour le texte et la carte de l'Île-de-France
           h3("Analyse des Revenus par Habitant"),
           p("Cette application affiche une carte des départements français avec les revenus par habitant."),
           p("Le département avec le revenu par habitant le plus élevé est :"),
           h4(departements_sf %>%
                filter(!is.na(Revenu)) %>%
                slice_max(order_by = Revenu, n = 1) %>%
                pull(nom)),
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
        fillColor = ~pal(Revenu),
        color = "black",
        weight = 1,
        fillOpacity = 0.8,
        label = ~paste(nom, "<br> Revenu moyen : ", round(Revenu, 0), " €"),
        popup = ~paste("<strong>", nom, "</strong><br/>Revenu moyen : ", round(Revenu, 0), " €")
      ) %>%
      addLegend(
        position = "topright",
        pal = pal,
        values = ~Revenu,
        title = "Revenu (€)",
        labFormat = labelFormat(suffix = " €"),
        opacity = 1
      ) %>%
      setView(lng = 2.2137, lat = 46.2276, zoom = 6)
  })
  
  # Carte secondaire (Île-de-France)
  output$idf_carte <- renderLeaflet({
    leaflet(departements_sf %>% filter(code %in% selected_departements),
            options = leafletOptions(zoomControl = FALSE, dragging = FALSE)) %>%
      addPolygons(
        fillColor = ~pal(Revenu),
        color = "black",
        weight = 1,
        fillOpacity = 0.8,
        label = ~paste(nom, "<br> Revenu moyen : ", round(Revenu, 0), " €"),
        popup = ~paste("<strong>", nom, "</strong><br/>Revenu moyen : ", round(Revenu, 0), " €")
      ) %>%
      setView(lng = 2.35, lat = 48.85, zoom = 9)  # Centrer sur l'Île-de-France
  })
}

shinyApp(ui, server)



