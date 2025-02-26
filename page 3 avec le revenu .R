library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readxl)

# Charger les données des revenus
data <- read_xlsx("/Users/pierrequintindekercadio/Desktop/shinny/TAUX CHOMAGE FRANCE _ ESPAGNE T4 2024.xlsx")

# Normaliser le format des codes départementaux
data$Code <- as.character(data$Code)  # Assurer que les codes restent des chaînes

data$Code <- ifelse(nchar(data$Code) == 1, paste0("0", data$Code), data$Code)  # Ajouter un zéro si nécessaire

data$Revenu <- as.numeric(data$Revenu)

# Ajouter des valeurs pour la Corse si elles ne sont pas présentes
if (!"2A" %in% data$Code) {
  data <- data %>% add_row(Code = "2A", Libellé = "Corse-du-Sud", Revenu = 22000)
}
if (!"2B" %in% data$Code) {
  data <- data %>% add_row(Code = "2B", Libellé = "Haute-Corse", Revenu = 21000)
}

# Charger les données géographiques
geojson_url <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements.geojson"
departements_sf <- st_read(geojson_url, quiet = TRUE)

departements_sf <- departements_sf %>%
  mutate(code = trimws(as.character(code)))

# Vérifier et fusionner les données
print(unique(departements_sf$code))
print(unique(data$Code))

departements_sf <- left_join(departements_sf, data, by = c("code" = "Code"))

# Vérifier les valeurs manquantes après la jointure
print(sum(is.na(departements_sf$Revenu)))

# Remplacer les NA dans la colonne Revenu par 0 (ou une valeur spécifique si nécessaire)
departements_sf$Revenu[is.na(departements_sf$Revenu)] <- 0

# Création de la palette de couleurs bleues avec les tranches demandées
bins <- c(19000, 20000, 21000, 22000, 23000, 24000, 25000, Inf)
labels <- c("19,000-20,000 €", "20,000-21,000 €", "21,000-22,000 €", "22,000-23,000 €", "23,000-24,000 €", "24,000-25,000 €", ">= 25,000 €")
colors <- c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c")

pal <- colorBin(palette = colors, bins = bins, na.color = "#f0f0f0", domain = departements_sf$Revenu)

# Trouver le département avec le plus haut revenu
max_revenu_dep <- departements_sf %>% filter(!is.na(Revenu)) %>% slice_max(order_by = Revenu, n = 1)

title_text <- paste("Département avec le plus haut revenu moyen:", max_revenu_dep$nom, "(", round(max_revenu_dep$Revenu, 0), "€)")

# Interface Shiny
ui <- fluidPage(
  titlePanel("Indicateur Économiques"),
  fluidRow(
    column(6,
           h3("Analyse des Revenus par Habitant"),
           p(title_text),
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
        fillColor = ~pal(Revenu),
        color = "white",
        weight = 1,
        fillOpacity = 0.8,
        label = ~paste(nom, "<br> Revenu moyen : ", round(Revenu, 0), " €"),
        popup = ~paste("<strong>", nom, "</strong><br/>Revenu moyen : ", round(Revenu, 0), " €")
      ) %>%
      addLegend(
        position = "bottomleft",
        pal = pal,
        values = ~Revenu,
        title = "Revenu (€)",
        labFormat = labelFormat(suffix = " €"),
        opacity = 1
      ) %>%
      setView(lng = 2.2137, lat = 46.2276, zoom = 6)
  })
  
  # Sélection des départements de l'Île-de-France
  selected_departements <- c("75", "77", "78", "91", "92", "93", "94", "95")
  
  output$idf_carte <- renderLeaflet({
    leaflet(departements_sf %>% filter(code %in% selected_departements)) %>%
      addPolygons(
        fillColor = ~pal(Revenu),
        color = "white",
        weight = 1,
        fillOpacity = 0.8,
        label = ~paste(nom, "<br> Revenu moyen : ", round(Revenu, 0), " €"),
        popup = ~paste("<strong>", nom, "</strong><br/>Revenu moyen : ", round(Revenu, 0), " €")
      ) %>%
      setView(lng = 2.35, lat = 48.85, zoom = 9)
  })
}

shinyApp(ui, server)

