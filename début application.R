library(shiny)
library(leaflet)
library(sf)
library(readxl)
library(dplyr)
library(shinythemes)

# Charger les données des revenus, chômage et transport
data <- read_xlsx("/Users/pierrequintindekercadio/Desktop/shinny/TAUX CHOMAGE FRANCE _ ESPAGNE T4 2024.xlsx")

# Normaliser les codes départementaux
data$Code <- as.character(data$Code)
data$Code <- ifelse(nchar(data$Code) == 1, paste0("0", data$Code), data$Code)
data$Revenu <- as.numeric(data$Revenu)
data$Transport <- as.numeric(data$Transport)

# Ajouter des valeurs pour la Corse si nécessaire
if (!"2A" %in% data$Code) {
  data <- data %>% add_row(Code = "2A", Libellé = "Corse-du-Sud", Revenu = 22000, Chomage = 6.1, Transport = 10)
}
if (!"2B" %in% data$Code) {
  data <- data %>% add_row(Code = "2B", Libellé = "Haute-Corse", Revenu = 21000, Chomage = 7, Transport = 8)
}

# Charger les données géographiques
geojson_url <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements.geojson"
departements_sf <- st_read(geojson_url, quiet = TRUE)

departements_sf <- departements_sf %>% mutate(code = trimws(as.character(code)))

# Vérifier et fusionner les données
departements_sf <- left_join(departements_sf, data, by = c("code" = "Code"))

# Remplacer les NA dans les colonnes Revenu, Chômage et Transport par des valeurs par défaut
departements_sf <- departements_sf %>%
  mutate(
    Revenu = ifelse(is.na(Revenu), 0, Revenu),
    Chomage = ifelse(is.na(Chomage), 0, Chomage),
    Transport = ifelse(is.na(Transport), 0, Transport)
  )

# Création des palettes de couleurs
bins_revenu <- c(19000, 20000, 21000, 22000, 23000, 24000, 25000, Inf)
colors_revenu <- c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c")
pal_revenu <- colorBin(palette = colors_revenu, bins = bins_revenu, na.color = "#f0f0f0", domain = departements_sf$Revenu)

bins_chomage <- c(0, 6.5, 8.5, 12.4, Inf)
colors_chomage <- c("#f2dede", "#f9bfbf", "#e47676", "#a50f15")
pal_chomage <- colorBin(palette = colors_chomage, bins = bins_chomage, na.color = "transparent", domain = departements_sf$Chomage)

bins_transport <- c(0, 3, 6, 10, 20, 35, 50, 70, 100)
colors_transport <- c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c", "#08306b")
pal_transport <- colorBin(palette = colors_transport, bins = bins_transport, na.color = "#f0f0f0", domain = departements_sf$Transport)

# Trouver les départements avec les indices les plus élevés
max_revenu_dep <- departements_sf %>% filter(!is.na(Revenu)) %>% slice_max(order_by = Revenu, n = 1)
highest_chomage_dep <- departements_sf %>% filter(!is.na(Chomage)) %>% slice_max(order_by = Chomage, n = 1)
max_transport_dep <- departements_sf %>% filter(!is.na(Transport)) %>% slice_max(order_by = Transport, n = 1)

revenu_text <- paste("Département avec le plus haut revenu moyen :", max_revenu_dep$nom, "(", round(max_revenu_dep$Revenu, 0), "€)")
chomage_text <- paste("Département avec le taux de chômage le plus élevé :", highest_chomage_dep$nom, "(", round(highest_chomage_dep$Chomage, 1), "%)")
transport_text <- paste("Département avec le plus haut indice de transport :", max_transport_dep$nom, "(", round(max_transport_dep$Transport, 0), ")")

# Interface utilisateur
ui <- navbarPage("Comparaison Socio-Économique", theme = shinytheme("flatly"),
                 tabPanel("Accueil",
                          fluidPage(
                            div("Comparaison des Départements", class = "title", style = "text-align:center; font-size: 36px; font-weight: bold; margin-bottom: 20px;"),
                            fluidRow(
                              column(4, h3("Description du projet"),
                                     p("Ce projet vise à comparer les départements français à travers diverses statistiques et caractéristiques régionales."),
                                     p("Sélectionnez un département sur la carte pour voir ses informations.")),
                              column(8, leafletOutput("map_general", height = "600px"))
                            )
                          )
                 ),
                 tabPanel("Carte du Chômage",
                          fluidPage(
                            titlePanel("Indicateur Socio-Économique - Chômage"),
                            fluidRow(
                              column(6,
                                     h3("Analyse du Taux de Chômage"),
                                     p(chomage_text),
                                     leafletOutput("idf_carte_chomage", height = "600px")
                              ),
                              column(6, h4("Carte générale du Chômage"),
                                     leafletOutput("map_chomage", height = "600px")
                              )
                            )
                          )
                 ),
                 tabPanel("Carte des Revenus",
                          fluidPage(
                            titlePanel("Indicateur Économique - Revenus"),
                            fluidRow(
                              column(6,
                                     h3("Analyse des Revenus par Habitant"),
                                     p(revenu_text),
                                     leafletOutput("idf_carte_revenu", height = "600px")
                              ),
                              column(6, h4("Carte générale des Revenus"),
                                     leafletOutput("map_revenu", height = "600px")
                              )
                            )
                          )
                 ),
                 tabPanel("Carte du Transport",
                          fluidPage(
                            titlePanel("Indicateur de Transport"),
                            fluidRow(
                              column(6,
                                     h3("Indice de Transport par Département"),
                                     p(transport_text),
                                     leafletOutput("idf_carte_transport", height = "600px")
                              ),
                              column(6, h4("Carte générale du Transport"),
                                     leafletOutput("map_transport", height = "600px")
                              )
                            )
                          )
                 )
)

# Serveur
server <- function(input, output, session) {
  # Carte générale
  output$map_general <- renderLeaflet({
    leaflet(departements_sf) %>%
      addPolygons(fillColor = "lightblue", color = "black", weight = 1,
                  highlight = highlightOptions(weight = 3, color = "red", fillOpacity = 0.7),
                  label = ~paste(code, "-", nom), popup = ~paste("Département n°", code, "<br>Nom :", nom)) %>%
      setView(lng = 2.2137, lat = 46.2276, zoom = 6)
  })
  
  # Carte du chômage
  output$map_chomage <- renderLeaflet({
    leaflet(departements_sf) %>%
      addPolygons(fillColor = ~pal_chomage(Chomage), color = "black", weight = 1,
                  fillOpacity = 0.8, label = ~paste(nom, "<br>Taux de chômage :", round(Chomage, 1), "%"),
                  popup = ~paste("<strong>", nom, "</strong><br/>Taux de chômage :", round(Chomage, 1), "%")) %>%
      addLegend(position = "topright", pal = pal_chomage, values = ~Chomage, title = "Taux de Chômage (%)",
                labFormat = labelFormat(suffix = " %"), opacity = 1) %>%
      setView(lng = 2.2137, lat = 46.2276, zoom = 6)
  })
  
  output$idf_carte_chomage <- renderLeaflet({
    selected_departements <- c("75", "77", "78", "91", "92", "93", "94", "95")
    leaflet(departements_sf %>% filter(code %in% selected_departements)) %>%
      addPolygons(fillColor = ~pal_chomage(Chomage), color = "black", weight = 1,
                  fillOpacity = 0.8, label = ~paste(nom, "<br>Taux de chômage :", round(Chomage, 1), "%"),
                  popup = ~paste("<strong>", nom, "</strong><br/>Taux de chômage :", round(Chomage, 1), "%")) %>%
      setView(lng = 2.35, lat = 48.85, zoom = 9)
  })
  
  # Carte des revenus
  output$map_revenu <- renderLeaflet({
    leaflet(departements_sf) %>%
      addPolygons(fillColor = ~pal_revenu(Revenu), color = "white", weight = 1,
                  fillOpacity = 0.8, label = ~paste(nom, "<br>Revenu moyen :", round(Revenu, 0), " €"),
                  popup = ~paste("<strong>", nom, "</strong><br/>Revenu moyen :", round(Revenu, 0), " €")) %>%
      addLegend(position = "bottomleft", pal = pal_revenu, values = ~Revenu, title = "Revenu (€)",
                labFormat = labelFormat(suffix = " €"), opacity = 1) %>%
      setView(lng = 2.2137, lat = 46.2276, zoom = 6)
  })
  
  output$idf_carte_revenu <- renderLeaflet({
    selected_departements <- c("75", "77", "78", "91", "92", "93", "94", "95")
    leaflet(departements_sf %>% filter(code %in% selected_departements)) %>%
      addPolygons(fillColor = ~pal_revenu(Revenu), color = "white", weight = 1,
                  fillOpacity = 0.8, label = ~paste(nom, "<br>Revenu moyen :", round(Revenu, 0), " €"),
                  popup = ~paste("<strong>", nom, "</strong><br/>Revenu moyen :", round(Revenu, 0), " €")) %>%
      setView(lng = 2.35, lat = 48.85, zoom = 9)
  })
  
  # Carte du transport
  output$map_transport <- renderLeaflet({
    leaflet(departements_sf) %>%
      addPolygons(fillColor = ~pal_transport(Transport), color = "white", weight = 1,
                  fillOpacity = 0.8, label = ~paste(nom, "<br>Indice de Transport :", round(Transport, 0)),
                  popup = ~paste("<strong>", nom, "</strong><br/>Indice de Transport :", round(Transport, 0))) %>%
      addLegend(position = "bottomleft", pal = pal_transport, values = ~Transport, title = "Indice de Transport",
                labFormat = labelFormat(suffix = ""), opacity = 1) %>%
      setView(lng = 2.2137, lat = 46.2276, zoom = 6)
  })
  
  output$idf_carte_transport <- renderLeaflet({
    selected_departements <- c("75", "77", "78", "91", "92", "93", "94", "95")
    leaflet(departements_sf %>% filter(code %in% selected_departements)) %>%
      addPolygons(fillColor = ~pal_transport(Transport), color = "white", weight = 1,
                  fillOpacity = 0.8, label = ~paste(nom, "<br>Indice de Transport :", round(Transport, 0)),
                  popup = ~paste("<strong>", nom, "</strong><br/>Indice de Transport :", round(Transport, 0))) %>%
      setView(lng = 2.35, lat = 48.85, zoom = 9)
  })
}

# Lancer l'application
shinyApp(ui, server)
