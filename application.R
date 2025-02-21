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
data$construction <- as.numeric(data$construction)

# Ajouter des valeurs pour la Corse si nécessaire
if (!"2A" %in% data$Code) {
  data <- data %>% add_row(Code = "2A", Libellé = "Corse-du-Sud", Revenu = 22000, Chomage = 6.1, Transport = 10, construction = 0.5)
}
if (!"2B" %in% data$Code) {
  data <- data %>% add_row(Code = "2B", Libellé = "Haute-Corse", Revenu = 21000, Chomage = 7, Transport = 8, construction = 0.4)
}

# Charger les données géographiques
geojson_url <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements.geojson"
departements_sf <- st_read(geojson_url, quiet = TRUE)

departements_sf <- departements_sf %>% mutate(code = trimws(as.character(code)))

# Vérifier et fusionner les données
departements_sf <- left_join(departements_sf, data, by = c("code" = "Code"))

# Remplacer les NA dans les colonnes Revenu, Chômage, Transport et Construction par des valeurs par défaut
departements_sf <- departements_sf %>%
  mutate(
    Revenu = ifelse(is.na(Revenu), 0, Revenu),
    Chomage = ifelse(is.na(Chomage), 0, Chomage),
    Transport = ifelse(is.na(Transport), 0, Transport),
    construction = ifelse(is.na(construction), 0, construction)
  )

# Création des palettes de couleurs
bins_revenu <- c(19000, 20000, 21000, 22000, 23000, 24000, 25000, Inf)
colors_revenu <- c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c")
pal_revenu <- colorBin(palette = colors_revenu, bins = bins_revenu, na.color = "#f0f0f0", domain = departements_sf$Revenu)

bins_chomage <- c(0, 6.5, 8.5, 12.4, Inf)
colors_chomage <- c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6")
pal_chomage <- colorBin(palette = colors_chomage, bins = bins_chomage, na.color = "#f0f0f0", domain = departements_sf$Chomage)

bins_transport <- c(0, 3, 6, 10, 20, 35, 50, 70, 100)
colors_transport <- c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c", "#08306b")
pal_transport <- colorBin(palette = colors_transport, bins = bins_transport, na.color = "#f0f0f0", domain = departements_sf$Transport)

bins_construction <- c(0.2, 0.6, 1.0, 1.4, 2.1)
colors_construction <- c("#f7fbff", "#c6dbef", "#6baed6", "#08519c")
pal_construction <- colorBin(palette = colors_construction, bins = bins_construction, na.color = "#f0f0f0", domain = departements_sf$construction)

colnames(departements_sf) <- gsub("démo", "Demo", colnames(departements_sf))  # Renomme la colonne


# Trouver les départements avec les indices les plus élevés
max_revenu_dep <- departements_sf %>% filter(!is.na(Revenu)) %>% slice_max(order_by = Revenu, n = 1)
min_revenu_dep <- departements_sf %>% filter(Revenu == min(Revenu, na.rm = TRUE))
highest_chomage_dep <- departements_sf %>% filter(!is.na(Chomage)) %>% slice_max(order_by = Chomage, n = 1)
min_chomage_dep <- departements_sf %>% filter(Chomage == min(Chomage, na.rm = TRUE))
max_transport_dep <- departements_sf %>% filter(!is.na(Transport)) %>% slice_max(order_by = Transport, n = 1)
min_transport_dep <- departements_sf %>% filter(Transport == min(Transport, na.rm = TRUE))
max_construction_dep <- departements_sf %>% filter(!is.na(construction)) %>% slice_max(order_by = construction, n = 1)
min_construction_dep <- departements_sf %>% filter(construction == min(construction, na.rm = TRUE))
max_demo_dep <- departements_sf %>% filter(Demo == min(Demo, na.rm = TRUE))


revenu_text <- paste("Département avec le plus bas revenu moyen :", min_revenu_dep$nom, "(", round(min_revenu_dep$Revenu, 0), "€)")
chomage_text <- paste("Département avec le taux de chômage le plus élevé :", highest_chomage_dep$nom, "(", round(highest_chomage_dep$Chomage, 1), "%)")
transport_text <- paste("Département avec le plus bas indice de transport :", min_transport_dep$nom, "(", round(min_transport_dep$Transport, 0), ")")
construction_text <- paste("Département avec le plus bas taux de construction :", min_construction_dep$nom, "(", round(min_construction_dep$construction, 2), ")")
demo_text <- paste("Département avec le plus bas taux démographique :", max_demo_dep$nom, "(", round(max_demo_dep$Demo, 2), ")")
# Interface utilisateur

ui <- navbarPage("Comparaison Socio-Économique des départements francais en 2022", theme = shinytheme("flatly"),
                 
                 
                 tabPanel("Accueil",
                          fluidPage(
                            div("Présentation", class = "title", style = "text-align:center; font-size: 36px; font-weight: bold; margin-bottom: 20px;"),
                            fluidRow(
                              column(4, 
                                     
                                     h2("Description de l'étude"),
                                     p("Cette étude propose une analyse socio-économique des 96 départements de la métropole française afin d’accompagner les décideurs politiques dans l’identification des territoires nécessitant des investissements prioritaires. L’objectif est de favoriser une répartition plus équitable des ressources et de réduire les inégalités territoriales."),
                                     
                                     h2("Plan de l’étude"),
                                     p("- ", strong("Carte du Chômage"), " : Visualisation des taux de chômage par département pour identifier les zones où l’emploi est le plus fragile."),
                                     p("- ", strong("Carte des Revenus"), " : Comparaison des niveaux de revenus afin de mettre en évidence les disparités économiques entre territoires."),
                                     p("- ", strong("Carte du Transport"), " : Analyse des infrastructures de transport et de leur accessibilité pour comprendre leur impact sur le développement économique."),
                                     p("- ", strong("Carte de la Construction"), " : État des dynamiques de construction et d’urbanisation pour évaluer le développement immobilier et son influence sur la croissance locale.")
                              ),
                              column(8, 
                                     div(style = "display: flex; justify-content: center;"),leafletOutput("map_general", height = "600px"))
                            )
                          )
                 ),
                 tabPanel("Carte des Revenus",
                          fluidPage(
                            titlePanel("Indicateur Économique - Revenus"),
                            p("Le revenu moyen par habitant reflète le niveau de vie des populations et les inégalités économiques entre départements. Il permet d’identifier les territoires les plus aisés et ceux où les habitants disposent de moindres ressources financières. Ce critère est fondamental pour adapter les politiques publiques et orienter les investissements en matière de logement, d’éducation et d’infrastructures."),
                            
                            # Sélection du département
                            selectInput("select_departement_revenu", "Sélectionnez un département :", 
                                        choices = unique(departements_sf$nom), selected = "Paris"),
                            
                            textOutput("info_revenu"),
                            
                            fluidRow(
                              column(6,
                                     h3("Carte de l'Ile de France des Revenus par habitant", style = "text-align: center;"),
                                     
                                     leafletOutput("idf_carte_revenu", height = "600px")
                              ),
                              column(6, 
                                     h3("Carte francaise des Revenus par habitant", style = "text-align: center;"),
                                     
                                     leafletOutput("map_revenu", height = "600px"),
                                     
                              )
                              
                            )
                          ),
                          p(revenu_text,  style = "text-align: center; font-size: 28px; font-weight: bold; margin-top: 10px;")
                 ),
                 
                 
                 
                 tabPanel("Carte du Chômage",
                          fluidPage(
                            titlePanel("Indicateur du taux de Chômage"),
                            p("Le taux de chômage représente la proportion de la population active sans emploi et en recherche active de travail. Cet indicateur est essentiel pour évaluer la santé économique d’un territoire et identifier les zones où l’emploi est le plus fragile. Un taux de chômage élevé peut signaler des difficultés structurelles, tandis qu’un taux faible est souvent associé à une économie dynamique et attractive."),
                            
                            # Sélection du département
                            selectInput("select_departement_chomage", "Sélectionnez un département :", 
                                        choices = unique(departements_sf$nom), selected = "Paris"),
                            textOutput("info_chomage"),
                            
                            fluidRow(
                              column(6,
                                     h3("Analyse du Taux de Chômage", style = "text-align: center;"),
                                     
                                     leafletOutput("idf_carte_chomage", height = "600px")
                              ),
                              column(6, 
                                     h3("Carte du taux de Chômage", style = "text-align: center;"),
                                     
                                     leafletOutput("map_chomage", height = "600px")
                              )
                            )
                          ), 
                          p(chomage_text, style = "text-align: center; font-size: 28px; font-weight: bold; margin-top: 10px;")
                 ),
                 
                 tabPanel("Carte du Transport",
                          fluidPage(
                            titlePanel("Indicateur de Transport"),
                            p("L’accessibilité et la qualité des transports jouent un rôle clé dans le développement d’un territoire. Le taux de transport mesure l'accessibilité aux transports dans une région. Un bon réseau de transport améliore la mobilité des habitants, favorise le développement économique et réduit les disparités territoriales. À l’inverse, un déficit d’infrastructures peut freiner l’emploi et l’attractivité d’une région."),
                            
                            # Sélection du département
                            selectInput("select_departement_transport", "Sélectionnez un département :", 
                                        choices = unique(departements_sf$nom), selected = "Paris"),
                            textOutput("info_transport"),
                            fluidRow(
                              column(6,
                                     h3("Indice de Transport par Département", style = "text-align: center;"),
                                     
                                     leafletOutput("idf_carte_transport", height = "600px")
                              ),
                              column(6, 
                                     h3("Carte du taux de Transport", style = "text-align: center;"),
                                     
                                     leafletOutput("map_transport", height = "600px")
                              )
                            )
                          ), 
                          p(transport_text,style = "text-align: center; font-size: 28px; font-weight: bold; margin-top: 10px;"),
                 ),
                 
                 tabPanel("Carte de la Construction",
                          fluidPage(
                            titlePanel("Indicateur de Construction"),
                            p("L’activité de construction indique le dynamisme immobilier et l’urbanisation d’un département durant les 10 dernières années. Un taux élevé traduit un fort développement urbain, souvent lié à une croissance économique et démographique. À l’inverse, une faible construction peut signaler un manque d’attractivité ou des restrictions foncières freinant l’expansion du territoire."),
                            
                            # Sélection du département
                            selectInput("select_departement_construction", "Sélectionnez un département :", 
                                        choices = unique(departements_sf$nom), selected = "Paris"),
                            textOutput("info_construction"),
                            fluidRow(
                              column(6,
                                     h3("Taux de Construction par Département", style = "text-align: center;"),
                                     
                                     leafletOutput("idf_carte_construction", height = "600px")
                              ),
                              column(6, 
                                     h3("Carte du taux de Construction", style = "text-align: center;"),
                                     
                                     leafletOutput("map_construction", height = "600px")
                              )
                            )
                          ), 
                          p(construction_text,style = "text-align: center; font-size: 28px; font-weight: bold; margin-top: 10px;"),
                 ), 
                 tabPanel("Carte de la Démographie",
                          fluidPage(
                            titlePanel("Indicateur Démographique"),
                            p("Le taux de croissance démographique mesure l’évolution de la population d’un département durant les 10 dernières années. Une hausse rapide indique une région attractive en termes d’emplois et de qualité de vie, tandis qu’une baisse démographique peut révéler des difficultés économiques et un exode de la population. Cet indicateur permet d’anticiper les besoins en logements, services publics et infrastructures."),
                            
                            # Sélection du département
                            fluidRow(
                              column(4, 
                                     selectInput("select_departement_demo", "Sélectionnez un département :", 
                                                 choices = unique(departements_sf$nom), selected = "Paris")
                              )
                            ),
                            textOutput("info_demo"),
                            
                            # Cartes alignées côte à côte
                            fluidRow(
                              column(6, 
                                     h3("Carte Démographique de l'Ile de France", style = "text-align: center;"),
                                     leafletOutput("idf_carte_demo", height = "600px")
                              ),
                              column(6, 
                                     h3("Carte Démographique de la France", style = "text-align: center;"),
                                     leafletOutput("map_demo", height = "600px")
                              )
                            )
                            
                            
                          ), p(demo_text,  style = "text-align: center; font-size: 28px; font-weight: bold; margin-top: 10px;")
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
      addPolygons(data = highest_chomage_dep, color = "red", weight = 3, fillOpacity = 0, 
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
  observeEvent(input$select_departement_chomage, {
    req(input$select_departement_chomage)
    
    selected_dep <- departements_sf %>% filter(nom == input$select_departement_chomage)
    
    leafletProxy("map_chomage") %>%
      clearGroup("selection") %>%
      addPolygons(data = selected_dep, fillColor = "orange", color = "black", weight = 3, 
                  fillOpacity = 0.9, label = ~paste(nom, "<br>Taux de chômage :", round(Chomage, 1), "%"),
                  popup = ~paste("<strong>", nom, "</strong><br/>Taux de chômage :", round(Chomage, 1), "%"),
                  group = "selection")
  })
  output$info_chomage <- renderText({
    req(input$select_departement_chomage)
    
    selected_dep <- departements_sf %>% filter(nom == input$select_departement_chomage)
    
    paste("Le département sélectionné est", input$select_departement_chomage, 
          "avec un taux de chômage de", round(selected_dep$Chomage, 1), "%.")
  })
  
  
  # Carte des revenus
  output$map_revenu <- renderLeaflet({
    leaflet(departements_sf) %>%
      addPolygons(fillColor = ~pal_revenu(Revenu), color = "white", weight = 1,
                  fillOpacity = 0.8, label = ~paste(nom, "<br>Revenu moyen :", round(Revenu, 0), " €"),
                  popup = ~paste("<strong>", nom, "</strong><br/>Revenu moyen :", round(Revenu, 0), " €")) %>%
      addPolygons(data = min_revenu_dep, color = "red", weight = 3, fillOpacity = 0, 
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
  # Mise à jour de la carte générale des Revenus avec le département sélectionné
  observeEvent(input$select_departement_revenu, {
    req(input$select_departement_revenu)
    
    selected_dep <- departements_sf %>% filter(nom == input$select_departement_revenu)
    
    leafletProxy("map_revenu") %>%
      clearGroup("selection") %>%  # Supprime la sélection précédente
      addPolygons(data = selected_dep, fillColor = "orange", color = "black", weight = 3, 
                  fillOpacity = 0.9, label = ~paste(nom, "<br>Revenu moyen :", round(Revenu, 0), "€"),
                  popup = ~paste("<strong>", nom, "</strong><br/>Revenu moyen :", round(Revenu, 0), "€"),
                  group = "selection")
  })
  output$info_revenu <- renderText({
    req(input$select_departement_revenu)  # Vérifier qu'un département est sélectionné
    
    selected_dep <- departements_sf %>% filter(nom == input$select_departement_revenu)
    
    paste("Le département sélectionné est", input$select_departement_revenu, 
          "avec un revenu moyen de", round(selected_dep$Revenu, 0), "€.")
  })
  
  
  
  # Carte du transport
  output$map_transport <- renderLeaflet({
    leaflet(departements_sf) %>%
      addPolygons(fillColor = ~pal_transport(Transport), color = "white", weight = 1,
                  fillOpacity = 0.8, label = ~paste(nom, "<br>Indice de Transport :", round(Transport, 0)),
                  popup = ~paste("<strong>", nom, "</strong><br/>Indice de Transport :", round(Transport, 0))) %>%
      addPolygons(data = min_transport_dep, color = "red", weight = 3, fillOpacity = 0, 
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
  
  observeEvent(input$select_departement_transport, {
    req(input$select_departement_transport)
    
    selected_dep <- departements_sf %>% filter(nom == input$select_departement_transport)
    
    leafletProxy("map_transport") %>%
      clearGroup("selection") %>%
      addPolygons(data = selected_dep, fillColor = "orange", color = "black", weight = 3, 
                  fillOpacity = 0.9, label = ~paste(nom, "<br>Indice de Transport :", round(Transport, 0)),
                  popup = ~paste("<strong>", nom, "</strong><br/>Indice de Transport :", round(Transport, 0)),
                  group = "selection")
  })
  output$info_transport <- renderText({
    req(input$select_departement_transport)
    
    selected_dep <- departements_sf %>% filter(nom == input$select_departement_transport)
    
    paste("Le département sélectionné est", input$select_departement_transport, 
          "avec un indice de transport de", round(selected_dep$Transport, 0), ".")
  })
  
  
  # Carte du taux de construction
  output$map_construction <- renderLeaflet({
    leaflet(departements_sf) %>%
      addPolygons(fillColor = ~pal_construction(construction), color = "white", weight = 1,
                  fillOpacity = 0.8, label = ~paste(nom, "<br>Taux de Construction :", round(construction, 2)),
                  popup = ~paste("<strong>", nom, "</strong><br/>Taux de Construction :", round(construction, 2))) %>%
      addPolygons(data = min_construction_dep, color = "red", weight = 3, fillOpacity = 0, 
                  popup = ~paste("<strong>", nom, "</strong><br/>Taux de Construction :", round(construction, 2))) %>%
      addLegend(position = "bottomleft", pal = pal_construction, values = ~construction, title = "Taux de Construction",
                labFormat = labelFormat(suffix = ""), opacity = 1) %>%
      setView(lng = 2.2137, lat = 46.2276, zoom = 6)
  })
  
  output$idf_carte_construction <- renderLeaflet({
    selected_departements <- c("75", "77", "78", "91", "92", "93", "94", "95")
    leaflet(departements_sf %>% filter(code %in% selected_departements)) %>%
      addPolygons(fillColor = ~pal_construction(construction), color = "white", weight = 1,
                  fillOpacity = 0.8, label = ~paste(nom, "<br>Taux de Construction :", round(construction, 2)),
                  popup = ~paste("<strong>", nom, "</strong><br/>Taux de Construction :", round(construction, 2))) %>%
      setView(lng = 2.35, lat = 48.85, zoom = 9)
  })
  
  observeEvent(input$select_departement_construction, {
    req(input$select_departement_construction)
    
    selected_dep <- departements_sf %>% filter(nom == input$select_departement_construction)
    
    leafletProxy("map_construction") %>%
      clearGroup("selection") %>%
      addPolygons(data = selected_dep, fillColor = "orange", color = "black", weight = 3, 
                  fillOpacity = 0.9, label = ~paste(nom, "<br>Taux de Construction :", round(construction, 2)),
                  popup = ~paste("<strong>", nom, "</strong><br/>Taux de Construction :", round(construction, 2)),
                  group = "selection")
  })
  output$info_construction <- renderText({
    req(input$select_departement_construction)
    
    selected_dep <- departements_sf %>% filter(nom == input$select_departement_construction)
    
    paste("Le département sélectionné est", input$select_departement_construction, 
          "avec un taux de construction de", round(selected_dep$construction, 2), ".")
  })
  
  # Affichage du texte avec la valeur sélectionnée
  output$info_demo <- renderText({
    req(input$select_departement_demo)
    
    selected_dep <- departements_sf %>% filter(nom == input$select_departement_demo)
    
    paste("Le département sélectionné est", input$select_departement_demo, 
          "avec un taux de croissance démographique de", round(selected_dep$Demo, 2), "%.")
  })
  
  # Affichage du département avec la plus forte croissance démographique
  output$max_demo_dep <- renderText({
    paste("Département avec la plus forte croissance démographique :", max_demo_dep$nom, 
          "(", round(max_demo_dep$Demo, 2), "% )")
  })
  
  # Carte nationale de la Démographie
  output$map_demo <- renderLeaflet({
    leaflet(departements_sf) %>%
      addPolygons(fillColor = ~colorNumeric("Blues", departements_sf$Demo)(Demo), 
                  color = "white", weight = 1,
                  fillOpacity = 0.8, label = ~paste(nom, "<br>Taux de croissance démographique :", round(Demo, 2), "%"),
                  popup = ~paste("<strong>", nom, "</strong><br/>Taux de croissance démographique :", round(Demo, 2), "%")) %>%
      
      # Contour rouge pour le département avec la plus forte croissance démographique
      addPolygons(data = max_demo_dep, color = "red", weight = 3, fillOpacity = 0, 
                  popup = ~paste("<strong>", nom, "</strong><br/>Taux de croissance démographique :", round(Demo, 2), "%")) %>%
      
      addLegend(position = "bottomleft", 
                pal = colorNumeric("Blues", departements_sf$Demo), 
                values = departements_sf$Demo, title = "Taux de Croissance Démographique (%)",
                labFormat = labelFormat(suffix = " %"), opacity = 1) %>%
      setView(lng = 2.2137, lat = 46.2276, zoom = 6)
  })
  
  # Carte de l'Île-de-France
  output$idf_carte_demo <- renderLeaflet({
    selected_departements <- c("75", "77", "78", "91", "92", "93", "94", "95")
    
    leaflet(departements_sf %>% filter(code %in% selected_departements)) %>%
      addPolygons(fillColor = ~colorNumeric("Blues", departements_sf$Demo)(Demo), 
                  color = "white", weight = 1,
                  fillOpacity = 0.8, label = ~paste(nom, "<br>Taux de croissance démographique :", round(Demo, 2), "%"),
                  popup = ~paste("<strong>", nom, "</strong><br/>Taux de croissance démographique :", round(Demo, 2), "%")) %>%
      
      # Contour rouge pour le département avec la plus forte croissance démographique
      addPolygons(data = max_demo_dep, color = "red", weight = 3, fillOpacity = 0, 
                  popup = ~paste("<strong>", nom, "</strong><br/>Taux de croissance démographique :", round(Demo, 2), "%")) %>%
      
      setView(lng = 2.35, lat = 48.85, zoom = 9)
  })
  
  # Observer le département sélectionné et le mettre en surbrillance orange
  observeEvent(input$select_departement_demo, {
    req(input$select_departement_demo)
    
    selected_dep <- departements_sf %>% filter(nom == input$select_departement_demo)
    
    leafletProxy("map_demo") %>%
      clearGroup("selection") %>%
      addPolygons(data = selected_dep, fillColor = "orange", color = "black", weight = 3, 
                  fillOpacity = 0.9, label = ~paste(nom, "<br>Taux de croissance démographique :", round(Demo, 2), "%"),
                  popup = ~paste("<strong>", nom, "</strong><br/>Taux de croissance démographique :", round(Demo, 2), "%"),
                  group = "selection")
    
    leafletProxy("idf_carte_demo") %>%
      clearGroup("selection") %>%
      addPolygons(data = selected_dep, fillColor = "orange", color = "black", weight = 3, 
                  fillOpacity = 0.9, label = ~paste(nom, "<br>Taux de croissance démographique :", round(Demo, 2), "%"),
                  popup = ~paste("<strong>", nom, "</strong><br/>Taux de croissance démographique :", round(Demo, 2), "%"),
                  group = "selection")
  })
  
}

# Lancer l'application
shinyApp(ui, server)
