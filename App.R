library(shiny)
library(leaflet)
library(sf)
library(readxl)
library(dplyr)
library(shinythemes)
library(ggplot2)
library(plotly)
library(DT)

# Charger les données des revenus, chômage et transport
data <- read_xlsx("www/TAUX CHOMAGE FRANCE _ ESPAGNE T4 2024.xlsx")
table_resume <- read_xlsx("www/table_resumee.xlsx")
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

print(colnames(departements_sf))  # Liste des colonnes disponibles


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

# Calcul des moyennes nationales des indicateurs
moyenne_nationale <- departements_sf %>%
  summarise(
    Chomage = mean(Chomage, na.rm = TRUE),
    Revenu = mean(Revenu, na.rm = TRUE),
    Transport = mean(Transport, na.rm = TRUE),
    Construction = mean(construction, na.rm = TRUE),
    Demo = mean(Demo, na.rm = TRUE)
  )


# Interface utilisateur

ui <- navbarPage(
  title = div(
    style = "width: 100%; text-align: center; font-size: 22px; font-weight: bold;",
    "Comparaison Socio-Économique des départements français en 2022"
  ),
  theme = shinytheme("flatly"),
  
  
  # 📌 Ajout de CSS pour justifier le texte
  tags$head(
    tags$style(HTML("
      .justified-text {
        text-align: justify;
      }
    "))
  ),
  
  tabPanel("Accueil",
           fluidPage(
             div("Présentation", class = "title", style = "text-align:center; font-size: 36px; font-weight: bold; margin-bottom: 20px;"),
             fluidRow(
               column(4, 
                      h2("Description de l'étude"),
                      p("Cette étude propose une analyse socio-économique des 96 départements de la métropole française afin d’accompagner les décideurs politiques dans l’identification des territoires nécessitant des investissements prioritaires. L’objectif est de favoriser une répartition plus équitable des ressources et de réduire les inégalités territoriales.", class = "justified-text"),
                      
                      h2("Plan de l’étude"),
                      p("- ", strong("Carte des Revenus"), " : Analyse des niveaux de revenus par département afin d’identifier les disparités économiques et sociales entre les territoires.", class = "justified-text"),
                      p("- ", strong("Carte du Chômage"), " : Visualisation des taux de chômage départementaux pour repérer les zones où l’emploi est le plus fragile.", class = "justified-text"),
                      p("- ", strong("Carte du Transport"), " : Analyse des infrastructures de transport et de leur accessibilité afin de comprendre leur rôle dans le développement économique et social.", class = "justified-text"),
                      p("- ", strong("Carte de la Construction"), " : Étude des dynamiques de construction et d’urbanisation pour mesurer leur impact sur l’aménagement du territoire et la croissance locale.", class = "justified-text"),
                      p("- ", strong("Carte de la Démographie"), " : Analyse des évolutions démographiques pour mieux comprendre les tendances de peuplement et leurs implications socio-économiques.", class = "justified-text"),
                      p("- ", strong("Tableau de Bord Graphique"), " : Synthèse visuelle des indicateurs clés pour une analyse comparative des départements français.", class = "justified-text"), 
                      p("-", strong("Annexe"), ": Ensemble des données des départements français.", class = "justified-text")
               ),
               column(8, 
                      div(style = "display: flex; justify-content: center;"), leafletOutput("map_general", height = "600px"))
             )
           )
  ),
  
  
  tabPanel("Carte des Revenus",
           fluidPage(
             titlePanel("Indicateur Économique - Revenus"),
             p("Le revenu moyen par habitant reflète le niveau de vie des populations et les inégalités économiques entre départements. Il permet d’identifier les territoires les plus aisés et ceux où les habitants disposent de moindres ressources financières. Ce critère est fondamental pour adapter les politiques publiques et orienter les investissements en matière de logement, d’éducation et d’infrastructures. Vous retrouverez le département avec le revenu le plus faible encadré en rouge sur la carte de la France à droite."),
             
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
                      h3("Carte francaise des Revenus par département", style = "text-align: center;"),
                      
                      leafletOutput("map_revenu", height = "600px"),
                      
               )
               
             )
           ),
           p(revenu_text,  style = "text-align: center; font-size: 28px; font-weight: bold; margin-top: 10px;")
  ),
  
  
  
  tabPanel("Carte du Chômage",
           fluidPage(
             titlePanel("Indicateur du taux de Chômage"),
             p("Le taux de chômage représente la proportion de la population active sans emploi et en recherche active de travail. Cet indicateur est essentiel pour évaluer la santé économique d’un territoire et identifier les zones où l’emploi est le plus fragile. Un taux de chômage élevé peut signaler des difficultés structurelles, tandis qu’un taux faible est souvent associé à une économie dynamique et attractive. Vous retrouverez le département avec le chomage le plus élevé encadré en rouge sur la carte de la France à droite."),
             
             # Sélection du département
             selectInput("select_departement_chomage", "Sélectionnez un département :", 
                         choices = unique(departements_sf$nom), selected = "Paris"),
             textOutput("info_chomage"),
             
             fluidRow(
               column(6,
                      h3("Carte de l'Ile de France du taux de Chômage", style = "text-align: center;"),
                      
                      leafletOutput("idf_carte_chomage", height = "600px")
               ),
               column(6, 
                      h3("Carte française du taux de chômage par département", style = "text-align: center;"),
                      
                      leafletOutput("map_chomage", height = "600px")
               )
             )
           ), 
           p(chomage_text, style = "text-align: center; font-size: 28px; font-weight: bold; margin-top: 10px;")
  ),
  
  tabPanel("Carte du Transport",
           fluidPage(
             titlePanel("Indicateur de Transport"),
             p("L’accessibilité et la qualité des transports jouent un rôle clé dans le développement d’un territoire. Le taux de transport mesure l'accessibilité aux transports en commun dans un département. Un bon réseau de transport améliore la mobilité des habitants, favorise le développement économique et réduit les disparités territoriales. À l’inverse, un déficit d’infrastructures peut freiner l’emploi et l’attractivité d’une région. Vous retrouverez le département avec l'indice de transport le plus faible encadré en rouge sur la carte de la France à droite."),
             
             # Sélection du département
             selectInput("select_departement_transport", "Sélectionnez un département :", 
                         choices = unique(departements_sf$nom), selected = "Paris"),
             textOutput("info_transport"),
             fluidRow(
               column(6,
                      h3("Carte de l'Ile de France de l'indice de Transport", style = "text-align: center;"),
                      
                      leafletOutput("idf_carte_transport", height = "600px")
               ),
               column(6, 
                      h3("Carte française de l'indice de transport par département", style = "text-align: center;"),
                      
                      leafletOutput("map_transport", height = "600px")
               )
             )
           ), 
           p(transport_text,style = "text-align: center; font-size: 28px; font-weight: bold; margin-top: 10px;"),
  ),
  
  tabPanel("Carte de la Construction",
           fluidPage(
             titlePanel("Indicateur de Construction"),
             p("L’activité de construction indique le dynamisme immobilier et l’urbanisation d’un département durant les 10 dernières années. Un taux élevé traduit un fort développement urbain, souvent lié à une croissance économique et démographique. À l’inverse, une faible construction peut signaler un manque d’attractivité ou des restrictions foncières freinant l’expansion du territoire. Vous retrouverez le département avec le taux de construction le plus faible encadré en rouge sur la carte de la France à droite."),
             
             # Sélection du département
             selectInput("select_departement_construction", "Sélectionnez un département :", 
                         choices = unique(departements_sf$nom), selected = "Paris"),
             textOutput("info_construction"),
             fluidRow(
               column(6,
                      h3("Carte de l'Ile de France de l'indice de Construction", style = "text-align: center;"),
                      
                      leafletOutput("idf_carte_construction", height = "600px")
               ),
               column(6, 
                      h3("Carte française de l'indice de Construction par département", style = "text-align: center;"),
                      
                      leafletOutput("map_construction", height = "600px")
               )
             )
           ), 
           p(construction_text,style = "text-align: center; font-size: 28px; font-weight: bold; margin-top: 10px;"),
  ), 
  tabPanel("Carte de la Démographie",
           fluidPage(
             titlePanel("Indicateur Démographique"),
             p("Le taux de croissance démographique mesure l’évolution de la population d’un département durant les 10 dernières années. Une hausse rapide indique une région attractive en termes d’emplois et de qualité de vie, tandis qu’une baisse démographique peut révéler des difficultés économiques et un exode de la population. Cet indicateur permet d’anticiper les besoins en logements, services publics et infrastructures.Vous retrouverez le département avec le taux de croissance démographique le plus faible encadré en rouge sur la carte de la France à droite."),
             
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
                      h3("Carte de l'Ile de France du taux de croissance Démographique", style = "text-align: center;"),
                      leafletOutput("idf_carte_demo", height = "600px")
               ),
               column(6, 
                      h3("Carte française du taux de croissance Démographique par département", style = "text-align: center;"),
                      leafletOutput("map_demo", height = "600px")
               )
             )
             
             
           ), p(demo_text,  style = "text-align: center; font-size: 28px; font-weight: bold; margin-top: 10px;")
  ), 
  
  tabPanel("Tableau de Bord Graphique",
           fluidPage(
             titlePanel("Visualisation des Indicateurs Socio-Économiques"),
             p("Cette section propose une visualisation interactive des indicateurs socio-économiques sous forme de jauges dynamiques. Chaque jauge permet de situer un département sélectionné par rapport aux autres départements français, en affichant la valeur minimale et maximale observée à l’échelle nationale.
Grâce au bouton Télécharger le Rapport, vous pouvez générer un fichier PDF contenant l’emplacement du département sélectionné sur une carte, toutes ses données socio-économiques et un tableau avec la moyenne nationale des départements français."),
             
             # Sélection du département
             fluidRow(
               column(4, 
                      selectInput("select_departement_graph", "Sélectionnez un département :", 
                                  choices = unique(departements_sf$nom), selected = "Paris")
               )
             ),
             div(style = "text-align: right; margin-top: -20px; margin-bottom: 20px;",
                 downloadButton("download_dashboard", 
                                shiny::HTML("<span style='font-weight: bold;'> Télécharger le Rapport </span>"), 
                                style = "width: 250px; height: 50px; background: #D29B42; color: white; 
                              padding: 10px 15px; border-radius: 8px;
                              font-size: 16px; font-weight: bold;"),
                 
                 # Affichage dynamique des jauges
                 fluidRow(
                   column(6,  plotlyOutput("gauge_chomage")),
                   column(6,  plotlyOutput("gauge_revenu"))
                 ),
                 fluidRow(
                   column(6, plotlyOutput("gauge_transport")),
                   column(6,  plotlyOutput("gauge_construction"))
                 ),
                 fluidRow(
                   column(6,  plotlyOutput("gauge_demo"))
                 ),
                 
             ), 
           ),
           
  ), 
  tabPanel("Annexe",
           fluidPage(
             titlePanel("Tableau Résumé des Indicateurs Socio-Économiques"),
             DTOutput("table_resumee")
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
      addPolygons(data = highest_chomage_dep, color = "red", weight = 3, fillOpacity = 0, 
                  popup = ~paste("<strong>", nom, "</strong><br/>Taux de chômage :", round(Chomage, 1), "%")) %>%
      addLegend(position = "bottomleft", pal = pal_chomage, values = ~Chomage, title = "Taux de Chômage (%)",
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
      addLegend(position = "bottomleft", pal = pal_construction, values = ~construction, title = "Taux de Construction (%)",
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
                values = departements_sf$Demo, title = "Croissance Démographique (%)",
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
  
  create_gauge_plotly <- function(value, min_val, max_val, title) {
    angle <- pi * (1 - (value - min_val) / (max_val - min_val))  # Calcul de l'angle
    
    x_end <- 0.5 + 0.33 * cos(angle)  # Ajustement de la flèche
    y_end <- 0.32 + 0.33 * sin(angle)  # Ajustement de la hauteur
    
    fig <- plot_ly(
      type = "indicator",
      mode = "gauge",
      value = value,
      domain = list(x = c(0, 1), y = c(0, 1)),  
      title = list(text = title, font = list(size = 18, color = "black"), x = 0.5, y = 1.2),  
      gauge = list(
        axis = list(range = list(min_val, max_val)),  
        bar = list(color = "transparent"),  
        steps = list(
          list(range = c(min_val, min_val + (max_val - min_val) * 0.5), color = "#d0e1f9"),  
          list(range = c(min_val + (max_val - min_val) * 0.5, min_val + (max_val - min_val) * 0.75), color = "#7bafd4"),  
          list(range = c(min_val + (max_val - min_val) * 0.75, max_val), color = "#08306b")  
        )
      )
    ) %>%
      layout(
        width = 360, height = 290,  # 📌 Ajustement de la taille pour éviter les collisions
        margin = list(l = 15, r = 15, t = 40, b = 40),  # 📌 Ajout de marge inférieure pour plus d'espace
        shapes = list(
          list(  # Flèche noire
            type = "line",
            x0 = 0.5, y0 = 0.32,  
            x1 = x_end, y1 = y_end,  
            line = list(color = "black", width = 6)  
          ),
          list(  # Cercle central
            type = "circle",
            xref = "paper", yref = "paper",
            x0 = 0.48, x1 = 0.52, y0 = 0.3, y1 = 0.34,
            fillcolor = "black",
            line = list(color = "black")
          )
        ),
        annotations = list(
          list(  # 📌 Valeur actuelle sous la flèche
            x = 0.5, y = 0.05,  
            text = paste0("<b>", round(value, 2), "</b>"),  
            font = list(size = 20),  
            showarrow = FALSE
          ),
          list(  # 📌 Min à gauche avec label
            x = 0.2, y = -0.15,  # 📌 Ajusté plus bas pour éviter les collisions
            text = paste0("<b>Min = ", round(min_val, 2), "</b>"),
            font = list(size = 12),  # 📌 Réduction de la taille du texte
            showarrow = FALSE
          ),
          list(  # 📌 Max à droite avec label
            x = 0.8, y = -0.15,  # 📌 Ajusté plus bas pour éviter les collisions
            text = paste0("<b>Max = ", round(max_val, 2), "</b>"),
            font = list(size = 12),  # 📌 Réduction de la taille du texte
            showarrow = FALSE
          )
        )
      )
    
    return(fig)
  }
  
  
  
  
  # 📌 Vérifier que les données sont bien chargées
  req(departements_sf)
  
  # 📌 Définition des valeurs min/max pour chaque indicateur
  min_chomage <- reactive({ min(departements_sf$Chomage, na.rm = TRUE) })
  max_chomage <- reactive({ max(departements_sf$Chomage, na.rm = TRUE) })
  
  min_revenu <- reactive({ min(departements_sf$Revenu, na.rm = TRUE) })
  max_revenu <- reactive({ max(departements_sf$Revenu, na.rm = TRUE) })
  
  min_transport <- reactive({ min(departements_sf$Transport, na.rm = TRUE) })
  max_transport <- reactive({ max(departements_sf$Transport, na.rm = TRUE) })
  
  min_construction <- reactive({ min(departements_sf$construction, na.rm = TRUE) })
  max_construction <- reactive({ max(departements_sf$construction, na.rm = TRUE) })
  
  min_demo <- reactive({ min(departements_sf$Demo, na.rm = TRUE) })
  max_demo <- reactive({ max(departements_sf$Demo, na.rm = TRUE) })
  
  # 📌 Observer le département sélectionné et générer les jauges
  output$gauge_chomage <- renderPlotly({
    req(input$select_departement_graph)
    selected_dep <- departements_sf %>% filter(nom == input$select_departement_graph)
    create_gauge_plotly(selected_dep$Chomage, min_chomage(), max_chomage(), "Taux de Chômage (%)")
  })
  
  output$gauge_revenu <- renderPlotly({
    req(input$select_departement_graph)
    selected_dep <- departements_sf %>% filter(nom == input$select_departement_graph)
    create_gauge_plotly(selected_dep$Revenu, min_revenu(), max_revenu(), "Revenu Moyen (€)")
  })
  
  output$gauge_transport <- renderPlotly({
    req(input$select_departement_graph)
    selected_dep <- departements_sf %>% filter(nom == input$select_departement_graph)
    create_gauge_plotly(selected_dep$Transport, min_transport(), max_transport(), "Indice de Transport")
  })
  
  output$gauge_construction <- renderPlotly({
    req(input$select_departement_graph)
    selected_dep <- departements_sf %>% filter(nom == input$select_departement_graph)
    create_gauge_plotly(selected_dep$construction, min_construction(), max_construction(), "Indice de Construction")
  })
  
  output$gauge_demo <- renderPlotly({
    req(input$select_departement_graph)
    selected_dep <- departements_sf %>% filter(nom == input$select_departement_graph)
    create_gauge_plotly(selected_dep$Demo, min_demo(), max_demo(), "Croissance Démographique (%)")
  })
  output$gauge_demo <- renderPlotly({
    req(input$select_departement_graph)
    selected_dep <- departements_sf %>% filter(nom == input$select_departement_graph)
    create_gauge_plotly(selected_dep$Demo, min_demo(), max_demo(), "Croissance Démographique (%)")
  })
  output_pdf_map_path <- tempfile(fileext = ".png")  # Création d'un fichier temporaire
  
  output_pdf_map_path <- tempfile(fileext = ".png")  # Création d'un fichier temporaire
  
  generate_map <- function(selected_dep_name) {
    req(selected_dep_name)  # Assurer qu'un département est bien sélectionné
    
    # 📌 Vérifier si le département sélectionné existe bien dans les données
    selected_dep <- departements_sf %>% filter(nom == selected_dep_name)
    
    if (nrow(selected_dep) == 0) {
      warning("Le département sélectionné n'existe pas dans departements_sf.")
      return(NULL)
    }
    
    # 📌 Générer la carte
    map_plot <- ggplot(departements_sf) +
      geom_sf(aes(fill = ifelse(nom == selected_dep_name, "Sélectionné", "Autres")), color = "black", size = 0.2) +
      scale_fill_manual(values = c("Autres" = "lightblue", "Sélectionné" = "orange")) +
      theme_void() +
      
      theme(legend.position = "none")
    
    # 📌 Sauvegarde en PNG
    ggsave(output_pdf_map_path, map_plot, width = 6, height = 5, dpi = 300)
    
    return(output_pdf_map_path)
  }
  
  
  output$download_dashboard <- downloadHandler(
    filename = function() {
      paste0("tableau_de_bord_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      library(rmarkdown)
      
      req(input$select_departement_graph)  # Vérifie qu'un département est sélectionné
      
      selected_dep_name <- input$select_departement_graph  # Nom du département sélectionné
      
      # 📌 Générer la carte avec le département en orange
      map_path <- generate_map(selected_dep_name)
      
      if (is.null(map_path)) {
        stop("Erreur : Impossible de générer la carte.")
      }
      
      # 📌 Récupération des valeurs du département sélectionné
      selected_dep <- departements_sf %>% filter(nom == selected_dep_name)
      
      if (nrow(selected_dep) == 0) {
        stop("Erreur : Département sélectionné introuvable dans les données.")
      }
      
      # 📌 Calcul des moyennes nationales
      moyenne_nationale <- departements_sf %>%
        summarise(
          Chomage = mean(Chomage, na.rm = TRUE),
          Revenu = mean(Revenu, na.rm = TRUE),
          Transport = mean(Transport, na.rm = TRUE),
          Construction = mean(construction, na.rm = TRUE),
          Demo = mean(Demo, na.rm = TRUE)
        ) %>%
        mutate(
          Chomage = round(Chomage, 1),
          Revenu = round(Revenu, 0),
          Transport = round(Transport, 0),
          Construction = round(Construction, 2),
          Demo = round(Demo, 2)
        )
      
      output_pdf_path <- tempfile(fileext = ".pdf")
      rmd_file <- "www/dashboard_template.Rmd"
      
      if (!file.exists(rmd_file)) {
        stop(paste0("❌ Le fichier RMarkdown n'existe pas à l'emplacement : ", rmd_file))
      }
      
      tryCatch({
        rmarkdown::render(
          input = rmd_file,
          output_format = "pdf_document",
          output_file = output_pdf_path,
          params = list(
            # 📌 Indicateurs pour le département sélectionné
            departement = selected_dep_name,
            chomage = round(selected_dep$Chomage, 1),
            revenu = format(round(selected_dep$Revenu, 0), big.mark = " "),
            transport = round(selected_dep$Transport, 0),
            construction = round(selected_dep$construction, 2),
            demo = round(selected_dep$Demo, 2),
            # 📌 Moyenne nationale
            chomage_moy = moyenne_nationale$Chomage,
            revenu_moy = format(moyenne_nationale$Revenu, big.mark = " "),
            transport_moy = moyenne_nationale$Transport,
            construction_moy = moyenne_nationale$Construction,
            demo_moy = moyenne_nationale$Demo,
            # 📌 Ajout de la carte
            map_path = map_path
          ),
          envir = new.env(parent = globalenv())
        )
        
        if (!file.exists(output_pdf_path)) {
          stop("❌ PDF introuvable après génération.")
        }
        
        file.copy(output_pdf_path, file, overwrite = TRUE)
        
      }, error = function(e) {
        stop("Erreur dans la génération du PDF : ", e$message)
      })
    }
  )
  
  
  
  output$table_resumee <- renderDT({
    datatable(table_resumee, options = list(pageLength = 10))
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)
