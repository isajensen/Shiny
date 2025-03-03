library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(shinythemes)
library(DT)
library(mongolite)
library(ggsoccer)

# Opret forbindelse til MongoDB-databaser
db_games <- mongo(collection = "games", db = "wyscout", url = "mongodb://localhost:27017")
db_matches <- mongo(collection = "matches", db = "wyscout", url = "mongodb://localhost:27017")

# Hent skuddata fra databasen
query_skuddata <- '{"$or": [{"type.primary": "shot"}, {"type.secondary": "shot"}]}'
fields_skuddata <- '{"matchId": 1, "team": 1, "shot": 1, "player.name": 1, "_id": 0}'
skuddata <- db_games$find(query_skuddata, fields = fields_skuddata)

# Hent kampdata fra databasen
kampdata <- db_matches$find('{}', '{ "_id": 1, "competitionId": 1, "seasonId": 1, "date": 1 }')

# Fladgør skuddata for nemmere håndtering
skuddata_flad <- skuddata %>%
  unnest(team, names_sep = "_") %>%
  unnest(player, names_sep = "_") %>%
  unnest(shot, names_sep = "_")

# Omdøb kolonne for at matche i merge
kampdata <- kampdata %>% rename(matchId = `_id`)

# Flet skuddata med kampdata og tilføj liga- og sæsoninformation
skuddata_samlet <- skuddata_flad %>%
  left_join(kampdata, by = "matchId") %>%
  mutate(
    league = ifelse(competitionId == 635, "Holland", "Polen"),
    season = case_when(
      seasonId %in% c(186215, 187502) ~ "2021/22",
      seasonId %in% c(188088, 188125) ~ "2022/23"
    )
  )

# Hent data til "Skud i en kamp"
query_skud_kamp <- '{"$or": [{"type.primary": "shot"}, {"type.secondary": "shot"}]}'
fields_skud_kamp <- '{"matchId": 1, "location": 1, "team": 1, "shot": 1, "type": 1, "_id": 0}'
skuddata_kamp <- db_games$find(query_skud_kamp, fields = fields_skud_kamp)

# Hent matchinformation til "Skud i en kamp"
kampinfo_skud <- db_matches$find('{}', fields = '{"_id": 1, "label": 1, "competitionId": 1, "seasonId": 1}')

# Rens og flet data til "Skud i en kamp"
skuddata_kamp_samlet <- skuddata_kamp %>%
  left_join(kampinfo_skud, by = c("matchId" = "_id")) %>%
  mutate(
    location_x = location$x,
    location_y = location$y,
    team_name = team$name,
    shot_isGoal = shot$isGoal,
    shot_onTarget = shot$onTarget,
    kamp_label = label  # Tilføj kamp_label fra kampinfo_skud
  ) %>%
  select(-location, -team, -shot) %>%
  mutate(
    liga = ifelse(competitionId == 635, "Holland", "Polen")
  )

# Hent data til "Afleveringer"
query_afleveringer <- '{"type.primary": "pass"}'
fields_afleveringer <- '{"matchId": 1, "team": 1, "pass": 1, "player": 1, "_id": 0}'
afleveringsdata <- db_games$find(query_afleveringer, fields = fields_afleveringer)

# Fladgør og ren afleveringsdata
afleveringsdata_flad <- afleveringsdata %>%
  mutate(
    team_name = team$name,
    player_name = player$name,
    pass_accurate = pass$accurate
  ) %>%
  select(-team, -player, -pass)

# Flet afleveringsdata med kampdata
afleveringsdata_samlet <- afleveringsdata_flad %>%
  left_join(kampdata, by = "matchId") %>%
  mutate(
    league = ifelse(competitionId == 635, "Holland", "Polen"),
    season = case_when(
      seasonId %in% c(186215, 187502) ~ "2021/22",
      seasonId %in% c(188088, 188125) ~ "2022/23"
    )
  )

# UI
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  tags$head(
    tags$style(HTML("
      body { background-color: white; color: black; }
      .navbar-default { background-color: #001F3F; border-color: #001F3F; }
      .navbar-default .navbar-brand { color: white; font-weight: bold; }
      .navbar-default .navbar-nav > li > a { color: white; }
      .btn-primary { background-color: #0073C2; border-color: #005A9E; }
      .btn-primary:hover { background-color: #005A9E; }
      .well { background-color: #F8F9FA; border-left: 5px solid #0073C2; }
    "))
  ),
  titlePanel("⚽ Fodbolddata fra Eredivisie og Ekstraklasa"),
  tabsetPanel(
    tabPanel("Mål og xG",
             sidebarLayout(
               sidebarPanel(
                 selectInput("input_league", "Vælg liga:", choices = c("Alle", unique(skuddata_samlet$league))),
                 selectInput("input_season", "Vælg sæson:", choices = c("Alle", unique(skuddata_samlet$season))),
                 selectInput("input_team", "Vælg hold:", choices = "Alle"),
                 sliderInput("input_num_players", "Antal spillere:", min = 5, max = 50, value = 20, step = 1),
                 downloadButton("download_player_data", "Download Data", class = "btn-primary")
               ),
               mainPanel(
                 plotOutput("goals_xg_plot"),
                 DTOutput("player_data_table")
               )
             )
    ),
    tabPanel("Afleveringer",
             sidebarLayout(
               sidebarPanel(
                 selectInput("liga", "Vælg liga:", choices = unique(afleveringsdata_samlet$league)),
                 selectInput("team_name", "Vælg hold:", choices = NULL),
                 selectInput("matchId", "Vælg kamp:", choices = NULL),
                 actionButton("compare", "Se data"),
                 verbatimTextOutput("time_output")
               ),
               mainPanel(
                 plotOutput("barPlot")
               )
             )
    ),
    tabPanel("Skud",
             fluidRow(
               column(3, 
                      wellPanel(
                        selectInput("liga_skud", "Vælg liga:", choices = unique(skuddata_kamp_samlet$liga)),
                        selectInput("hold_skud", "Vælg hold:", choices = NULL),
                        selectInput("kamp_skud", "Vælg kamp:", choices = NULL)
                      )
               ),
               column(9, 
                      plotOutput("skudPlot", width = "100%", height = "500px")
               )
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Mål og xG
  filtrerede_skud <- reactive({
    skuddata_samlet %>%
      filter((input$input_league == "Alle" | league == input$input_league),
             (input$input_season == "Alle" | season == input$input_season))
  })
  
  observe({
    tilgængelige_hold <- unique(filtrerede_skud()$team_name)
    updateSelectInput(session, "input_team",
                      choices = c("Alle", tilgængelige_hold),
                      selected = ifelse(input$input_team %in% tilgængelige_hold, input$input_team, "Alle"))
  })
  
  spilleroversigt <- reactive({
    filtrerede_skud() %>%
      filter(input$input_team == "Alle" | team_name == input$input_team) %>%
      group_by(player_name) %>%
      summarise(total_goals = sum(shot_isGoal, na.rm = TRUE),
                total_xG = sum(shot_xg, na.rm = TRUE)) %>%
      arrange(desc(total_goals)) %>%
      head(input$input_num_players)
  })
  
  output$goals_xg_plot <- renderPlot({
    ggplot(spilleroversigt(), aes(x = reorder(player_name, total_goals), y = total_goals)) +
      geom_bar(stat = "identity", fill = "#0073C2", color = "black", width = 0.6) +
      geom_point(aes(y = total_xG, color = "xG sum"), size = 5, alpha = 0.8) +
      scale_color_manual(name = "", values = c("xG sum" = "#EFC000FF")) +
      coord_flip() +
      labs(
        title = "Spillernes målscore og xG-sum",
        subtitle = "Sammenligning af faktiske mål og forventede mål (xG)",
        x = "Spillere",
        y = "Antal mål"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        axis.text.y = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5)
      ) +
      geom_text(aes(label = total_goals), hjust = -0.3, size = 5, fontface = "bold")
  })
  
  output$player_data_table <- renderDT({
    datatable(spilleroversigt(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$download_player_data <- downloadHandler(
    filename = function() {
      paste("player_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(spilleroversigt(), file, row.names = FALSE)
    }
  )
  
  # Afleveringer
  observe({
    updateSelectInput(session, "liga", choices = unique(afleveringsdata_samlet$league))
  })
  
  observeEvent(input$liga, {
    hold_valg <- afleveringsdata_samlet %>%
      filter(league == input$liga) %>%
      pull(team_name) %>%
      unique()
    
    updateSelectInput(session, "team_name", choices = hold_valg)
  })
  
  observeEvent(input$team_name, {
    kamp_valg <- afleveringsdata_samlet %>%
      filter(team_name == input$team_name) %>%
      pull(matchId) %>%
      unique()
    
    updateSelectInput(session, "matchId", choices = kamp_valg)
  })
  
  observeEvent(input$compare, {
    req(input$matchId)
    
    # Filtrer data baseret på valgt kamp
    filtrerede_data <- afleveringsdata_samlet %>%
      filter(matchId == input$matchId)
    
    # Beregn præcise og upræcise afleveringer
    afleveringsoversigt <- filtrerede_data %>%
      group_by(team_name) %>%
      summarise(
        good = sum(pass_accurate, na.rm = TRUE),
        inaccurate = sum(!pass_accurate, na.rm = TRUE),
        total = good + inaccurate,
        .groups = "drop"
      ) %>%
      pivot_longer(cols = c(good, inaccurate), names_to = "pass_type", values_to = "count") %>%
      mutate(percentage = count / total * 100)
    
    # Plot
    output$barPlot <- renderPlot({
      ggplot(afleveringsoversigt, aes(x = team_name, y = count, fill = pass_type)) +
        geom_bar(stat = "identity", position = "stack") +
        geom_text(aes(label = sprintf("%.0f %%", percentage), y = count / 2), 
                  color = "black", size = 5, fontface = "bold") +
        scale_fill_manual(values = c("good" = "darkseagreen", "inaccurate" = "coral2"),
                          name = "Afleveringer", labels = c("Præcis", "Upræcis")) +
        labs(title = "Afleveringer",
             x = "Hold", y = "Antal afleveringer") +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          text = element_text(color = "black"),
          axis.text = element_text(color = "black"),
          axis.title = element_text(color = "black", face = "bold"),
          plot.title = element_text(size = 18, face = "bold", color = "black"),
          legend.background = element_rect(fill = "white"),
          legend.key = element_rect(fill = "white"),
          legend.title = element_text(color = "black", face = "bold"),
          legend.text = element_text(color = "black")
        )
    })
  })
  
  # Skud i en kamp
  observeEvent(input$liga_skud, {
    hold_valg <- skuddata_kamp_samlet %>%
      filter(liga == input$liga_skud) %>%
      pull(team_name) %>%
      unique()
    
    updateSelectInput(session, "hold_skud", choices = hold_valg)
  })
  
  observeEvent(input$hold_skud, {
    kamp_valg <- skuddata_kamp_samlet %>%
      filter(team_name == input$hold_skud) %>%
      pull(kamp_label) %>%
      unique()
    
    updateSelectInput(session, "kamp_skud", choices = kamp_valg)
  })
  
  output$skudPlot <- renderPlot({
    req(input$kamp_skud) 
    
    data_filtreret <- skuddata_kamp_samlet %>%
      filter(kamp_label == input$kamp_skud)
    
    hold1 <- unique(data_filtreret$team_name)[1]
    hold2 <- unique(data_filtreret$team_name)[2]
    
    data_filtreret <- data_filtreret %>%
      mutate(location_x = ifelse(team_name == hold1, location_x, 100 - location_x))
    
    ggplot(data_filtreret, aes(x = location_x, y = location_y, color = team_name, 
                               shape = shot_onTarget, size = shot_isGoal)) +
      annotate_pitch(fill = "seagreen", alpha = 0.5, colour = "white") + 
      geom_point(alpha = 0.8) +
      scale_color_manual(values = c("red", "blue")) + 
      scale_shape_manual(values = c(4, 16)) +  
      scale_size_manual(values = c(3, 5)) +    
      coord_equal(ratio = 0.6) +
      theme_minimal() +
      ggtitle(paste("Skud i kampen:", input$kamp_skud))
  })
}

# Kør appen
shinyApp(ui = ui, server = server)

library(rsconnect)
deployApp()
