library(shiny)
library(tidyverse)
library(rvest)

toernooi_plaatsen <- c("vlissingen", "zutphen", "arnhem", "utrecht", "leeuwarden", 
                       "eindhoven", "heerenveen", "groningen")
have_ticket <- c("Dirk Boehlé / Steven van de Velde", 
                 "Alexander Brouwer / Robert Meeuwsen",
                 "Jasper Bouter / Christiaan Varenhorst",
                 "Neilton Moises Santos Merces / Harley Marques Silva",
                 "Sanne Keizer / Madelein Meppelink",
                 "Marleen Ramond-van Iersel / Joy Stubbe")

scrape_data <- function() {
  data <- toernooi_plaatsen %>%
    map(~paste0("https://www.eredivisiebeach.nl/tour-info/", .x)) %>%
    map(read_html)
  
  team_names <- data %>%
    map(html_nodes, ".ranking_team_name") %>% 
    map(html_text)
  positions <- data %>%
    map(html_nodes, ".ranking_position") %>% 
    map(html_text)
  points <- data %>%
    map(html_nodes, ".ranking_points") %>% 
    map(html_text)
  
  df <- tibble(
    team_names = team_names %>%
      unlist(),
    positions = positions %>%
      unlist(),
    points = points %>%
      unlist()
  ) %>%
    mutate(points = as.integer(str_sub(points, 1, 3)),
           positions = as.integer(positions),
           gender = NA)
  write_rds(df, "./data.rds")
}

parse_data <- function() {
  df <- read_rds("./data.rds")
  
  current_gender <- "F"
  n_toernaments_played <- 1
  df$gender[1] <- current_gender
  for(i in 2:nrow(df)) {
    if (df$positions[i - 1] <= df$positions[i]) {
      df$gender[i] <- current_gender
    } else {
      #print(i)
      n_toernaments_played <- n_toernaments_played + 1
      if (!i %in% c(25, 49, 74)) {
        current_gender <- if_else(current_gender == "F", "M", "F")
      }
      df$gender[i] <- current_gender
    }
  }
  n_toernaments_played <- n_toernaments_played / 2
  list(df, n_toernaments_played)
}

calculate_standings <- function(df, n_toernaments_played, gender_arg) {
  df %>%
    filter(gender == gender_arg,
           !team_names %in% have_ticket) %>%
    arrange(positions) %>%
    group_by(team_names) %>%
    slice(1:4) %>%
    summarise(
      punten = sum(points),
      max_punten = as.integer(sum(
        sort(append(rep(768, 8 - n_toernaments_played), points), decreasing = TRUE)[1:4]
      )),
      aantal_deelnames = n()
    ) %>%
    # summarise(
    #   eerste = sum(positions == 1),
    #   tweede = sum(positions == 2),
    #   derde = sum(positions == 3)
    # ) %>%
    arrange(desc(punten)) %>%
    filter(max(punten) <= max_punten) %>%
    slice(1:6)
}

ui <- fluidPage(
  titlePanel("Tussenstand voor de laatste wildcard voor het EK"),
  p("Hier wordt de tussenstand voor het laatste EK ticket weergegeven. 
    De 4 beste resultaten voor een team zijn bij elkaar opgeteld.
    max_punten geeft aan hoeveel punten het team maximaal 
    (met alleen maar eeste plekken in de resterende toernooien)
    nog kan halen."),
  p("Wanneer er een nieuwe uitslag op eredivisiebeach.nl 
    verschijnt kan je hem bijwerken door op updaten te drukken"),
  h1("Vrouwen"),
  tableOutput("table_women"),
  h1("Mannen"),
  tableOutput("table_men"),
  actionButton(inputId = "refresh", label = "Update", class="btn btn-primary"),
  a("Maracuya IT", href="http://maracuya-it.com/")
)

server <- function(input, output) {
  
  rv <- reactiveValues(result_list = parse_data())
  
  output$table_men <- renderTable({
    calculate_standings(rv$result_list[[1]], rv$result_list[[2]], "M")
  })
  
  output$table_women <- renderTable({
    calculate_standings(rv$result_list[[1]], rv$result_list[[2]], "F")
  })
  
  observeEvent(input$refresh, {
    withProgress(message = 'Getting data...', value = 0, {
      scrape_data()
      incProgress(0.9, detail = "Calculating standings...")
      rv$result_list <- parse_data()
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

