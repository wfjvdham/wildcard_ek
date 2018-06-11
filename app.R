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

ui <- fluidPage(
  titlePanel("Tussenstand voor de laatste wildcard voor het EK"),
  p("Hier wordt de tussenstand voor het laatste EK ticket weergegeven."),
  p("Wanneer er een nieuwe uitslag op eredivisiebeach.nl verschijnt wordt hij automatisch bijgewerkt."),
  p("Het laden kan het even duren..."),
  h1("Vrouwen"),
  tableOutput("table_women"),
  h1("Mannen"),
  tableOutput("table_men"),
  a("Maracuya IT", href="http://maracuya-it.com/")
)

server <- function(input, output) {
  
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
    team_names = t(as.matrix(team_names)) %>%
      unlist(),
    positions = t(as.matrix(positions)) %>%
      unlist(),
    points = t(as.matrix(points)) %>%
      unlist()
  ) %>%
     mutate(points = as.integer(str_sub(points, 1, 3)),
            positions = as.integer(positions),
            gender = NA)
  
  current_gender <- "F"
  df$gender[1] <- current_gender
  for(i in 2:nrow(df)) {
    if (df$positions[i - 1] <= df$positions[i]) {
      df$gender[i] <- current_gender
    } else {
      #print(i)
      if (!i %in% c(25, 49, 74)) {
        current_gender <- if_else(current_gender == "F", "M", "F")
      }
      df$gender[i] <- current_gender
    }
  }
  
  calculate_standings <- function(gender_arg) {
    df %>%
      filter(gender == gender_arg,
             !team_names %in% have_ticket) %>%
      arrange(positions) %>%
      group_by(team_names) %>%
      slice(1:4) %>%
      summarise(
        punten = sum(points),
        eerste = sum(positions == 1),
        tweede = sum(positions == 2),
        derde = sum(positions == 3),
        aantal_deelnames = n()
      ) %>%
      arrange(desc(punten), eerste, tweede, derde) %>%
      slice(1:6)
  }
  
  output$table_men <- renderTable({
    calculate_standings("M")
  })
  
  output$table_women <- renderTable({
    calculate_standings("F")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

