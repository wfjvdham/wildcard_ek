library(shiny)
library(tidyverse)
library(rvest)

toernooi_plaatsen <- c("vlissingen", "zutphen", "arnhem", "utrecht", "leeuwarden", 
                       "eindhoven", "heerenveen", "groningen")
men_first <- c(F, T, F, T, T, T, T, T)
have_ticket <- c("Dirk Boehlé / Steven van de Velde", 
                 "Alexander Brouwer / Robert Meeuwsen",
                 "Jasper Bouter / Christiaan Varenhorst",
                 "Neilton Moises Santos Merces / Harley Marques Silva",
                 "Sanne Keizer / Madelein Meppelink",
                 "Marleen Ramond-van Iersel / Joy Stubbe")

get_top_3 <- function(n, uitslagen_list, gender) {
  if (length(uitslagen_list[[n]]) > 0) {
    result <- case_when(
      men_first[n] && gender == "M" ~ uitslagen_list[[n]][1:3],
      !men_first[n] && gender == "M" ~ uitslagen_list[[n]][13:15],
      men_first[n] && gender != "M" ~ uitslagen_list[[n]][13:15],
      !men_first[n] && gender != "M" ~ uitslagen_list[[n]][1:3]
    )
    if (n == 4 && gender != "M") {
      result <- uitslagen_list[[n]][15:17]
    }
    if (n == 5 && gender != "M") {
      result <- uitslagen_list[[n]][14:16]
    }
    return(result)
  } 
}

ui <- fluidPage(
  titlePanel("Tussenstand voor de laatste wildcard voor het EK"),
  p("Hier wordt de tussenstand voor het laatste EK ticket weergegeven."),
  p("Wanneer er een nieuwe uitslag op eredivisiebeach.nl verschijnt wordt hij automatisch bijgewerkt."),
  p("Vanwege de trage eredivisiebeach.nl kan het even duren..."),
  h1("Vrouwen"),
  tableOutput("table_women"),
  h1("Mannen"),
  tableOutput("table_men")
)

server <- function(input, output) {
  
  uitslagen_data <- toernooi_plaatsen %>%
    map(~paste0("https://www.eredivisiebeach.nl/tour-info/", .x)) %>%
    map(read_html) %>%
    map(html_nodes, ".ranking_team_name") %>% 
    map(html_text)
  
  output$table_men <- renderTable({
    uitslagen_parsed <- data_frame(
      team = 1:length(toernooi_plaatsen) %>%
        map(get_top_3, uitslagen_data, "M") %>%
        unlist(),
      plaats = rep(c(1, 2, 3), length(team) / 3)
    )
    
    uitslagen_parsed %>%
      group_by(team) %>%
      summarise(eerste_plekken = sum(plaats == 1, na.rm = TRUE),
                tweede_plekken = sum(plaats == 2, na.rm = TRUE),
                derde_plekken = sum(plaats == 3, na.rm = TRUE)) %>%
      filter(!team %in% have_ticket) %>%
      arrange(desc(eerste_plekken), desc(tweede_plekken), desc(derde_plekken))
  })
  
  output$table_women <- renderTable({
    uitslagen_parsed <- data_frame(
      team = 1:length(toernooi_plaatsen) %>%
        map(get_top_3, uitslagen_data, "F") %>%
        unlist(),
      plaats = rep(c(1, 2, 3), length(team) / 3)
    )
    
    uitslagen_parsed %>%
      group_by(team) %>%
      summarise(eerste_plekken = sum(plaats == 1, na.rm = TRUE),
                tweede_plekken = sum(plaats == 2, na.rm = TRUE),
                derde_plekken = sum(plaats == 3, na.rm = TRUE)) %>%
      filter(!team %in% have_ticket) %>%
      arrange(desc(eerste_plekken), desc(tweede_plekken), desc(derde_plekken))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

