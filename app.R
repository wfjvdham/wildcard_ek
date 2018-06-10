library(shiny)
library(tidyverse)
library(rvest)

toernooi_plaatsen <- c("vlissingen", "zutphen", "arnhem", "utrecht", "leeuwarden", 
                       "eindhoven", "heerenveen", "groningen")
men_first <- c(F, T, F, T, T, T, T, T)
have_ticket <- c("Dirk Boehlé / Steven van de Velde", 
                 "Alexander Brouwer / Robert Meeuwsen",
                 "Jasper Bouter / Christiaan Varenhorst",
                 "Neilton Moises Santos Merces / Harley Marques Silva")

get_top_3_men <- function(n, uitslagen_list) {
  if (length(uitslagen_list[[n]]) > 0) {
    if (men_first[n]) {
      return(uitslagen_list[[n]][1:3])
    } else {
      return(uitslagen_list[[n]][13:15])
    }
  } 
}

ui <- fluidPage(
  titlePanel("Tussenstand voor de laatste wildcard voor het EK"),
  p("Hier wordt de tussenstand voor het laatste EK ticket weergegeven."),
  p("Wanneer er een nieuwe uitslag op eredivisiebeach.nl verschijnt wordt hij automatisch bijgewerkt."),
  p("Vanwege de trage eredivisiebeach.nl kan het even duren..."),
  tableOutput("table")
)

server <- function(input, output) {
  
  output$table <- renderTable({
    uitslagen_data <- toernooi_plaatsen %>%
      map(~paste0("https://www.eredivisiebeach.nl/tour-info/", .x)) %>%
      map(read_html) %>%
      map(html_nodes, ".ranking_team_name") %>% 
      map(html_text)
    
    uitslagen_parsed <- data_frame(
      team = 1:length(toernooi_plaatsen) %>%
        map(get_top_3_men, uitslagen_data) %>%
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

