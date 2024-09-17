library(shiny)

coltypes <- c("character", "factor", rep("numeric", 26),
              rep("character", 3), rep("numeric", 30))
eac_county <- read.csv("out/eac.csv", header = TRUE, colClasses = coltypes, 
                       na.strings = "") 
county_list <- eac_county$countyname
state_list <- unique(eac_county$state)

if (interactive()) {
  
  ui <- fluidPage(
    p("The checkbox group controls the select input"),
    checkboxGroupInput("inCheckboxGroup", "Input checkbox",
                       c("Item A", "Item B", "Item C")),
    selectInput("inSelect", "Select input",
                c("Item A", "Item B", "Item C")),
    selectInput(inputId = "state", label = "State", choices = state_list),
    selectInput(inputId = "county", label = "County", choices = county_list),
    plotOutput("plot1", width = "100%"),
    plotOutput("plot2", width = "100%")
  )
  
  server <- function(input, output, session) {
    #observe({
      #state_selected <- input$state
      #state_subset <- eac_county %>% 
        #filter(state == state_selected)
      #counties <- state_subset$countyname
      
      #updateSelectInput(session, inputID = "county", choices = county_list)
    #})
    
    observe({
      x <- input$inCheckboxGroup
      
      # Can use character(0) to remove all choices
      if (is.null(x))
        x <- character(0)
      
      # Can also set the label and select items
      updateSelectInput(session, "inSelect",
                        label = paste("Select input label", length(x)),
                        choices = x,
                        selected = tail(x, 1)
      )
      state_subset <- filter(eac_county, state == input$state)
      counties <- state_subset$countyname
      updateSelectInput(session, "county",
                        label = "County",
                        choices = counties
                        )
    })
  }
  
  shinyApp(ui, server)
}
