#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

a=getMetadata("kpi")
b=getMetadata("municipality")

# Define UI for application that draws a histogram
ui <- fluidPage( theme=shinytheme("superhero"),
                 navbarPage(
                   "Kolada Data",
                   
                   tabPanel(
                     "KPI",
                     mainPanel(
                       h1("Available KPIs"),
                       h6("The Kolada database contains 5,000 key figures, ie measures that are suitable for comparisons. The available key performance indicators are displayed below"),
                       tableOutput("kpi1"),
                     )
                   ),
                   tabPanel(
                     "Municipality",
                     mainPanel(
                       h1("Available Muncipality Data"),
                       h6("**Type :K stands for Swedish kommuner and L stands for landsting"),
                       tableOutput("munici1"),
                     )
                   ),
                   tabPanel(
                     "Kpi and Municipality",
                     sidebarPanel(
                       tags$h2("Input"),
                       selectInput("kpi","Select KPI",choices = a$KPI_ID),
                       selectInput("munici","Select Municipality id :", choices = b$Municipality_ID),
                       tags$h6("** Count contains the number of members associated with select muncipality id which is 1 unless selected id is a municipality group."),
                       tags$h6("\nGender contains 3 values T(Total),K(Kvinna) and M(Man)."),
                       tags$h6("\n Status is blank if weighted average is avialable , if not available it shows missing."),
                       tags$h6("Value is the weighted average.")
                       
                     ),
                     mainPanel(
                       h1("Information about selected KPI and municipality"),
                       tableOutput("result"),
                     )
                   )
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$result = renderTable({
    shiny::validate(need(input$kpi %in% a$KPI_ID , "Invalid KPI , Please look into KPI tab and input a valid KPI Id"))
    shiny::validate(need(input$munici %in% b$Municipality_ID , "Invalid municipality id , Please look into Municipality tab and input a valid Municipality Id"))
    fetch_given_kpiandmuncipality_id(input$kpi,input$munici)
    
  })
  output$kpi1 = renderTable({
   a
  })
  output$munici1 = renderTable({
    b
  })
}

# Run the application 
shinyApp(ui = ui, server = server)