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

# Define UI for application that draws a histogram
ui <- fluidPage( theme=shinytheme("superhero"),
    navbarPage(
        "Kolada Data",
        
        tabPanel(
            "KPI",
            mainPanel(
                h1("Available KPIs"),
                tableOutput("kpi1"),
            )
            ),
        tabPanel(
            "Municipality",
            mainPanel(
                h1("Available Muncipality Data"),
                tableOutput("munici1"),
            )
        ),
        tabPanel(
            "Kpi and Municipality",
            sidebarPanel(
                tags$h2("Input"),
                textInput("kpi","Enter KPI Id :", "N00945"),
                numericInput("munici","Enter Municipality id :", 1860),
                
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
        a=getMetadata("kpi")
        b=getMetadata("municipality")
        validate(need(input$kpi %in% a$values.id , "Invalid KPI , Please look into KPI tab and input a valid KPI Id"))
        validate(need(input$munici %in% b$values.id , "Invalid municipality id , Please look into Municipality tab and input a valid Municipality Id"))
        fetch_given_kpiandmuncipality_id(input$kpi,input$munici)
       
    })
    output$kpi1 = renderTable({
        rowset=c(5,14,2,3)
        temp=getMetadata("kpi")
        res=temp[rowset]
        names(res)=c("KPI Id","Title","Auspices","Description")
        res
    })
    output$munici1 = renderTable({
        rowset1=c(2,3,4)
        temp1=getMetadata("municipality")
        res1=temp1[rowset1]
        names(res1)=c("Municipality Id","Title","Type")
        res1
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
