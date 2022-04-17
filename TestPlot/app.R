#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(DT)
library(tibble)
library(ggplot2)
library(plotly)
library(highcharter)
library(dplyr)

Base <- readRDS("Base.rds")

Groups <- readRDS("Groups.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Plot Compraison app"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h2("Select your group"),
            selectInput("Group",
                        "Name of your group:",
                        choices = c(sort(unique(Groups$Group)), ""),
                        selected = "",
                        multiple = F),
            selectInput("Plot",
                        "Name of your plot",
                        choices = Groups$plot),
            h2("Compare by"),
            radioButtons("Comparison",
                         "Choose how to compare",
                         choices = c("Habitat", "Distance")),
            uiOutput("ComparisonUI")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           leaflet::leafletOutput("Map"),
           textOutput("Comp")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    ## Comparisons UI
    output$Comp <- renderText({input$Comparison})



    output$ComparisonUI <- renderUI({

        if(input$Comparison == "Habitat") {
            message("Habitat selected")
            selectizeInput("Habitat",
                           label = "Select habitat type",
                           choices = sort(unique(Base$MajorHab)))
        }

        else if(input$Comparison == "Distance"){
            message("Distance selected")
            sliderInput("Distance", "Distance in meters:",
                        min = 300, max = 5000, value = 500
            )
        }


    })
    ### Selected data

     SelectedData <- reactive({
         shiny::req(input$Group)
         Data <- Groups %>%
             dplyr::filter(Group == input$Group) %>%
             dplyr::filter(plot == input$Plot) %>%
             mutate(Data = "Group") %>%
             select(-Group)

         CompareTo <- Base

         if(input$Comparison == "Habitat"){
             CompareTo <- CompareTo %>%
                 dplyr::filter(MajorHab == input$Habitat) %>%
                 mutate(Data = "Novana")
         }

         Data <- rbind(Data, CompareTo) %>%
             arrange(desc(Data))
         Data
     })



     output$Map <- leaflet::renderLeaflet({

         Categories <- sort(unique(SelectedData()$Data))

         factpal <- colorFactor(c("#ef8a62", "#67a9cf"), Categories)


         leaflet(data = SelectedData()) %>%
             addTiles() %>%
             leaflet::addCircleMarkers(color = ~factpal(Data))
     })
    #
    # ## Update the plot input
    #
     observe({
         SelectedGroup <- input$Group

         Choices <- Groups %>%
             dplyr::filter(Group == SelectedGroup) %>%
             pull(plot)

    #     # Can use character(0) to remove all choices
         if (SelectedGroup == "")
             SelectedGroup <- character(0)

    #     # Can also set the label and select items
         updateSelectInput(session, "Plot",
                           label = "Name of your plot",
                           choices = Choices,
                           selected = head(Choices, 1)
         )
     })
}

# Run the application
shinyApp(ui = ui, server = server)
