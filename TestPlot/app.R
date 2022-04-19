
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
library(dplyr)
library(ggtern)
library(sf)
library(forcats)
library(tidyr)

Base <- readRDS("Base.rds")

Groups <- readRDS("Groups.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Plot Comparison app"),

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
            uiOutput("ComparisonUI"),
            h2("Download report"),
            downloadButton("report", "Generate report")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            shiny::tabsetPanel(tabPanel(title = "Map",
                                        leaflet::leafletOutput("Map")),
                               tabPanel(title = "Ellenberg's Indicator Values",
                                        plotly::plotlyOutput(height = "600px", "BoxplotEllemberg")),
                               tabPanel(title = "Diversity measures",
                                        plotly::plotlyOutput("BoxplotRichness")),
                               tabPanel(title = "Grime values",
                                        plotOutput("GGTernPlot")))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    ## Comparisons UI

    output$ComparisonUI <- renderUI({

        if(input$Comparison == "Habitat") {
            selectizeInput("Habitat",
                           label = "Select habitat type",
                           choices = c(sort(unique(Base$habitat_name)), ""),
                           selected = "")
        }

        else if(input$Comparison == "Distance"){
            sliderInput("Distance", "Distance in meters:",
                        min = 300, max = 2000, value = 500, step = 100
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
                 dplyr::filter(habitat_name == input$Habitat) %>%
                 mutate(Data = "Novana")
             Data <- rbind(Data, CompareTo) %>%
                 arrange(desc(Data)) %>%
                 dplyr::mutate(Data = fct_relevel(Data, "Group", "Novana"))
             list(Data = Data)

         } else if(input$Comparison == "Distance") {
             TestPointBuffer <- Data %>%
                 st_buffer(dist = input$Distance) %>%
                 dplyr::select(Richness)

             CompareTo <- CompareTo %>%
                 st_crop(TestPointBuffer) %>%
                 st_intersection(TestPointBuffer) %>%
                 mutate(Data = "Novana") %>%
                 dplyr::select(-Richness.1)
             Data <- rbind(Data, CompareTo) %>%
                 arrange(desc(Data)) %>%
                 dplyr::mutate(Data = fct_relevel(Data, "Group", "Novana"))
             list(Data = Data, Buffer = TestPointBuffer)

         }


     })



     output$Map <- leaflet::renderLeaflet({

         Categories <- sort(unique(SelectedData()$Data$Data))

         factpal <- colorFactor(c("#ef8a62", "#67a9cf"), Categories)


         l <- leaflet(data = SelectedData()$Data) %>%
             addTiles() %>%
             leaflet::addCircleMarkers(color = ~factpal(Data), popup = ~SpeciesButton)

         if(input$Comparison == "Distance"){
             l <- l %>% leaflet::addPolylines(data = SelectedData()$Buffer, color = "red",
                                              weight = 1)
         } else if (input$Comparison != "Distance"){
             l
         }

     })

     ## Plots

     output$BoxplotRichness <- plotly::renderPlotly({

         Data <- SelectedData()$Data  %>%
             as.data.frame() %>%
             dplyr::select(-geometry, -Species) %>%
             pivot_longer(cols = c("Richness", "Artsindex"), names_to = "Diversity")

         G <- ggplot(Data, aes(x = "Plots", y = value)) +
             geom_boxplot() +
             geom_jitter(aes(color = Data), alpha = 0.5) +
             labs(x = NULL,
                  y = "Diversity estimate") +
             theme_bw() +
             facet_wrap(~Diversity, ncol = 1, scales = "free", strip.position = "right") +
             theme(axis.title.y=element_blank(), axis.text.y=element_blank(),
                   axis.ticks.y=element_blank())  +
             ggplot2::coord_flip()
         plotly::ggplotly(G)
     })

     output$BoxplotEllemberg <- plotly::renderPlotly({
         Data <- SelectedData()$Data  %>%
             as.data.frame() %>%
             dplyr::select(-geometry, -Species, - Richness) %>%
             pivot_longer(cols = c("L", "F", "R", "N", "N_R"), names_to = "Ellemberg") %>%
             mutate(Ellemberg = gsub(pattern = "N_R", replacement = "N/R", x = Ellemberg))

         G <- ggplot(Data, aes(x = "Plots", y = value)) +
             geom_boxplot() +
             geom_jitter(aes(color = Data), alpha = 0.5) +
             labs(x = NULL,
                  y = "Ellemberg value") +
             theme_bw() +
             facet_wrap(~Ellemberg, ncol = 1, scales = "free", strip.position = "right") +
             theme(axis.title.y=element_blank(), axis.text.y=element_blank(),
                   axis.ticks.y=element_blank())  +
             ggplot2::coord_flip()
         plotly::ggplotly(G)
     })

     ### Ternary outputs


     output$GGTernPlot <- renderPlot({
         Data <- SelectedData()$Data %>%
             as.data.frame() %>%
             dplyr::select(-geometry) %>%
             arrange(desc(Data)) %>%
             mutate(Data = fct_relevel(Data, "Novana", "Group"))

         print(ggtern(data = Data, aes(x = grime_R, y = grime_C, z = grime_S)) +
             geom_point(aes(color = rgb, size = Data, shape = Data), alpha = 0.5) + scale_color_identity() + ggtern::theme_rgbw() +
             zlab('Stress tolerator') + xlab('Ruderal') + ylab('Competitor'))
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

         ## Generate report
         output$report <- downloadHandler(
             # For PDF output, change this to "report.pdf"
             filename = "report.pdf",
             content = function(file) {
                 # Copy the report file to a temporary directory before processing it, in
                 # case we don't have write permissions to the current working dir (which
                 # can happen when deployed).
                 tempReport <- file.path(tempdir(), "report.Rmd")
                 file.copy("report.Rmd", tempReport, overwrite = TRUE)

                 # Set up parameters to pass to Rmd document
                 params <- list(Comparison = input$Comparison,
                                Group = input$Group,
                                Plot = input$Plot,
                                Habitat = input$Habitat,
                                Distance = input$Distance,
                                Data = SelectedData()$Data)

                 # Knit the document, passing in the `params` list, and eval it in a
                 # child of the global environment (this isolates the code in the document
                 # from the code in this app).
                 rmarkdown::render(tempReport, output_file = file,
                                   params = params,
                                   envir = new.env(parent = globalenv())
                 )
             }
         )
         ###

     })
}

# Run the application
shinyApp(ui = ui, server = server)
