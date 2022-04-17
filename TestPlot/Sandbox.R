library(shiny)
library(leaflet)
library(shinyjs)
library(rgeos)

#crime <- read.csv("crime.csv", header=TRUE, stringsAsFactors=FALSE)

crime_df <-
  structure(
    list(
      Postcode = c(
        "WN1 3LU",
        "BL7 9YH",
        "BT36 7WE",
        "B6 6BS",
        "BD20 6LJ",
        "OL3 7EX",
        "M18 8RX",
        "BD23 1LY",
        "FY7 8PU",
        "M19 2FT",
        "FY4 2BL",
        "SK9 7QL",
        "HX4 8PQ",
        "LA12 7NP",
        "BL0 9UN",
        "DL10 7DP",
        "OL11 5SS",
        "OL12 6EL",
        "AB43 9QW",
        "SL2 3UR",
        "B15 3JH",
        "BB8 9HA",
        "B24 9LP",
        "B36 0JN",
        "BL7 0QG",
        "B43 5JG",
        "B23 5GJ",
        "WS4 1LH",
        "B45 8DP",
        "B46 2LH",
        "B60 2AQ",
        "SK9 1BY",
        "B63 3RA",
        "B67 7DJ",
        "B75 5HP",
        "B15 2ER",
        "BB8 0PH",
        "M33 4HZ",
        "B76 2TN",
        "BD23 1HN",
        "HG3 4AN",
        "BB6 8HN",
        "BB1 1BQ",
        "BB1 1DR",
        "BB1 1DU",
        "BB1 1DZ",
        "BB2 2NA",
        "BB1 1HN",
        "BB1 1LS",
        "BB1 1RU"
      ),
      crime = c(
        2L,
        7L,
        1L,
        2L,
        7L,
        1L,
        1L,
        10L,
        3L,
        1L,
        3L,
        1L,
        1L,
        1L,
        5L,
        3L,
        2L,
        4L,
        1L,
        1L,
        1L,
        6L,
        2L,
        3L,
        8L,
        4L,
        3L,
        3L,
        2L,
        1L,
        4L,
        3L,
        2L,
        1L,
        1L,
        2L,
        7L,
        3L,
        2L,
        6L,
        1L,
        16L,
        1L,
        2L,
        2L,
        1L,
        5L,
        3L,
        6L,
        3L
      ),
      Latitude = c(
        53.546367,
        53.613982,
        54.666528,
        52.504583,
        53.874506,
        53.53092,
        53.464027,
        53.966432,
        53.905032,
        53.443761,
        53.791965,
        53.312478,
        53.687511,
        54.198119,
        53.626566,
        54.407691,
        53.626655,
        53.630964,
        57.682959,
        51.555254,
        52.464528,
        53.861084,
        52.519347,
        52.500382,
        53.647756,
        52.543856,
        52.542246,
        52.602772,
        52.385279,
        52.521596,
        52.328812,
        53.324304,
        52.45635,
        52.491619,
        52.592831,
        52.462017,
        53.857882,
        53.40895,
        52.558527,
        53.962755,
        54.038168,
        53.820994,
        53.745103,
        53.74722,
        53.747415,
        53.745182,
        53.740827,
        53.745517,
        53.74505,
        53.740014
      ),
      Longitude = c(
        -2.620909,-2.406439,
        -5.981969,
        -1.89328,
        -1.91552,
        -2.009768,
        -2.161701,-2.026868,
        -3.039515,
        -2.19589,
        -3.03992,
        -2.236681,
        -1.883202,-3.113402,
        -2.323867,
        -1.717372,
        -2.212356,
        -2.157153,
        -2.011322,-0.616795,
        -1.936503,
        -2.171532,
        -1.820899,
        -1.752919,
        -2.39153,-1.939161,
        -1.848246,
        -1.952058,
        -2.003599,
        -1.639788,
        -2.055299,-2.225203,
        -2.068454,
        -1.973917,
        -1.833944,
        -1.910421,
        -2.156804,-2.348656,
        -1.789616,
        -2.008168,
        -1.763933,
        -2.457171,
        -2.47016,-2.471184,
        -2.471854,
        -2.47057,
        -2.489655,
        -2.472272,
        -2.467627,-2.470512
      )
    ),
    row.names = c(NA, 50L),
    class = "data.frame"
  )

postcodes = sort(unique(crime_df$Postcode))
crime = crime_df
coordinates(crime) = ~Longitude+Latitude
proj4string(crime) = CRS("+init=epsg:4326")
crime <- spTransform(x = crime, CRS = CRS("+init=epsg:27700"))

ui <- fluidPage(shinyjs::useShinyjs(),
                fluidPage(
                  # Give the page a title
                  titlePanel("Crime Map"),
                  mainPanel(leafletOutput("map")),

                  fluidRow(column(
                    3,
                    sliderInput(
                      "miles",
                      "Miles from location",
                      min = 1,
                      max = 100,
                      value = 10,
                      width = '120px'
                    ),
                    selectInput("postcode", 'postcode', choices = postcodes, selected = postcodes[2])
                  ))
                ))

server <- function (input, output, session) {
  output$map <- renderLeaflet({
    inside_df <- inside_df()
    leaflet(crime_df) %>%
      addTiles() %>%
      setView(lng = -1.525,
              lat = 55,
              zoom = 5) %>%
      addMarkers(
        lng = inside_df$Longitude,
        lat = inside_df$Latitude,
        popup = inside_df$Postcode
      )
  })
  circle <- reactive({
    postcode <- input$postcode
    location <- crime_df %>% dplyr::filter(Postcode == postcode) %>% dplyr::select(Latitude, Longitude)
    coordinates(location) <- ~Longitude+Latitude
    proj4string(location) = CRS("+init=epsg:4326")
    location <- spTransform(location, CRS = CRS("+init=epsg:27700"))
    circle <- gBuffer(location, width = input$miles * 1609.34)
    circle
  })
  inside_df <- reactive({
    circle = circle()
    inside = crime[circle,]   # find points inside the circle
    inside = spTransform(inside,  CRS("+init=epsg:4326"))
    inside_df = as.data.frame(inside)
    inside_df
  })
}

shinyApp(ui = ui, server = server)
