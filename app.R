
library(shiny)
library(leaflet)
library(ggmap)
library(DBI)

con <- dbConnect(odbc::odbc(), "SQL Server (DSN)")


#TIMESTAMP '2004-10-19 10:23:54'
# feedback <- "
# CREATE TABLE feedback (
# timestamp   timestamp,
# lat         double precision,
# lon         double precision,
# first       varchar(32),
# last        varchar(32),
# email       varchar(32),
# addressin   varchar(128),
# addressmap  varchar(128),
# comment     varchar(256)
# )"
#dbSendQuery(con, feedback)
#dbSendQuery(con, "drop table feedback")
# #geocode("1600 Pennsylvania Ave, Washington, DC", output = "latlona", source = "google")
# dd <- data.frame(
#   timestamp = Sys.time(),
#   first = "First",
#   last = NA,
#   email = "first.last@email.com",
#   addressin = "1600 Pennsylvania Ave, Washington DC",
#   lat = 39.41403,
#   lon = -77.40763,
#   comment = "Too loud, too low, too frequent.",
#   addressmap = "1600 pennsylvania ave nw, washington, dc 20500, usa"
# )
#dbWriteTable(con, "feedback", dd, append = TRUE)
#dbReadTable(con, "feedback")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("FAA NextGen Complaints"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         textInput(
           "firstname",
           "First Name",
           "",
           "100%"
         ),
         textInput(
           "lastname",
           "Last Name",
           "",
           "100%"
         ),
         textInput(
           "email",
           "Email*",
           "your.name@email.com",
           "100%"
         ),
         textInput(
           "address",
           "Address (Zip code)*", 
           "1600 pennsylvania avenue, washington dc",
           "100%"),
         textAreaInput(
           "comment",
           "Comment",
           "Too loud, too low, too frequent.",
           "100%",
           "200px",
           resize = "vertical"),
         actionButton(
           "submit",
           "Commit"
         ),
         helpText("* Required (If you don't want to submit your home address, please use a valid zipcode)"),
      width = "4"),
      mainPanel(
        tableOutput("table"),
        leafletOutput("mapPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  entry <- eventReactive(input$submit, {
    req(input$address, input$comment, input$email, cancelOutput = TRUE)
    xy <- geocode(input$address, output = "latlona", source = "google")
    dd <- data.frame(
      timestamp = Sys.time(),
      lat = xy$lat,
      lon = xy$lon,
      first = input$firstname,
      last = input$lastname,
      email = input$email,
      addressin = input$address,
      addressmap = xy$address,
      comment = input$comment
    )
    dbWriteTable(con, "feedback", dd, append = TRUE)
    dd
  })


   output$mapPlot <- renderLeaflet({
     leaflet() %>%
       addTiles() %>%  # Add default OpenStreetMap map tiles
       addPopups(lng = entry()$lon, lat = entry()$lat, popup = entry()$comment, options = popupOptions(openPopup=TRUE))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

