library(shiny)

data_path <- "C:/Users/angus/OneDrive/Desktop/github/ORZ_dashboard"
tifs <- list.files(data_path, full.names = T, pattern = ".tif")
shps <- list.files(data_path, full.names = T, pattern = ".rds")
file_df <- data.frame(
                  files      = c(tifs, shps)
              ) %>%
              mutate(
                  name       = basename(files),
                  extension  = tools::file_ext(files)
              )

getwd()
file_df <- data.frame(
  files      = list.files(data_path, full.names = TRUE)
) %>%
  mutate(
    name       = basename(files),
    extension  = tools::file_ext(files)
  ) %>%
  filter(extension %in% c("tif", "shp"))

mask_open      <- readRDS("grid_mask_ldh_open.rds")%>%
  st_transform(4326) %>%
  mutate(label = "Final mask (LDH open)")
mask_shp <- mask_open$geometry %>%
  st_sf()
waterways      <- raster::raster("navig_waterways_480m_resampled.tif")

# sowb           <- sf::read_sf("state_owned_water_bottoms_simplified.shp")
ui <- fluidPage(
  # App title ----
  titlePanel("Hello Shiny!"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      actionButton("button1", "click me 1"), br(),
      actionButton("button2", "click me 2"), br(),
      downloadButton("d1", "Download Data 1"), br(),
      downloadButton("d2", "Download Data 2")
    ),
    mainPanel(
      helpText("Tables will apear when you push 'click me'"),
      br(),
      tableOutput("table1"),
      br(),
      tableOutput("table2")
    )))

server <- function(input, output, server) {

  mydata1 <- eventReactive(input$button1, {
    data.frame(col1 = c(1:4),
               col2 = letters[1:4],
               stringsAsFactors = FALSE)
  })
  output$table1 <- renderTable( mydata1() )

  mydata2 <- eventReactive(input$button2, {
    data.frame(col1 = c(5:8),
               col2 = letters[5:8],
               stringsAsFactors = FALSE)
  })
  output$table2 <- renderTable( mydata2() )

  #download data 1
  output$d1 <- downloadHandler(
    filename = function() {
      "data1.csv"
    },
    content = function(file) {
      write.csv(mydata1(), file, row.names = FALSE)
    }
  )

  #download data 2
  output$d2 <- downloadHandler(
    filename = function() {
      "data2.csv"
    },
    content = function(file) {
      write.csv(mydata2(), file, row.names = FALSE)
    }
  )

}
n()
#open directly in browser
runApp(list(ui = ui, server = server), launch.browser = T)
ui <- fluidPage(

  # App title ----
  titlePanel("Downloading Data"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Choose dataset ----
      selectInput("dataset", "Choose a dataset:",
                  choices = c("mask_shp", "waterways")),

      # Button
      downloadButton("downloadData", "Download")

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      plotOutput("plot")

    )

  )
)

server <- function(input, output) {

  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    # plot(input$dataset)
    switch(input$dataset,
           "mask_shp"  = mask_shp,
           "waterways" = waterways
           )
  })

  # Table of selected dataset ----
  output$plot <- renderPlot({
    datasetInput()
  })

  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".tif", sep = "")
    },
    content = function(file) {
      raster::writeRaster(datasetInput(), file=file)
      # write.csv(datasetInput(), file, row.names = FALSE)
    }
  )

}

shiny::shinyApp(ui = ui, server = server)
