library(shiny)
library(ggplot2)
library(plotly)
library(magick)


# Load your data
work_folder <- choose.dir()
setwd(work_folder)

# List all the .rda files in the directory
livedta_files <- list.files(pattern = "\\livedta-saved.csv$")

# Load each .rda file
donnees <- read.csv(livedta_files)

pic <- list.files(pattern = "\\.jpg$")

Brightfield <- pic[grep("BF", pic)]
Viability <- pic[grep("Viability", pic)]
CellWall <- pic[grep("CW", pic)]

# Convert the image dimensions into pixels:
donnees$cooX_px <- donnees$cooX/0.6490139 
donnees$cooY_px <- donnees$cooY/0.6490139

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  titlePanel("Segpipe-CW: check your data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("select_column_value", "Select a condition:", choices = unique(donnees$medium))
    ),
    mainPanel(
      plotlyOutput("plot"),
      verbatimTextOutput("clicked_info"),
      verbatimTextOutput("pic"),
      h3("Images:"),
      fluidRow(
        column(width = 4,
               h4("Brightfield:"),
               imageOutput("zoomed_bf")),
        column(width = 4,
               h4("Viability:"),
               imageOutput("zoomed_via")),
        column(width = 4,
               h4("Cell wall:"),
               imageOutput("zoomed_cw"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  clicked_coords <- reactiveValues(cgX = NULL, cgY = NULL)
  real_coords <- reactiveValues(cX = NULL, cY= NULL)
  
  # Create an interactive dot plot with plotly
  output$plot <- renderPlotly({
    # Filter data based on selected x-axis
    condition <- donnees[donnees$medium == input$select_column_value, ]
    
    # Print out filtered data for debugging
    print(head(condition))
    
    # Plot
    p <- ggplot(condition, aes(x = medium, y = meanCW)) +
      geom_point(aes(color = diameter), size = 1.5, alpha = 0.8, position = position_jitter(width = 0.4, height = 0)) +
      scale_color_gradientn(colors= c("yellow", "red", "purple", "blue")) 
      theme_bw() +
      ggtitle("CW intensity") +
      xlab("medium") +
      ylab("Cell wall staining")
    
    p <- ggplotly(p)
    
    p <- htmlwidgets::onRender(p, '
    function(el, x) {
      el.on("plotly_click", function(data) {
        Shiny.setInputValue("clicked_point", data.points[0].x + "," + data.points[0].y);
      });
    }
  ')
  })
  
  
  # Display clicked point info
  observeEvent(input$clicked_point, {
    req(input$clicked_point)
    coords <- strsplit(input$clicked_point, ",")[[1]]
    clicked_coords$cgX <- as.numeric(coords[1])
    clicked_coords$cgY <- as.numeric(coords[2])
  })
  
  # Function to calculate picture coordinates
  coordinates <- function(cgY, donnees) {
    cX <- donnees$cooX_px[donnees$meanCW == cgY]
    cY <- donnees$cooY_px[donnees$meanCW == cgY]
    return(c(cX, cY))
  }
  
  # Display clicked point info
  output$clicked_info <- renderPrint({
    req(clicked_coords$cgX, clicked_coords$cgY)
    paste("Clicked Point Coordinates: (X =", clicked_coords$cgX, ", Y =", clicked_coords$cgY, ")")
  })
  
  # Call the coordinates function to get picture coordinates
  output$pic <- renderPrint({ 
    coords <- coordinates(clicked_coords$cgY, donnees)
    paste("Picture coordinates: (X =", coords[1], ", Y =", coords[2], ")")
  })
  
  # Render the zoomed image
  output$zoomed_bf <- renderImage({
    req(input$select_column_value)
    image_condition <- input$select_column_value
    image_name <- paste0(image_condition, "-BF.jpg")
    # Construct the path to the selected image
    image_path <- file.path(work_folder, image_name)
    # Read the image using magick package
    image <- image_read(image_path)
    # for pic coordinates
    coords <- coordinates(clicked_coords$cgY, donnees)
    # Define zoom region (adjust the zoom_factor and zoom_size as needed)
    zoom_factor <- 1  # You can adjust this value
    zoom_size <- 100 / zoom_factor
    x_start <- max(1, coords[1] - zoom_size / 2)
    y_start <- max(1, coords[2] - zoom_size / 2)
    x_end <- min(image_info(image)$width, coords[1] + zoom_size / 2)
    y_end <- min(image_info(image)$height, coords[2] + zoom_size / 2)
    # Crop the image to the zoom region
    zoomed_image <- image_crop(image, geometry_area(width = x_end - x_start, height = y_end - y_start, x_off = x_start, y_off = y_start))
    # Save the zoomed image to a temporary file
    temp_file <- tempfile(fileext = ".png")
    image_write(zoomed_image, temp_file)
    list(src = temp_file, contentType = 'image/png')
  }, deleteFile = TRUE)
  
  output$zoomed_via <- renderImage({
    req(input$select_column_value)
    image_condition <- input$select_column_value
    image_name <- paste0(image_condition, "-Viability.jpg")
    # Construct the path to the selected image
    image_path <- file.path(work_folder, image_name)
    # Read the image using magick package
    image <- image_read(image_path)
    # for pic coordinates
    coords <- coordinates(clicked_coords$cgY, donnees)
    # Define zoom region (adjust the zoom_factor and zoom_size as needed)
    zoom_factor <- 1  # You can adjust this value
    zoom_size <- 100 / zoom_factor
    x_start <- max(1, coords[1] - zoom_size / 2)
    y_start <- max(1, coords[2] - zoom_size / 2)
    x_end <- min(image_info(image)$width, coords[1] + zoom_size / 2)
    y_end <- min(image_info(image)$height, coords[2] + zoom_size / 2)
    # Crop the image to the zoom region
    zoomed_image <- image_crop(image, geometry_area(width = x_end - x_start, height = y_end - y_start, x_off = x_start, y_off = y_start))
    # Save the zoomed image to a temporary file
    temp_file <- tempfile(fileext = ".png")
    image_write(zoomed_image, temp_file)
    list(src = temp_file, contentType = 'image/png')
  }, deleteFile = TRUE)
  
  output$zoomed_cw <- renderImage({
    req(input$select_column_value)
    image_condition <- input$select_column_value
    image_name <- paste0(image_condition, "-CW.jpg")
    # Construct the path to the selected image
    image_path <- file.path(work_folder, image_name)
    # Read the image using magick package
    image <- image_read(image_path)
    # for pic coordinates
    coords <- coordinates(clicked_coords$cgY, donnees)
    # Define zoom region (adjust the zoom_factor and zoom_size as needed)
    zoom_factor <- 1  # You can adjust this value
    zoom_size <- 100 / zoom_factor
    x_start <- max(1, coords[1] - zoom_size / 2)
    y_start <- max(1, coords[2] - zoom_size / 2)
    x_end <- min(image_info(image)$width, coords[1] + zoom_size / 2)
    y_end <- min(image_info(image)$height, coords[2] + zoom_size / 2)
    # Crop the image to the zoom region
    zoomed_image <- image_crop(image, geometry_area(width = x_end - x_start, height = y_end - y_start, x_off = x_start, y_off = y_start))
    # Save the zoomed image to a temporary file
    temp_file <- tempfile(fileext = ".png")
    image_write(zoomed_image, temp_file)
    list(src = temp_file, contentType = 'image/png')
  }, deleteFile = TRUE)
}

# Run the application
shinyApp(ui = ui, server = server)
