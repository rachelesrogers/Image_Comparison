library(shiny)
library(magick)
library(colourpicker)
library(rsvg)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Character Randomizer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            colourpicker::colourInput("skin",
                        "Skin Tone:",
                        "#bf8448"),
            # sliderInput("eyes",
            #             "Eye Hue:",
            #             min = 0,
            #             max = 200,
            #             value = 100),
            # sliderInput("eye_bright",
            #             "Eye Brightness:",
            #             min = 40,
            #             max = 150,
            #             value = 100),
            selectInput("clothes_choice", "Select Outfit:",
                        choices=c("defendant", "scientist", "police",
                                  "judge", "analyst","inmate", "lawyer")),
            selectInput("head_choice", "Select Head:",
                        choices=c("1", "2"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           imageOutput("characterPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  hand_location <- data.frame("character"=c("defendant", "scientist", "judge", "analyst", "inmate",
                                            "lawyer"), 
                              "x1"=c(298, NA, 306, 273, NA, NA), "y1"=c(375, NA, 406, 383, NA, NA),
                              "x2"=c(125, NA, 92, 127, NA, NA),"y2"=c(392, NA, 410, 381, NA, NA))
  
    output$characterPlot <- renderImage({
      
      head_path <- paste0("www/head",input$head_choice,".svg")
      body_path <- paste0("www/",input$clothes_choice,".svg")
      
      head_magic <- image_read_svg(head_path, width=400)
      head_magic <- image_fill(head_magic, 'none', point=geometry_point(5,5), fuzz=20)
      head_magic <- image_fill(head_magic, input$skin, point=geometry_point(187,177), fuzz=10)
      
      body_hands <- hand_location[hand_location$character==input$clothes_choice,]
      
      body_magic <- image_read_svg(body_path, width=400)
      body_magic <- image_fill(body_magic, 'none', point=geometry_point(5,5), fuzz=20)
      if (!is.na(body_hands$x1) & !is.na(body_hands$y1)){
      body_magic <- image_fill(body_magic, input$skin, 
                               point=geometry_point(body_hands$x1,body_hands$y1), fuzz=10)}
      if (!is.na(body_hands$x2) & !is.na(body_hands$y2)){
      body_magic <- image_fill(body_magic, input$skin, 
                               point=geometry_point(body_hands$x2,body_hands$y2), fuzz=10)}
      
      img <- c(body_magic, head_magic)
      
      combined <- image_flatten(img)
      
      print(combined)
      
      tmpfile <- image_write(combined, tempfile(fileext='png'), format="png")

      list(src = tmpfile, contentType = "image/png", width="70%")
      
    }, deleteFile = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)
