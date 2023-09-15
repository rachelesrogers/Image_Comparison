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
            colourInput("skin",
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
            # selectInput("clothes_choice", "Select Outfit:", 
            #             choices=c("defendant", "forensic_scientist", "police",
            #                       "judge", "analyst","inmate", "lawyer")),
            # selectInput("head_choice", "Select Head:", 
            #             choices=c("1", "2"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           imageOutput("characterPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$characterPlot <- renderImage({
      
      # head_path_eyes <- paste0("www/head",input$head_choice,"_eyes.png")
      # head_path_skin <- paste0("www/head",input$head_choice,"_skin.png")
      # head_path <-paste0("www/head",input$head_choice,".png")
      # 
      # head_eyes <- image_read(head_path_eyes)
      # head_skin <- image_read(head_path_skin)
      # head <- image_read(head_path)
      # 
      # clothes_path_skin <- paste0("www/",input$clothes_choice,"_skin.png")
      # clothes_path <- paste0("www/",input$clothes_choice,"_clothes.png")
      # 
      # clothes_skin <- image_read(clothes_path_skin)
      # clothes <- image_read(clothes_path)
      # 
      # head_eyes <- image_modulate(head_eyes, hue=input$eyes, brightness=input$eye_bright)
      # clothes_skin <- image_modulate(clothes_skin, brightness= input$skin)
      # head_skin <- image_modulate(head_skin, brightness= input$skin)
      # 
      # img <- c(clothes_skin, clothes, head_skin, head, head_eyes)
      
      head_image <- rsvg_raw("www/head1.svg")
      head_magic <- image_read(head_image)
      head_magic <- image_fill(head_magic, 'none', point=geometry_point(5,5), fuzz=10)
      head_magic <- image_fill(head_magic, input$skin, point=geometry_point(87,77), fuzz=10)
      
      body_image <- rsvg_raw("www/defendant.svg")
      body_magic <- image_read(body_image)
      body_magic <- image_fill(body_magic, 'none', point=geometry_point(5,5), fuzz=10)
      body_magic <- image_fill(body_magic, input$skin, point=geometry_point(136,180), fuzz=10)
      body_magic <- image_fill(body_magic, input$skin, point=geometry_point(60,188), fuzz=10)
      
      img <- c(body_magic, head_magic)
      
      combined <- image_flatten(img)
      
      tmpfile <- image_write(combined, tempfile(fileext='png'), format="png")

      list(src = tmpfile, contentType = "image/png", width="70%")
      
    }, deleteFile = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)
