library(shiny)
library(magick)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Character Randomizer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("skin",
                        "Skin Tone:",
                        min = 40,
                        max = 150,
                        value = 100),
            selectInput("clothes_choice", "Select Outfit:", 
                        choices=c("defendant", "forensic_scientist", "judge", "analyst","inmate")),
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
    output$characterPlot <- renderImage({
      
      head_path_skin <- paste0("www/head",input$head_choice,"_skin.png")
      head_path <-paste0("www/head",input$head_choice,".png")
      
      head_skin <- image_read(head_path_skin)
      head <- image_read(head_path)
      
      clothes_path_skin <- paste0("www/",input$clothes_choice,"_skin.png")
      clothes_path <- paste0("www/",input$clothes_choice,"_clothes.png")
      
      clothes_skin <- image_read(clothes_path_skin)
      clothes <- image_read(clothes_path)
      
      clothes_skin <- image_modulate(clothes_skin, brightness= input$skin)
      head_skin <- image_modulate(head_skin, brightness= input$skin)
      
      img <- c(clothes_skin, clothes, head_skin, head)
      
      combined <- image_flatten(img)
      
      tmpfile <- image_write(combined, tempfile(fileext='png'), format="png")

      list(src = tmpfile, contentType = "image/png")
      
    }, deleteFile = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)
