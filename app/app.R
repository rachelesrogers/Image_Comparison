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
            selectInput("clothes_choice", "Select Outfit:", choices=c("defendant", "forensic_scientist"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           imageOutput("characterPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  head_skin <- image_read("www/head1_skin.png")
  head <- image_read("www/head1.png")

    output$characterPlot <- renderImage({
      
      clothes_path_skin <- paste0("www/",input$clothes_choice,"_skin.png")
      clothes_path <- paste0("www/",input$clothes_choice,"_clothes.png")
      
      clothes_skin <- image_read(clothes_path_skin)
      clothes <- image_read(clothes_path)
      
      clothes_skin <- image_modulate(clothes_skin, brightness= input$skin)
      head_skin <- image_modulate(head_skin, brightness= input$skin)
      
      img <- c(clothes_skin, clothes, head_skin, head)
      
      combined <- image_mosaic(img)
      
      # image_write(combined, path="www/final.png", format="png")
      # 
      # img(src='final.png')
      
      print(combined)
      
      # image_write(combined, format="png")
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
