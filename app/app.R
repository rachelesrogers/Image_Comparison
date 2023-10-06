library(shiny)
library(magick)
library(colourpicker)
library(rsvg)
change_fill <- function(file_contents, new_fill = "#aaaaff") {
  str_replace_all(file_contents, "fill:#[0-f]{6};", sprintf("fill:%s;", new_fill))
}

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
  
    output$characterPlot <- renderImage({
      
      head_path <- paste0("www/head",input$head_choice,".svg")
      body_path <- paste0("www/",input$clothes_choice,".svg")
      
      file_head <- as.data.frame(paste(gsub("'","",readLines(head_path)), collapse = ""))
      
      head_split <-file_head %>% str_split(">") %>% 
        as.data.frame(col.names="svg_file") %>% filter(svg_file !="")
      
      head_split$svg_file <- paste0(head_split$svg_file, ">")
      
      finding_row_head<-mapply(grepl, "skin",head_split)
      
      head_split[finding_row_head,] <- change_fill(head_split[finding_row_head,], input$skin)
      
      file_final_head <- apply(head_split,2,paste, collapse="")
      
      head_magic <- image_read_svg(file_final_head, width=400)
      
      
      file_body <- as.data.frame(paste(gsub("'","",readLines(body_path)), collapse = ""))
      
      body_split <-file_body %>% str_split(">") %>% 
        as.data.frame(col.names="svg_file") %>% filter(svg_file !="")
      
      body_split$svg_file <- paste0(body_split$svg_file, ">")
      
      finding_row_body<-mapply(grepl, "skin",body_split)
      
      body_split[finding_row_body,] <- change_fill(body_split[finding_row_body,], input$skin)
      
      file_final_body <- apply(body_split,2,paste, collapse="")
      
      
      body_magic <- image_read_svg(file_final_body, width=400)
      
      
      img <- c(body_magic, head_magic)
      
      combined <- image_flatten(img)
      
      
      tmpfile <- image_write(combined, tempfile(fileext='png'), format="png")

      list(src = tmpfile, contentType = "image/png", width="70%")
      
    }, deleteFile = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)
