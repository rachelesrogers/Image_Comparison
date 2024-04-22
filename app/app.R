library(shiny)
library(magick)
library(colourpicker)
library(rsvg)
library(stringr)
library(dplyr)
change_fill <- function(file_contents, new_fill = "#aaaaff") {
  str_replace_all(file_contents, "fill:#[0-f]{6};", sprintf("fill:%s;", new_fill))
}

fig_info <- read.csv("figure_information.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Character Customization"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("color_item", "Change color of:",
                      choices=c(NA, "skin", "hair", "eye",
                                "shirt","pants", "suit")),
            colourpicker::colourInput("color",
                        "Color:",
                        "#bd8347"),

            selectInput("clothes_choice", "Select Outfit:",
                        choices= unique(fig_info[fig_info$Part=="clothes",]$Label)),
            selectInput("head_choice", "Select Head:",
                        choices= unique(fig_info[fig_info$Part=="head",]$Label))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           imageOutput("characterPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  head_path <- reactive({
    paste0("www/head",input$head_choice,".svg")
  })

  body_path <- reactive({
    paste0("www/",input$clothes_choice,".svg")
  })
  
  
  
  clothes_selection <- reactive({
    fig_info %>% filter(Part == "clothes", Item == input$clothes_choice)
  })
  
    output$characterPlot <- renderImage({
      
      file_head <- as.data.frame(paste(gsub("'","",readLines(head_path())), collapse = ""))
      
      head_split <-file_head %>% str_split(">") %>% 
        as.data.frame(col.names="svg_file") %>% filter(svg_file !="")
      
      head_split$svg_file <- paste0(head_split$svg_file, ">")
      
      finding_row_head<-mapply(grepl, input$color_item,head_split)
      
      head_split[finding_row_head,] <- change_fill(head_split[finding_row_head,], input$color)
      
      file_final_head <- apply(head_split,2,paste, collapse="")
      
      head_magic <- image_read_svg(file_final_head, width=400)
      
      
      file_body <- as.data.frame(paste(gsub("'","",readLines(body_path())), collapse = ""))
      
      body_split <-file_body %>% str_split(">") %>% 
        as.data.frame(col.names="svg_file") %>% filter(svg_file !="")
      
      body_split$svg_file <- paste0(body_split$svg_file, ">")
      
      file_final_body <- body_split

      for (i in 1:length(clothes_selection()$Item)){
        finding_row_body<-mapply(grepl, clothes_selection()$Item[i],body_split)

        # body_split[finding_row_body,] <- change_fill(body_split[finding_row_body,],
        #                                              clothes_selection()$Color[i])
        # file_final_body <- apply(body_split,2,paste, collapse="")
      }
      
      finding_row_body<-mapply(grepl, input$color_item,body_split)

      body_split[finding_row_body,] <- change_fill(body_split[finding_row_body,], input$color)

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
