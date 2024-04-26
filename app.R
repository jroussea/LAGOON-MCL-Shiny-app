library(shiny)
library(ggplot2)
library(dplyr)
library(magrittr)
library(shinythemes)

ui = tagList(
  
  #shinythemes::themeSelector(),
  
  navbarPage(
    #theme = "cerulean",  # <--- To use a theme, uncomment this
    theme = shinytheme("cerulean"),
    
    "LAGOON-MCL ",
    tabPanel("Homogeneity score - distribution",
             
             sidebarPanel(
               fileInput("file1", "File input:", 
                         accept = c("text/tsv",
                                    "test/tab-separated-value,text/plain",
                                    ".tsv")),
               #textInput("txt", "Text input:", "general"),
               numericInput("sliderBins", "Bins:", min = 1, value = 50, 
                            step = 1),
               sliderInput("sliderHomSc", "homogeneiry score:", 0, 1,
                           value = c(0.0, 1.0)),
             ),
             
             mainPanel(
               
               tabsetPanel(type = "tabs",
                           tabPanel("Summary", tableOutput("summary")),
                           tabPanel("Plot", plotOutput("plot")),
                           tabPanel("Table", tableOutput("table"))),
             )
    ),
    
    tabPanel("Cluster size - distribution", "In development",
             sidebarPanel(
               fileInput("file2", "File input:"), 
               numericInput("inMin", "Minimum cluster size:", min = 0, 
                            value = 0, step = 1),
               numericInput("inMax", "Maximum cluster size:", min = 0, 
                            value = 10, step = 1)
             )),
    
    tabPanel("Homogeneity score - Cluster size", "In development",
             sidebarPanel(
               fileInput("file2", "File input:"))),
    
    tabPanel("About", "Cette applicaiton shiny permet d'explorer de manière 
             intéractive les différents fichiers TSV généré par le pipeline
             LAGOON-MCL.")
  )
)



server = function(input, output) {
  
  
  filedata1 <- reactive({
    
    infile <- input$file1
    if (is.null(infile)){
      return(NULL)      
    }
    read.table(infile$datapath, header = TRUE, sep = "\t")
  })
  
  output$table <- renderTable({
    
    dataframe <- filedata1()
    head(dataframe, n = 40)
  })
  
  output$summary <- renderText({
    dataframe <- filedata1()
    summary(dataframe)
  })
  
  output$plot <- renderPlot({
    
    dataframe <- filedata1()
    
    graph <- dataframe %>%
      filter(homogeneity_score >= input$sliderHomSc[1] &
               homogeneity_score <= input$sliderHomSc[2]) %>% 
      ggplot(aes(x = homogeneity_score)) +
      geom_histogram(bins = input$sliderBins, color = "darkblue", 
                     fill = "lightblue") +
      theme_light() +
      labs(title = "title_labs",
           x = "x_labs",
           y = "Number of clusters") +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12))
    graph
  })
}

shinyApp(ui, server)