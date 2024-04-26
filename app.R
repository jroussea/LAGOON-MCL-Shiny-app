library(shiny)
library(ggplot2)
library(dplyr)
library(magrittr)
library(shinythemes)

ui = tagList(
  
  navbarPage(
    
    theme = shinytheme("cerulean"),
    
    "LAGOON-MCL ",
    
    tabPanel("Homogeneity score",
             
             sidebarPanel(
          
               
               fileInput("file1", "File input:", 
                         accept = c("text/tsv",
                                    "test/tab-separated-value,text/plain",
                                    ".tsv")),
               
               #uiOutput("columnY"),
               
               fluidRow(radioButtons("plot_type", "Plot type:",
                                     list("Histogram", "Boxplot", "Point")),
                       
                        conditionalPanel(condition="input.plot_type=='Histogram'",
                                         uiOutput("columnX"),
                                         numericInput("sliderBins", "Bins:",
                                                      min = 1, value = 50,
                                                      step = 1),
                                         sliderInput("sliderHomSc", 
                                                     "homogeneiry score:", 0, 1,
                                                     value = c(0.0, 1.0))
                                         ),
                        ),
               ),
             
             mainPanel(
               
               tabsetPanel(type = "tabs",
                           tabPanel("Summary", tableOutput("summary")),
                           tabPanel("Plot",
                                    conditionalPanel(condition="input.plot_type
                                                     =='Histogram'",
                                                     plotOutput("histo")),
                                    conditionalPanel(condition="input.plot_type
                                                     =='Boxplot'",
                                                     plotOutput("")
                                    ),
                                    conditionalPanel(condition="input.plot_type
                                                     =='Point'",
                                                     plotOutput("")
                                    )
                                    
                                    
                                    ),
                           tabPanel("Table", tableOutput("table")),
                           tabPanel("Cluster selection")),
             )
    ),
    
#    tabPanel("Cluster size", "In development",
#             sidebarPanel(
#               fileInput("file2", "File input:"), 
#               numericInput("inMin", "Minimum cluster size:", min = 0, 
#                            value = 0, step = 1),
#               numericInput("inMax", "Maximum cluster size:", min = 0, 
#                            value = 10, step = 1)
#             )),
    
    tabPanel("About", "Cette applicaiton shiny permet d'explorer de manière 
             intéractive les différents fichiers TSV généré par le pipeline
             LAGOON-MCL.")
  )
)



server = function(input, output, session) {
  
  
  filedata1 <- reactive({
    
    infile <- input$file1
    if (is.null(infile)) {
      return(NULL)
    }
    #paste("data/", infile$datapath, sep = "", collapse)
    read.table(infile$datapath, header = TRUE, sep = "\t")
  })

  output$columnX <- renderUI({
    dataframe <- filedata1()
    
    if (is.null(dataframe)) return(NULL)
    
    nums <- sapply(dataframe, is.numeric)
    
    items = names(nums[nums])
    
    names(items) = items
    
    selectInput("Xcol", "Axis X", items)
  })
  
  output$columnY <- renderUI({
    dataframe <- filedata1()
    
    if (is.null(dataframe)) return(NULL)
    
    nums <- sapply(dataframe, is.numeric)
    
    items = names(nums[nums])
    
    names(items) = items
    
    selectInput("Ycol", "Axis Y", items, selected = dataframe$columnY)
  })
  

  
  output$summary <- renderText({
    dataframe <- filedata1()
    summary(dataframe)
  })
  
  column <- reactive({
    input$Xcol
  })
  
  filedata1filter <- reactive({
    
    column <- column()
    
    dataframe <- filedata1()
    
    #x_column <- names(input$Xcol)
    #x_column <- names(items)[items == input$Xcol]
    min_homsc <- input$sliderHomSc[1]
    max_homsc <- input$sliderHomSc[2]
    
    df_filter <- dataframe %>% 
      filter(get(column) >= min_homsc & get(column) <= max_homsc)
    
    df_filter
  })
  
  output$table <- renderTable({
    
    dataframe <- filedata1filter()
    head(dataframe, n = 40)
  })
  
  output$histo <- renderPlot({
    #dataframe <- filedata1()
    dataframe <- filedata1filter()
    

    graph <- dataframe %>%
      ggplot(aes(x = dataframe[,input$Xcol])) +
      geom_histogram(bins = input$sliderBins, color = "darkblue", 
                     fill = "lightblue") 
      #theme_light() +
      #labs(title = "title_labs",
      #     x = "x_labs",
      #     y = "Number of clusters") +
      #theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      #      axis.title = element_text(size = 14),
      #      axis.text = element_text(size = 12))
    graph
  })
}

shinyApp(ui, server)