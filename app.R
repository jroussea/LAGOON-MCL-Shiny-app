library(shiny)
library(ggplot2)
library(dplyr)
library(magrittr)
library(shinythemes)

ui = tagList(
  
  navbarPage(
    
    theme = shinytheme("cerulean"),
    
    "LAGOON-MCL ",
    
    tabPanel("Results analysis",
             
             sidebarPanel(
          
               
               fileInput("file1", "File input:", 
                         accept = c("text/tsv",
                                    "test/tab-separated-value,text/plain",
                                    ".tsv")),

               fluidRow(radioButtons("plot_type", "Plot type:",
                                     list("Histogram", "Boxplot", "Point")),
                        
                        conditionalPanel(condition = "input.plot_type == 'Histogram'",
                                       
                                         uiOutput("columnX"),
                                         
                                         numericInput("sliderBins", "Bins:",
                                                      min = 1, value = 50,
                                                      step = 1),
                                         
                                         uiOutput("cluster_size"),
                                         
                                         conditionalPanel(condition = c("input.Xcol != 'CC' && input.Xcol != 'CC_size'"),
                                                          sliderInput("sliderHomSc",
                                                                      "homogeneity score:", 0, 1,
                                                                      value = c(0.0, 1.0)),
                                                          ),
                                         
                                         conditionalPanel(condition = c("input.Xcol != 'CC' && input.Xcol != 'CC_size'"),
                                                          textInput("titlePlotLabel", "Title plot:", "Distribution homogeneity score")
                                         ),
                                         
                                         conditionalPanel(condition = "input.Xcol == 'CC_size'",
                                                          textInput("titlePlotSize", "Title plot:", "Distribution cluster size")
                                         ),
                                         
                                         conditionalPanel(condition = c("input.Xcol != 'CC' && input.Xcol != 'CC_size'"),
                                                          textInput("titleAxisXLabel", "Title axis X:", "Homogeneity score")
                                         ),
                                         
                                         conditionalPanel(condition = "input.Xcol == 'CC_size'",
                                                          textInput("titleAxisXSize", "Title axis X:", "Cluster size")
                                         ),
                                         
                                         textInput("titleAxisY", "Title axis Y:", "Number of cluster"),
                                         
                                         p("Download plot (.jpg format)"),
                                         
                                         conditionalPanel(condition = c("input.Xcol != 'CC' && input.Xcol != 'CC_size'"),
                                                          downloadButton('downloadPlotLabelJpg', 'Download plot')),
                                         
                                         conditionalPanel(condition = "input.Xcol == 'CC_size'",
                                                          downloadButton('downloadPlotSizeJpg', 'Download plot')),
                                         
                                         p("Download plot (.pdf format)"),
                                         
                                         conditionalPanel(condition = c("input.Xcol != 'CC' && input.Xcol != 'CC_size'"),
                                                          downloadButton('downloadPlotLabelPdf', 'Download plot')),
                                         
                                         conditionalPanel(condition = "input.Xcol == 'CC_size'",
                                                          downloadButton('downloadPlotSizePdf', 'Download plot')),
                                         
                                         p("Download filtered dataset (.tsv format)"),
                                         
                                         conditionalPanel(condition = c("input.Xcol != 'CC' && input.Xcol != 'CC_size'"),
                                                          downloadButton('downloadLabel', 'Download data')),
                                         
                                         conditionalPanel(condition = "input.Xcol == 'CC_size'",
                                                          downloadButton('downloadSize', 'Download data'))),
                        
                        conditionalPanel(condition = "input.plot_type == 'Boxplot'",
                                         p("In development")),
                        conditionalPanel(condition = "input.plot_type == 'Point'",
                                         p("In development")),
                        ),
               ),
             
             mainPanel(
               
               tabsetPanel(type = "tabs",
                           tabPanel("Summary", 
                                    h4("Information"),
                                    conditionalPanel(condition = c("input.Xcol != 'CC' && input.Xcol != 'CC_size'"),
                                      verbatimTextOutput("txtoutLabel")),
                                    conditionalPanel(condition = c("input.Xcol == 'CC_size'"),
                                                     verbatimTextOutput("txtoutSize"))
                                    ),
                           tabPanel("Plot",
                                    conditionalPanel(condition = c("input.plot_type == 'Histogram' && input.Xcol != 'CC_size' && input.Xcol != 'CC'"),
                                                     plotOutput("label")),
                                    
                                    conditionalPanel(condition = c("input.plot_type == 'Histogram' && input.Xcol == 'CC_size'"),
                                                     plotOutput("size")),
                                    
                                    conditionalPanel(condition = "input.plot_type == 'Boxplot'",
                                                     plotOutput("")),
                                    
                                    conditionalPanel(condition = "input.plot_type == 'Point'",
                                                     plotOutput(""))
                                    ),
                           tabPanel("Table selection",
                                    conditionalPanel(condition = c("input.plot_type == 'Histogram' && input.Xcol != 'CC_size' && input.Xcol != 'CC'"),
                                                     DT::dataTableOutput("tableLabel")),
                                    
                                    conditionalPanel(condition = c("input.plot_type == 'Histogram' && input.Xcol == 'CC_size'"),
                                                     DT::dataTableOutput("tableSize"))
                                    ),
                           ),
             )
    ),
    tabPanel("About", p("Cette applicaiton shiny permet d'explorer de manière 
             intéractive les différents fichiers TSV généré par le pipeline
             LAGOON-MCL."))
  )
)

server = function(input, output, session) {
  
  filedata <- reactive({
    
    infile <- input$file1
    
    if (is.null(infile)) {
      return(NULL)
    } 

    read.table(infile$datapath, header = TRUE, sep = "\t")
  })

  output$columnX <- renderUI({
    dataframe <- filedata()
    
    if (is.null(dataframe)) return(NULL)
    
    nums <- sapply(dataframe, is.numeric)
    
    items = names(nums[nums])
    
    names(items) = items
    
    selectInput("Xcol", "Axis X", items, selected = "CC_size")
  })
  
  output$columnY <- renderUI({
    dataframe <- filedata()
    
    if (is.null(dataframe)) return(NULL)
    
    nums <- sapply(dataframe, is.numeric)
    
    items = names(nums[nums])
    
    names(items) = items
    
    selectInput("Ycol", "Axis Y", items, selected = "CC_size")
  })
  
  output$cluster_size <- renderUI({
    
    dataframe <- filedata()
    
    if (is.null(dataframe)) return(NULL)
    
    min_val <- min(dataframe$CC_size)
    
    max_val <- max(dataframe$CC_size)
    
    sliderInput("SliderCCsize", 
                "Cluster size:", 0, max_val,
                value = c(min_val, max_val))
  })

  column <- reactive({
    input$Xcol
  })
  
  filedataLabel <- reactive({
    
    dataframe <- filedata()
    
    column <- column()
    
    min_homsc <- input$sliderHomSc[1]
    max_homsc <- input$sliderHomSc[2]
    
    minSize <- input$SliderCCsize[1]
    maxSize <- input$SliderCCsize[2]
    
    df_filter <- dataframe %>%
      select(CC, CC_size, column) %>% 
      filter(get(column) >= min_homsc & get(column) <= max_homsc) %>% 
      filter(CC_size >= minSize & CC_size <= maxSize)
    
    df_filter
  })
  
  output$tableLabel <- DT::renderDataTable({
    dataframe <- filedataLabel()
    
    column <- column()
    
    df_select <- dataframe %>% 
      select(CC, CC_size, column)
    
    DT::datatable(df_select, options = list(lengthMenu = c(5, 10, 50, 100, 200),
                                            pageLength = 10))
  })
  
  plotLabel <- reactive({

    dataframe <- filedataLabel()
    
    column <- column()
    
    graph <- dataframe %>%
      ggplot(aes(x = get(column))) +
      geom_histogram(bins = input$sliderBins, color = "darkblue", 
                     fill = "lightblue") +
      theme_light() +
      labs(title = input$titlePlotLabel,
           x = input$titleAxisXLabel,
           y = input$titleAxisY) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12))
    
  })
  
  output$label <- renderPlot({
    print(plotLabel())
  })
  
  filedataSize <- reactive({
    
    dataframe <- filedata()
    
    minSize <- input$SliderCCsize[1]
    maxSize <- input$SliderCCsize[2]
    
    df_filter <- dataframe %>% 
      select(CC, CC_size) %>% 
      filter(CC_size >= minSize & CC_size <= maxSize)
    
    df_filter
    
  })
  
  output$tableSize <- DT::renderDataTable({
    dataframe <- filedataSize()
    
    column <- column()
    
    df_select <- dataframe %>% 
      select(CC, CC_size)
    
    DT::datatable(df_select, options = list(lengthMenu = c(5, 10, 50, 100, 200),
                                            pageLength = 10))
  })
  
  plotSize <- reactive({
    
    dataframe <- filedataSize()
    
    graph <- dataframe %>%
      ggplot(aes(x = CC_size)) +
      geom_histogram(bins = input$sliderBins, color = "darkblue", 
                     fill = "lightblue") +
      theme_light() +
      labs(title = input$titlePlotSize,
           x = input$titleAxisXSize,
           y = input$titleAxisY) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12))
    
  })
  
  output$size <- renderPlot({
    print(plotSize())
  })
  
  output$summary <- renderTable({
    dataframe <- filedataLabel()
    
    column <- column()
    
    df_filter <- dataframe %>% 
      select(column)
    
    summary(df_filter)
  })

  output$txtoutSize <- renderText({
    
    dataframe <- filedata()
    dataframeSize <- filedataSize()
    
    column <- column()
    
    sum <- summary(select(dataframe, column))
    sumSize <- summary(select(dataframeSize, column))
    
    paste(
      paste("<><><><><><><><><><><><><><>", sep = ""),
      paste("Axis", column, sep = " "),
      paste(sum[1], sep = " "),
      paste(sum[6], sep = " "),
      paste(sum[3], sep = " "),
      paste(sum[4], sep = " "),
      paste(sum[2], sep = " "),
      paste(sum[5], sep = " "),
      paste("<><><><><><><><><><><><><><>", sep = ""),
      paste("Axis", column, '(filter)',sep = " "),
      paste(sumSize[1], sep = " "),
      paste(sumSize[6], sep = " "),
      paste(sumSize[3], sep = " "),
      paste(sumSize[4], sep = " "),
      paste(sumSize[2], sep = " "),
      paste(sumSize[5], sep = " "),
      paste("<><><><><><><><><><><><><><>", sep = ""),
      sep = "\n"
    )
  })
    
  output$txtoutLabel <- renderText({
    
    dataframe <- filedata()
    dataframeLabel <- filedataLabel()
    
    column <- column()

    sum <- summary(select(dataframe, column))
    sumLabel <- summary(select(dataframeLabel, column))

    paste(
      paste("<><><><><><><><><><><><><><>", sep = ""),
      paste("Axis", column, sep = " "),
      paste(sum[1], sep = " "),
      paste(sum[6], sep = " "),
      paste(sum[3], sep = " "),
      paste(sum[4], sep = " "),
      paste(sum[2], sep = " "),
      paste(sum[5], sep = " "),
      paste("<><><><><><><><><><><><><><>", sep = ""),
      paste("Axis", column, '(filter)',sep = " "),
      paste(sumLabel[1], sep = " "),
      paste(sumLabel[6], sep = " "),
      paste(sumLabel[3], sep = " "),
      paste(sumLabel[4], sep = " "),
      paste(sumLabel[2], sep = " "),
      paste(sumLabel[5], sep = " "),
      paste("<><><><><><><><><><><><><><>", sep = ""),
      sep = "\n"
      )
  })
  
  output$downloadLabel <- downloadHandler(
    filename = function() { 
      paste("dataset-filter-", Sys.Date(), ".tsv", sep="")
    },
    content = function(file) {
      write.table(filedataLabel(), file, sep = "\t", row.names = FALSE)
    }) 
  
  output$downloadSize <- downloadHandler(
    filename = function() { 
      paste("dataset-filter", Sys.Date(), ".tsv", sep="")
    },
    content = function(file) {
      write.table(filedataSize(), file, sep = "\t", row.names = FALSE)
    }) 

  output$downloadPlotSizeJpg <- downloadHandler(
    filename = function() { 
      paste("plot-", Sys.Date(), ".jpg", sep="")
    },
    content = function(file) {
      jpeg(file = file)
      print(plotSize())
      dev.off()
    })
  
  output$downloadPlotLabelJpg <- downloadHandler(
    filename = function() { 
      paste("plot-", Sys.Date(), ".jpg", sep="")
    },
    content = function(file) {
      jpeg(file = file)
      print(plotLabel())
      dev.off()
    })
  
  output$downloadPlotLabelPdf <- downloadHandler(
    filename = function() { 
      paste("plot-", Sys.Date(), ".pdf", sep="")
    },
    content = function(file) {
      pdf(file = file)
      print(plotLabel())
      dev.off()
    })
  
  output$downloadPlotSizePdf <- downloadHandler(
    filename = function() { 
      paste("plot-", Sys.Date(), ".pdf", sep="")
    },
    content = function(file) {
      pdf(file = file)
      print(plotSize())
      dev.off()
    })
  
  }

shinyApp(ui, server)
