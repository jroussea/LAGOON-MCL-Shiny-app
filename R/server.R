server <- function(input, output) {
    
    fileData <- reactive({
        
        infile <- input$file
        
        if (is.null(infile)) return(NULL)

        read.table(infile$datapath, header = TRUE, sep = "\t")
    })
    
    output$columnX <- renderUI({
        
        dataframe <- fileData()
        
        if (is.null(dataframe)) return(NULL)
        
        nums <- sapply(dataframe, is.numeric)
        items <- names(nums[nums])
        names(items) = items
        
        selectInput("Xcol", "Axis X:", items, selected = "CC_size")
    })
    
    output$columnY <- renderUI({
        
        dataframe <- fileData()
        
        if (is.null(dataframe)) return(NULL) 
        
        nums <- sapply(dataframe, is.numeric)
        items <- names(nums[nums])
        names(items) = items
        
        selectInput("Ycol", "Axis Y:", items, selected = "CC_size")
    })
    
    colX <- reactive({
        input$Xcol
    })
    
    colY <- reactive({
        intput$Ycol
    })
    
    output$clusterSize <- renderUI({
        
        dataframe <- fileData()
        
        if (is.null(dataframe)) { return(NULL) }
        
        min_val <- min(dataframe$CC_size)
        max_val <- max(dataframe$CC_size)
        
        sliderInput("sliderCCsize", "Cluster size:", 0, max_val,
                    value = c(min_val, max_val))
    })
    
    fileDataFilter <- reactive({
        
        dataframe <- fileData()
        columnX <- colX()
        
        min_homsc <- input$sliderHomSc[1]
        max_homsc <- input$sliderHomSc[2]
        
        min_size <- input$sliderCCsize[1]
        max_size <- input$sliderCCsize[2]
        
        df_filter <- dataframe %>% 
            select(CC, CC_size, columnX) %>% 
            filter(get(columnX) >= min_homsc & get(columnX) <= max_homsc) %>% 
            filter(CC_size >= min_size & CC_size <= max_size)
        df_filter
    })
    
    plotLabel <- reactive({
        
        dataframe <- fileDataFilter()
        
        columnX <- colX()
        
        graph <- dataframe %>% 
            ggplot(aes(x = get(columnX))) +
            geom_histogram(bins = input$sliderBins, color = 'darkblue',
                           fill = 'lightblue') +
            theme_light() +
            labs(title = input$titlePlotLabel,
                 x = input$titleAxisXLabel,
                 y = input$titleAxisYLabel) +
            theme(plot.title = element_text(size = 16, face = "bold",
                                            hjust = 0.5),
                  axis.title = element_text(size = 14),
                  axis.text = element_text(size = 12))
        graph
    })
    
    output$histLabel <- renderPlot({
        print(plotLabel())
    })
    
    plotSize <- reactive({
        
        dataframe <- fileData()
        
        graph <- dataframe %>% 
            ggplot(aes(x = CC_size)) +
            geom_histogram(bins = input$sliderBins, color = 'darkblue',
                           fill = 'lightblue') +
            theme_light() +
            labs(title = "Cluster size distribution",
                 x = "Cluster size",
                 y = "Number of cluster") +
            theme(plot.title = element_text(size = 16, face = "bold",
                                            hjust = 0.5),
                  axis.title = element_text(size = 14),
                  axis.text = element_text(size = 12))
        graph
    })
        
    output$histSize <- renderPlot({
        print(plotSize())
    })
}
        
 