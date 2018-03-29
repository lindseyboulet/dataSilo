 # *** Important ***
# Files must be exported from labchart into .txt files - use file>export and choose data file only
# Files must be labeled in this convention dataType_subjectID_cond1_cond_2.txt eg. "beat_001_PRE_PBO.txt"

# Load packages
y <- c("shiny", "plyr", "dplyr", "reshape", "shinythemes", "ggplot2", "readr", "shinyFiles",
       "shinyjs", "taRifx", "shinydashboard", "here", "plotrix", "data.table", "gridExtra",
       "rmarkdown")
for(i in 1:length(y)){is_installed <- function(mypkg){is.element(mypkg, installed.packages()[,1])}
    if(!is_installed(y[i])){install.packages(y[i], repos="http://lib.stat.cmu.edu/R/CRAN")
    }
    library(y[i], character.only=TRUE, quietly=TRUE,verbose=FALSE)
    }


# hover function ----------------------------------------------------------

hoverValue <- function(hover, x, y, captions, tolerance = 0.05) {
  if (!is.null(hover)) {
    x0 <- hover$x # x coordinate in user space
    y0 <- hover$y # y coordinate in user space
    xrange <- hover$domain$right - hover$domain$left
    yrange <- hover$domain$top - hover$domain$bottom
    # find the index of the observation closest in scaled 1-norm to the mouse
    dist <- abs(x0 - x) / xrange + abs(y0 - y) / yrange
    i <- which.min(dist)
    # return the corresponding index if close enough, else NULL
    if (dist[i] < tolerance) {
      return(paste(round(x[i],1), round(y[i],1), sep = ' , '))
    } else {
      cat("...")
    }
  } else {
    cat("...")
  }
}

# name your files using this convention: "beat_'subjectID'_'cond1'_'cond2'_etc.txt"
dataFilePath <- here::here("rawData") #### NEED TO CHANGE THIS TO FOLDER WITH RAW DATA ####
fileID <- list.files(path = dataFilePath) 

fileID <- strtrim(fileID, nchar(fileID)-4)
fileIndex <- as.data.frame(matrix(as.character(unlist(strsplit(fileID, "_"))), 
                                  ncol = sapply(gregexpr("_", fileID[1]), length) +1, byrow = TRUE))
type <- unique(fileIndex[,1])
subId <- unique(fileIndex[,2])

cond1 <- unique(fileIndex[,3])
if(ncol(fileIndex) >= 4){ cond2 <- unique(fileIndex[,4])}
if(ncol(fileIndex) == 5){ cond3 <- unique(fileIndex[,5])}

cvDataSample <- read.delim(list.files(
  path = dataFilePath, full.names = TRUE)[1])
respDataSample <- read.delim(list.files(
  path = dataFilePath, full.names = TRUE)[1])
if(length(type == 3)) {
    burstDataSample <- read.csv(list.files(
      dataFilePath, full.names = TRUE)[length(list.files(
        dataFilePath, full.names = TRUE))])
    burstDataSample <- burstDataSample[,-1]
    colnames(burstDataSample)[which(!is.na(match(colnames(burstDataSample), "TimeDate")))] <- "Time"
}

header <- dashboardHeader()
anchor <- tags$a(tags$img(src="logo2.png", width = 50, height = 50, align = 'left'),
                 'silo')

header$children[[2]]$children <- tags$div(
  tags$head(tags$style(HTML('.name {font-family: "Georgia", Times, "Times New Roman", serif;
                  font-weight: bold;
                  font-size: 48px;
                  color:black;}'))),
  anchor,
  class = 'name')

# Define UI
ui <- dashboardPage(skin = "black",
                    title="silo - Interactive Signal Averaging",
                header,                    
                    dashboardSidebar(width = 150,
                                     numericInput("fileStart", label = "File Start", value = NA),
                                     numericInput("fileEnd", label = "File End", value = NA),
                                     numericInput("protoStart", label = "Start of Protocol", value = NA),
                                     selectInput(inputId = "subjectId", label = strong("Subject"),choices = subId),
                                     selectInput(inputId = "cond1", label = strong("Condition 1"),
                                                 choices = unique(fileIndex$V3),
                                                 selectize = TRUE),
                                     conditionalPanel(
                                       condition = "output.noCond >= 2",
                                       selectInput(inputId = "cond2", label = strong("Condition 2"),
                                                 choices = unique(fileIndex$V4),
                                                 selectize = TRUE)
                                     ),
                                     conditionalPanel(
                                       condition = "output.noCond == 3",
                                       selectInput(inputId = "cond3", label = strong("Condition 3"),
                                                   choices = unique(fileIndex$V5),
                                                   selectize = TRUE)
                                     ),
                                    checkboxInput("burstDataAsk", label = "Include Burst Data", value = FALSE),
                                    checkboxInput("appendAsk", label = "Append Id tag to Output File", value = FALSE),
                                    conditionalPanel(condition = "input.appendAsk",
                                                     textInput("appendTag", label = "Append Tag", value = NULL)
                                    )
                                                     

                                     
      ),      
             dashboardBody(
              tabsetPanel(
                tabPanel("Raw Data",
                   # Select cond to be use
                   br(),
                   fluidRow(
                     column(3,
                   selectInput(inputId = "depVarBeat",
                               label = strong("Select Cardiovascular Variable"),
                               choices = colnames(cvDataSample),
                               selectize = TRUE)),
                   column(2,
                          textInput(inputId = "xmin", label = strong("X-min"),
                                    value = NULL)),
                   column(2,
                          textInput(inputId = "xmax", label = strong("X-max"),
                                    value = NULL)),
                   column(2,
                          textInput(inputId = "ymin", label = strong("Y-min"),
                                    value = NULL)),
                   column(2,
                          textInput(inputId = "ymax", label = strong("Y-max"),
                                    value = NULL))
                   
                   ),
                   
                   
             h4(strong("")),
                   fluidRow(
                     column(10,
                     plotOutput(outputId = "cvRawplot",
                                click = "plot1_click",
                                brush = brushOpts(
                                  id = "plot1_brush"),
                                  hover = "plot1_hover",
                                height = "auto")
                     ),
                     column(2, 
                            verbatimTextOutput("hover_info1"))
                   ),
                   fluidRow(column(6, actionButton("exclude_toggle", "Exclude Points"),
                            actionButton("exclude_reset", "Reset"))),
             br(),
                   fluidRow(
                     column(3,
                            selectInput(inputId = "depVarBreath",
                                        label = strong("Select Respiratory Variable"),
                                        choices = colnames(cvDataSample),
                                        selectize = TRUE)),
                     column(2,
                            textInput(inputId = "xminb", label = strong("X-min"),
                                      value = NULL)),
                     column(2,
                            textInput(inputId = "xmaxb", label = strong("X-max"),
                                      value = NULL)),
                     column(2,
                            textInput(inputId = "yminb", label = strong("Y-min"),
                                      value = NULL)),
                     column(2,
                            textInput(inputId = "ymaxb", label = strong("Y-max"),
                                      value = NULL))),
                     h4(strong("")),
                   fluidRow(
                     column(10,
                   plotOutput(outputId = "respRawplot",
                              click = "plot2_click",
                              brush = brushOpts(
                                id = "plot2_brush"),
                                hover = "plot2_hover",
                                height = "auto")
                   ),
                   column(2, 
                          verbatimTextOutput("hover_info2"))
                   ),
                   fluidRow(
                     column(6, actionButton("exclude_toggle2", "Exclude Points"),
                     actionButton("exclude_reset2", "Reset"))
                   ),
             br(),
             conditionalPanel(condition = "input.burstDataAsk",
               fluidRow(
                 column(3,
                        selectInput(inputId = "depVarBurst",
                                    label = strong("Select Burst Variable"),
                                    choices = colnames(burstDataSample),
                                    selectize = TRUE)),
                 column(2,
                        textInput(inputId = "xminc", label = strong("X-min"),
                                  value = NULL)),
                 column(2,
                        textInput(inputId = "xmaxc", label = strong("X-max"),
                                  value = NULL)),
                 column(2,
                        textInput(inputId = "yminc", label = strong("Y-min"),
                                  value = NULL)),
                 column(2,
                        textInput(inputId = "ymaxc", label = strong("Y-max"),
                                  value = NULL))
               ),
               
               h4(strong("")),
               fluidRow(
                 column(10,
                        plotOutput(outputId = "burstRawplot",
                                   click = "plot3_click",
                                   brush = brushOpts(
                                     id = "plot3_brush"),
                                   hover = "plot3_hover",
                                   height = "auto")
                 ),
                 column(2, 
                        verbatimTextOutput("hover_info3"))
               ),
               fluidRow(
                 column(6, actionButton("exclude_toggle3", "Exclued Points"),
                        actionButton("exclude_reset3", "Reset"))
                 )
             )
             ),
                
                  tabPanel("Variable Selection",
                           br(),
                   fluidRow(
                     column(4, selectInput('cvVars', 'Select Cardiovascular Variables',
                                           colnames(cvDataSample), multiple=TRUE, selectize=TRUE)),
                     column(4, selectInput('respVars', 'Select Respiratory Variables', 
                                        colnames(respDataSample), multiple=TRUE, selectize=TRUE)),
                     conditionalPanel(condition = "input.burstDataAsk",
                                    
                   column(4, selectInput('burstVars', 'Select Burst Variables',
                                      colnames(burstDataSample), multiple=TRUE, selectize=TRUE)
                   )
                   )
                            ),
                 
                   fluidRow(
                     column(4, selectInput('zeroRound', 'Round to Zero Decimal Places', "", multiple=TRUE, selectize=TRUE)
                     ),
                     column(4,
                            selectInput('twoRound', 'Round to Two Decimal Places', "", multiple=TRUE, selectize=TRUE)
                     )
                   )
                  ),
                tabPanel("Averaged Data",
                         fluidRow(
                           column(2,
                                  numericInput(inputId = "bin", label = strong("Bin"),
                                              value = 15)
                                  
                           ),
                           column(2,
                                  numericInput(inputId = "av1Xmin", label = strong("X-Min"),
                                              value = NULL)
                           ),
                           column(2,
                                  numericInput(inputId = "av1Xmax", label = strong("X-Max"),
                                            value = NULL)
                           ),
                                                    
                           column(2,
                                  selectInput(inputId = "plot1Var", label = strong("Variable"),
                                              choices = NULL)
                           ),
                           
                      
                           column(2,
                                  numericInput(inputId = "av1Ymin", label = strong("Y-Min"),
                                            value = NULL)
                           ),
                           column(2,
                                  numericInput(inputId = "av1Ymax", label = strong("Y-Max"),
                                            value = NULL)
                           )
                         ),
                         fluidRow(
                           column(12, 
                                        plotOutput(outputId = "meanPlot1",
                                             click = "meanClick",
                                             hover = "plot4_hover",
                                             height = "auto")
                           )
                         ),
                         br(),
                         actionButton('saveClean', 'Cleaned Data'),
                         actionButton('saveAverage', 'Averaged Data'),
                         actionButton('createRawFigs', 'Raw Figures'),
                         actionButton('createMeanFigs', 'Mean Figures'),
                         br(),
                         br(),
                         fluidRow(
                          column(1, numericInput(inputId = "noStages", 
                                                 value = 0,
                                                 label = strong("# stages"))),
                          column(1, conditionalPanel(condition = "input.noStages >=1",
                                 textInput(inputId = "st1", 
                                                 value = NULL,
                                                 label = strong("Stage 1")))),
                          column(1, conditionalPanel(condition = "input.noStages >=2",
                                                     textInput(inputId = "st2", 
                                                               value = NULL,
                                                               label = strong("Stage 2")))),
                          column(1, conditionalPanel(condition = "input.noStages >=3",
                                                     textInput(inputId = "st3", 
                                                               value = NULL,
                                                               label = strong("Stage 3")))),
                          column(1, conditionalPanel(condition = "input.noStages >=4",
                                                     textInput(inputId = "st4", 
                                                               value = NULL,
                                                               label = strong("Stage 4")))),
                          column(1, conditionalPanel(condition = "input.noStages >=5",
                                                     textInput(inputId = "st5", 
                                                               value = NULL,
                                                               label = strong("Stage 5")))),
                          column(1, conditionalPanel(condition = "input.noStages >=6",
                                                     textInput(inputId = "st6", 
                                                               value = NULL,
                                                               label = strong("Stage 6"))))
                          
                         ),
                         fluidRow(
                          column(12,actionButton('saveSelect', 'Selected Data'),
                                 actionButton("exclude_reset4", "Reset"))
                         )
                         )
                                  
                           
                         
                                        
                           
                         
                         )
                )
             )
              
              






# Define server function
server <- function(input, output, session) {
  session$onSessionEnded(stopApp)
# Load the data --------------------------------------------------------
  
  
  
  output$noCond <- reactive({
      dataFilePath <- here::here( "rawData") #### NEED TO CHANGE THIS TO FOLDER WITH RAW DATA ####
      fileID <- list.files(path = dataFilePath) 
      
      fileID <- strtrim(fileID, nchar(fileID)-4)
      fileIndex <- as.data.frame(matrix(as.character(unlist(strsplit(fileID, "_"))), 
                                        ncol = sapply(gregexpr("_", fileID[1]), length) +1, byrow = TRUE))
      ncol(fileIndex)-2
  })
  outputOptions(output, "noCond", suspendWhenHidden = FALSE) ## so the app can rectively guess the conditions
  
  noCond2 <- reactive({
    dataFilePath <- here::here("rawData") #### NEED TO CHANGE THIS TO FOLDER WITH RAW DATA ####
    fileID <- list.files(path = dataFilePath) 
    
    fileID <- strtrim(fileID, nchar(fileID)-4)
    fileIndex <- as.data.frame(matrix(as.character(unlist(strsplit(fileID, "_"))), 
                                      ncol = sapply(gregexpr("_", fileID[1]), length) +1, byrow = TRUE))
    ncol(fileIndex)-2
  })
  cvPlotData <- reactive({



    if(noCond2()== 1){
      beatFileName <- paste(here::here("rawData"),
                            "/beat_", input$subjectId,"_", input$cond1, ".txt", sep = "")
    }else if(noCond2()== 2){
      beatFileName <- paste(here::here( "rawData"),
                            "/beat_", input$subjectId,"_", input$cond1,
                            "_", input$cond2,".txt", sep = "")
    }else{
      beatFileName <- paste(here::here( "rawData"),
                            "/beat_", input$subjectId,"_", input$cond1,"_",
                            input$cond2,"_", input$cond3,".txt", sep = "")
    }
   
   cvFile <- read.delim(beatFileName)
    row1 <- data.frame(lapply(cvFile[1,], as.character), stringsAsFactors=FALSE)
    cvRawTimeCol <- which(!is.na(match(row1, "Time")))
    cvPlotData <- cvFile[-(1:2),]
    if(length(cvRawTimeCol)>1){ 
      cvPlotData <- cvPlotData[,-cvRawTimeCol[2]]
      cvRawTimeCol <- cvRawTimeCol[1]
    }
    cvPlotData <- data.frame(lapply(cvPlotData, as.character), stringsAsFactors=FALSE)
    suppressWarnings(cvPlotData <- data.frame(lapply(cvPlotData, as.numeric), stringsAsFactors=FALSE))
    colnames(cvPlotData)[cvRawTimeCol] <- "Time"
    
      if(!is.na(input$fileStart)){cvPlotData <- cvPlotData[cvPlotData$Time >= input$fileStart,]}
   
      if(!is.na(input$fileEnd)){cvPlotData <- cvPlotData[cvPlotData$Time <= input$fileEnd,]}
 
      if(!is.na(input$protoStart)){cvPlotData$Time <- cvPlotData$Time - input$protoStart}
    
    cvPlotData
    
  })
        
  respPlotData <- reactive({

    if(noCond2()== 1){
      breathFileName <- paste(here::here( "rawData"),
                            "/breath_", input$subjectId,"_", input$cond1, ".txt", sep = "")
    }else if(noCond2()== 2){
      breathFileName <- paste(here::here( "rawData"),
                            "/breath_", input$subjectId,"_", input$cond1,
                            "_", input$cond2,".txt", sep = "")
    }else{
      breathFileName <- paste(here::here( "rawData"),
                            "/breath_", input$subjectId,"_", input$cond1,"_",
                            input$cond2,"_", input$cond3,".txt", sep = "")
    }
    
    respFile <- read.delim(breathFileName)
    row1 <- data.frame(lapply(respFile[1,], as.character), stringsAsFactors=FALSE)
    respRawTimeCol <- which(!is.na(match(row1, "Time")))
    respPlotData <- respFile[-(1:2),]
    if(length(respRawTimeCol)>1){ 
      respPlotData <- respPlotData[,-respRawTimeCol[2]]
      respRawTimeCol <- respRawTimeCol[1]
    }
    respPlotData <- data.frame(lapply(respPlotData, as.character), stringsAsFactors=FALSE)
    suppressWarnings(respPlotData <- data.frame(lapply(respPlotData, as.numeric), stringsAsFactors=FALSE))
    colnames(respPlotData)[respRawTimeCol] <- "Time"

      if(!is.na(input$fileStart)){respPlotData <- respPlotData[respPlotData$Time > input$fileStart,]}
 
      if(!is.na(input$fileEnd)){respPlotData <- respPlotData[respPlotData$Time < input$fileEnd,]}
   
      if(!is.na(input$protoStart)){respPlotData$Time <- respPlotData$Time - input$protoStart}
   
    respPlotData
    
  })

    burstPlotData <- reactive({

      if(noCond2()== 1){
        burstFileName <- paste(here::here( "rawData"),
                                "/burst_", input$subjectId,"_", input$cond1, ".csv", sep = "")
      }else if(noCond2()== 2){
        burstFileName <- paste(here::here( "rawData"),
                                "/burst_", input$subjectId,"_", input$cond1,
                                "_", input$cond2,".csv", sep = "")
      }else{
        burstFileName <- paste(here::here( "rawData"),
                                "/burst_", input$subjectId,"_", input$cond1,"_",
                                input$cond2,"_", input$cond3,".csv", sep = "")
      }
      
    burstFile <- read.csv(burstFileName)
    burstPlotData <- burstFile
    burstPlotData <- burstPlotData[-1]
    burstRawTimeCol <- which(!is.na(match(colnames(burstPlotData), "TimeDate")))
    burstPlotData <- data.frame(lapply(burstPlotData, as.character), stringsAsFactors=FALSE)
    burstPlotData[, burstRawTimeCol] <-substr(burstPlotData[, burstRawTimeCol],
                                              11, nchar(burstPlotData[, burstRawTimeCol]))    
    suppressWarnings(burstPlotData <- data.frame(lapply(burstPlotData, as.numeric), stringsAsFactors=FALSE))
    colnames(burstPlotData)[burstRawTimeCol]<- "Time"
   
      if(!is.na(input$fileStart)){burstPlotData <- burstPlotData[burstPlotData$Time > input$fileStart,]}
 
      if(!is.na(input$fileEnd)){burstPlotData <- burstPlotData[burstPlotData$Time < input$fileEnd,]}
   
      if(!is.na(input$protoStart)){burstPlotData$Time <- burstPlotData$Time - input$protoStart}
     
    burstPlotData
  })

  
  # For storing which rows have been excluded
  valsCV <- reactiveValues(
    cvKeepRows = rep(TRUE, isolate(nrow(cvPlotData()))))
  valsResp <- reactiveValues(
  respKeepRows = rep(TRUE, isolate(nrow(respPlotData())))
  )
if(length(type)>2){
      valsBurst <- reactiveValues(
        burstKeepRows = rep(TRUE, isolate(nrow(burstPlotData()))))
}
 

# Create Raw Plots --------------------------------------------------------

  output$cvRawplot <- renderPlot({

    cvData <- cvPlotData()
    keepCV    <- cvData[ valsCV$cvKeepRows, , drop = FALSE]
    excludeCV <- cvData[!valsCV$cvKeepRows, , drop = FALSE]
    
      ggplot(keepCV ,aes_string( 
       x="Time",
        y=input$depVarBeat))+
      geom_point(colour='#e41a1c')+
          theme(axis.title.x=element_blank(),
                plot.background = element_rect(fill = "grey93")) +
        geom_point(data = excludeCV, shape = 21, fill = NA, color = "red", alpha = 0.25) +
        coord_cartesian() +
        scale_x_continuous(limits = as.numeric(c(input$xmin, input$xmax))) +
        scale_y_continuous(limits = as.numeric(c(input$ymin, input$ymax)))
    
      },
     height = function() {
       session$clientData$output_cvRawplot_width/4
   
     }
  )

 

    output$respRawplot <- renderPlot({

      respData <- respPlotData()
      keepResp    <- respData[ valsResp$respKeepRows, , drop = FALSE]
      excludeResp <- respData[!valsResp$respKeepRows, , drop = FALSE]
      
      ggplot(keepResp, aes_string(x="Time", y=input$depVarBreath))+
        geom_point(colour='#377eb8')+
        theme(axis.title.x=element_blank(),
              plot.background = element_rect(fill = "grey93")) +
        geom_point(data = excludeResp, shape = 21, fill = NA, color = "blue", alpha = 0.25) +
        coord_cartesian() +
        scale_x_continuous(limits = as.numeric(c(input$xminb, input$xmaxb))) +
        scale_y_continuous(limits = as.numeric(c(input$yminb, input$ymaxb)))
      
      },
      height = function() {
        session$clientData$output_respRawplot_width/4
      }
      )
    
    observe({
    if(input$burstDataAsk == TRUE){
      output$burstRawplot <- renderPlot({
        burstData <- burstPlotData()
        keepburst    <- burstData[valsBurst$burstKeepRows, , drop = FALSE]
        excludeburst <- burstData[!valsBurst$burstKeepRows, , drop = FALSE]
        
        ggplot(keepburst, aes_string(x="Time",
                                     y=input$depVarBurst))+
          geom_point(colour='#4daf4a')+
          theme(axis.title.x=element_blank(),
                plot.background = element_rect(fill = "grey93")) +
          geom_point(data = excludeburst, shape = 21, fill = NA, color = "green", alpha = 0.25) +
          coord_cartesian() +
          scale_x_continuous(limits = as.numeric(c(input$xminc, input$xmaxc))) +
          scale_y_continuous(limits = as.numeric(c(input$yminc, input$ymaxc)))
        
        },
        height = function() {
          session$clientData$output_burstRawplot_width/4
        }
      )
    }
      
    })

    # Toggle points that are clicked on plot 1
    observeEvent(input$plot1_click, {
      res <- nearPoints(cvPlotData(), input$plot1_click, allRows = TRUE)
      
      valsCV$cvKeepRows <- xor(valsCV$cvKeepRows, res$selected_)
    })
    
    # Toggle points that are brushed, when button is clicked on plot 1
    observeEvent(input$exclude_toggle, {
      res <- brushedPoints(cvPlotData(), input$plot1_brush, allRows = TRUE)
      valsCV$cvKeepRows<- xor(valsCV$cvKeepRows, res$selected_)
    })
    
    # Hover for plot 1
    output$hover_info1 <- renderPrint({
      cvPlotData <- cvPlotData()
      yVal <- which(colnames(cvPlotData) == input$depVarBeat)
       HTML(hoverValue(input$plot1_hover, cvPlotData$Time, cvPlotData[,yVal], cvPlotData$Time))
  
    })
    
    
    # Reset all points for plot 1
    observeEvent(input$exclude_reset, {
      valsCV$cvKeepRows <- rep(TRUE, nrow(cvPlotData()))
    })
    
    # Toggle points that are clicked on plot 2
    observeEvent(input$plot2_click, {
      res <- nearPoints(respPlotData(), input$plot2_click, allRows = TRUE)
      
      valsResp$respKeepRows <- xor(valsResp$respKeepRows, res$selected_)
    })
    
    # Toggle points that are brushed, when button is clicked on plot 2
    observeEvent(input$exclude_toggle2, {
      res <- brushedPoints(isolate(respPlotData()), input$plot2_brush, allRows = TRUE)
      valsResp$respKeepRows  <- xor(valsResp$respKeepRows, res$selected_)
    })
    
    # Hover for plot 2
    
    output$hover_info2 <- renderPrint({
      respPlotData <- respPlotData()
      yVal <- which(colnames(respPlotData) == input$depVarBreath)
      HTML(hoverValue(input$plot2_hover, respPlotData$Time, respPlotData[,yVal], respPlotData$Time))
      
    })
    
    # Reset all points for plot 2
    observeEvent(input$exclude_reset2, {
      valsResp$respKeepRows <- rep(TRUE, nrow(isolate(respPlotData())))
    })
    
    outVar = reactive({
      c(input$cvVars, input$respVars)
    })
    
    observe({
    if(input$burstDataAsk == TRUE){
      # Toggle points that are clicked on plot 3
      observeEvent(input$plot3_click, {
        res <- nearPoints(burstPlotData(), input$plot3_click, allRows = TRUE)
        valsBurst$burstKeepRows <- xor(valsBurst$burstKeepRows, res$selected_)
      })
      
      # Toggle points that are brushed, when button is clicked on plot 3
      observeEvent(input$exclude_toggle3, {
        res <- brushedPoints(isolate(burstPlotData()), input$plot3_brush, allRows = TRUE)
        valsBurst$burstKeepRows  <- xor(valsBurst$burstKeepRows, res$selected_)
      })
      # Hover for plot 3
      
      output$hover_info3 <- renderPrint({
        burstPlotData <- burstPlotData()
        yVal <- which(colnames(burstPlotData) == input$depVarBurst)
        HTML(hoverValue(input$plot3_hover, burstPlotData$Time, burstPlotData[,yVal], burstPlotData$Time))
        
      })
      
      # Reset all points for plot 3
      observeEvent(input$exclude_reset3, {
        valsBurst$burstKeepRows <- rep(TRUE, nrow(isolate(burstPlotData())))
      })
    }
    })


# ing Output Choices -------------------------------------------------

    outVar = reactive({
      if(input$burstDataAsk == TRUE){
        values <- c(input$cvVars, input$respVars, input$burstVars)
      } else {
        values <- c(input$cvVars, input$respVars)
      }
   values
    })
    observe({
      updateSelectInput(session, "zeroRound",
                        choices = outVar()
      )
      updateSelectInput(session, "twoRound",
                        choices = outVar()
      )})
        
    
# Average data ------------------------------------------------------------

averageData <- reactive({

      cvData <- cvPlotData()
      cvData    <- cvData[ valsCV$cvKeepRows, , drop = FALSE]

      respData <- respPlotData()
      respData    <- respData[ valsResp$respKeepRows, , drop = FALSE]
      
     if(input$burstDataAsk == TRUE){
        burstData <- burstPlotData()
        burstData    <- burstData[valsBurst$burstKeepRows, , drop = FALSE]
        if (!is.null(input$burstVars)){
          burstColRange <- which(!is.na(match(colnames(burstData), input$burstVars)))
        } else {burstColRange <- 2:6}  
        timeCol2 <- which(colnames(burstData) == "Time")
        timeCol2 <- timeCol2[1]
      }
     
     
      if (!is.null(input$cvVars)){
        cvColRange <- which(!is.na(match(colnames(cvData), input$cvVars)))
      } else {cvColRange <- 2:6}
      timeCol <- which(colnames(cvData) == "Time")
      if (!is.null(input$respVars)){
        respColRange <- which(!is.na(match(colnames(respData), input$respVars)))
      } else {respColRange <- 8:15}


        # Run averaging function
        cvData[cvData == "#NUM!"] <- NA
        respData[respData == "#NUM!"] <- NA

        cvColRange <- c(timeCol, cvColRange)
        respColRange <- c(timeCol, respColRange)

        cvData <- cvData[, cvColRange]
        respData <- respData[, respColRange]

  
       
          breaks <- seq(from = min(cvData$Time, na.rm = TRUE),
                      to = max(cvData$Time + input$bin, na.rm = TRUE), by = input$bin)
        
        
        cvData <- mutate(cvData, bins = cut(cvData$Time,
                             breaks = breaks, include.lowest = TRUE))
        respData <- mutate(respData, bins = cut(respData$Time,
                                            breaks = breaks, include.lowest = TRUE))
        
        cvDataMean <- summarise_all(group_by(cvData, bins),
                                    funs(mean(., na.rm = TRUE)))
        cvDataSem <- summarise_all(group_by(cvData, bins),
                                   funs(std.error(., na.rm = TRUE)))

        respDataMean <- summarise_all(group_by(respData, bins),
                                      funs(mean(., na.rm = TRUE)))
        respDataSem <- summarise_all(group_by(respData, bins),
                                     funs(std.error(., na.rm = TRUE)))

        cvDataMerge <- merge(cvDataMean, cvDataSem, by = "bins",
                             suffixes = c("_mean", "_sem"),
                             all = TRUE)
        respDataMerge <- merge(respDataMean, respDataSem, by = "bins",
                               suffixes = c("_mean", "_sem"),
                               all = TRUE)


        cvColOrder <- c(1,rep(seq(2, ncol(cvData)), each = 2))
        cvColOrder[seq(3, length(cvColOrder), by = 2)] <- 
        cvColOrder[seq(3, length(cvColOrder), by = 2)]+(ncol(cvDataMean)-1)

        respColOrder <- c(1,rep(seq(2, ncol(respData)), each = 2))
        respColOrder[seq(3, length(respColOrder), by = 2)] <-
          respColOrder[seq(3, length(respColOrder), by = 2)]+(ncol(respDataMean)-1)

        cvDataMerge <- cvDataMerge[, cvColOrder]
        respDataMerge <- respDataMerge[, respColOrder]

        cvDataMerge$nBeats <- as.data.frame(count(cvData, bins))[,2]
        respDataMerge$nBreaths <- as.data.frame(count(respData, bins))[,2]
        
        finalData <- merge(cvDataMerge[,c(1:2,4:ncol(cvDataMerge))],
                           respDataMerge[,c(1,4:ncol(respDataMerge))], by = "bins",
                           suffixes = c("", ""), all = TRUE)
        finalData <- finalData[which(complete.cases(finalData$bins)),]
        
         if(input$burstDataAsk == TRUE){
          burstData[burstData == "#NUM!"] <- NA
          burstColRange <- c(timeCol2, burstColRange)
          burstData <- burstData[, burstColRange]
          
          burstData$bins <- cut(burstData[,1],
                                breaks = seq(from = min(cvData$Time, na.rm = TRUE),
                                to = max(cvData$Time + input$bin, na.rm = TRUE), by = input$bin),
                                include.lowest = TRUE)
         
          burstDataMean <- summarise_all(group_by(burstData, bins),
                                         funs(mean(., na.rm = TRUE)))
          burstDataSem <- summarise_all(group_by(burstData, bins),
                                        funs(std.error(., na.rm = TRUE)))
          burstDataMerge <- merge(burstDataMean, burstDataSem, by = "bins",
                                  suffixes = c("_mean", "_sem"), all = TRUE)
          burstColOrder <- c(1,rep(seq(2, ncol(burstData)), each = 2))
          burstColOrder[seq(3, length(burstColOrder), by = 2)] <- 
          burstColOrder[seq(3, length(burstColOrder), by = 2)]+(ncol(burstDataMean)-1)
          burstDataMerge <- burstDataMerge[, burstColOrder]
          burstDataMerge$nBursts <- as.data.frame(count(burstData, bins))[,2]
          finalData <- merge(finalData,
                             burstDataMerge[,c(1,4:ncol(burstDataMerge))], by = "bins",
                             suffixes = c("", ""), all = TRUE)
        }
        
        
    
        zeroRoundmean <- paste(input$zeroRound, "_mean", sep = "")
        zeroRoundsem <-  paste(input$zeroRound, "_sem", sep = "")
        
        twoRoundmean <- paste(input$twoRound, "_mean", sep = "")
        twoRoundsem <-  paste(input$twoRound, "_sem", sep = "")
        
        if(input$burstDataAsk == TRUE){
        oneRoundmean <- paste(c(input$respVars,
                                input$cvVars, input$burstVars), "_mean", sep = "")
        oneRoundsem  <-  paste(c(input$respVars,
                                 input$cvVars, input$burstVars), "_sem", sep = "")
        } else {
          oneRoundmean <- paste(c(input$respVars, input$cvVars), "_mean", sep = "")
          oneRoundsem  <-  paste(c(input$respVars, input$cvVars), "_sem", sep = "")
        }
        
        
        zeroDigRange <- which(!is.na(match(colnames(finalData), c(zeroRoundmean, zeroRoundsem))))
        twoDigRange <- which(!is.na(match(colnames(finalData),  c(twoRoundmean, twoRoundsem))))
        oneDigRange <- which(!is.na(match(colnames(finalData), c(oneRoundmean, oneRoundsem))))
        oneDigRange <- oneDigRange[-(which(!is.na(match(oneDigRange, c(twoDigRange, twoDigRange)))))]
        
        finalData[,twoDigRange] <- round(finalData[,twoDigRange], 0)
        finalData[,oneDigRange] <- round(finalData[,oneDigRange], 1)
        finalData[,twoDigRange] <- round(finalData[,twoDigRange], 2)
        
        if(!dir.exists(here::here("output"))){
          dir.create(here::here("output"))
          dir.create(here::here("output", "averageData"))
          dir.create(here::here("output", "selectData"))
          dir.create(here::here("output", "cleanData"))

        }
        finalData <- finalData[which(complete.cases(finalData$bins)),]
        finalData <- finalData[order(finalData$Time_mean),]
        
        
     finalData
  
 })

     valsMean <- reactiveValues(
       meanKeepRows =  rep(TRUE, isolate(nrow(averageData()))))
     
     
    observe({
      if(input$burstDataAsk){
        updateSelectInput(session,"plot1Var",choices=c(input$cvVars,input$respVars,
                                                       input$burstVars))
      } else{
      updateSelectInput(session,"plot1Var",choices=c(input$cvVars,input$respVars))
      }
        })

# Plot Mean Data ----------------------------------------------------------

     output$meanPlot1 <- renderPlot({
       input$bin
       
      avData <- averageData()
      keepAv    <- avData[ valsMean$meanKeepRows , , drop = FALSE]
      excludeAv <- avData[!valsMean$meanKeepRows , , drop = FALSE]
      
      ggplot(keepAv,
            aes_string(x= "Time_mean",
                       y=paste(input$plot1Var, "_mean",sep = ""))) +
            geom_point(colour='black')+
        geom_point(data = excludeAv, color = "red") +
        coord_cartesian() +
            geom_errorbar(aes_string(ymin = paste(input$plot1Var, "_mean -",
                                                  input$plot1Var, "_sem",sep = ""),
                                    ymax = paste(input$plot1Var, "_mean + ",input$plot1Var, "_sem",sep = ""))) +
            scale_y_continuous(limits = c(input$av1Ymin,input$av1Ymax))+
            scale_x_continuous(limits = c(input$av1Xmin, input$av1Xmax)) +
            theme(plot.margin=unit(c(0,0,0,0),"mm"), axis.title.y = element_blank(),
                  axis.title.x=element_blank())
       
      },
      height = function() {
        session$clientData$output_meanPlot1_width/3}

    )

# Toggle points that are clicked on plot 3
    observeEvent(input$meanClick, {
      res <- nearPoints(averageData(), input$meanClick, allRows = TRUE)

      valsMean$meanKeepRows  <- xor(valsMean$meanKeepRows , res$selected_)
    })
    # Reset all points for plot 3
    observeEvent(input$exclude_reset4, {
      valsMean$meanKeepRows  <- rep(TRUE, nrow(isolate(averageData())))
    })

# Reactive Select Data -------------------------------------------------------------

   selectData <- reactive({
     avData <- averageData()
     selectedAv <- avData[!valsMean$meanKeepRows , , drop = FALSE]
     selectedAv[rowSums(is.na(selectedAv))!=ncol(selectData), ]
     if(input$noStages > 0){
       stages <- c(input$st1, input$st2, input$st3,
                   input$st4, input$st5, input$st6)
       stagesFinal <- vector()
       for(i in 1:input$noStages){stagesFinal <- c(stagesFinal, stages[i])}
       selectedAv$stage <-rep(stagesFinal, length.out=nrow(selectedAv))
       }
     selectedAv[which(complete.cases(selectedAv)),]
     selectedAv[,c(1:2,ncol(selectedAv), 3:(ncol(selectedAv)-1)) ]
     })


output$logo <- renderText({
  c('<img src="',
      "https://openclipart.org/download/279572/Silo.svg",
    '">'
    )
})        
 # Save Average Data -------------------------------------------------------   
    
observeEvent(input$saveAverage, {
        if(noCond2()== 1){
        fileName <- paste(here::here("output", "averageData"),"/",
              input$subjectId,"_", input$cond1,
              '_', input$bin,'sec_mean_all.csv', sep = "")
        }else if(noCond2()== 2){
          fileName <-paste(here::here("output", "averageData"), "/",
                input$subjectId,"_", input$cond1,
                                 "_", input$cond2,'_', input$bin,'sec_mean_all.csv', sep = "")
        }else{
          fileName <-paste(here::here("output", "averageData"),"/",
                input$subjectId,"_", input$cond1,"_",
                                 input$cond2,"_", input$cond3,'_',
                input$bin,'sec_mean_all.csv', sep = "")
        }
  if(input$appendAsk==TRUE){fileName <- paste(strtrim(fileName, nchar(fileName)-4),"_", input$appendTag, ".csv", sep = "" )}
  write.csv(averageData(), fileName, row.names = FALSE)




    })

# Save Select Data --------------------------------------------------------

observeEvent(input$saveSelect, {
  if(noCond2()== 1){
    fileName <- paste(here::here("output", "selectData"),"/",
                      input$subjectId,"_", input$cond1,
                      '_', input$bin,'sec_mean_select.csv', sep = "")
  }else if(noCond2()== 2){
    fileName <-paste(here::here("output", "selectData"), "/",
                     input$subjectId,"_", input$cond1,
                     "_", input$cond2,'_', input$bin,'sec_mean_select.csv', sep = "")
  }else{
    fileName <-paste(here::here("output", "selectData"),"/",
                     input$subjectId,"_", input$cond1,"_",
                     input$cond2,"_", input$cond3,'_',
                     input$bin,'sec_mean_select.csv', sep = "")
  }
  if(input$appendAsk==TRUE){fileName <- paste(strtrim(fileName, nchar(fileName)-4),"_", input$appendTag, ".csv", sep = "" )}
  write.csv(selectData(), fileName, row.names = FALSE)
  
})

# Save Clean Data ---------------------------------------------------------
    
    observeEvent(input$saveClean,{     #### called from UI

        if(noCond2()== 1){
          fileName <-paste(here::here("output", "cleanData"),"/",
                input$subjectId,"_", input$cond1, sep = "")
        }else if(noCond2()== 2){
          fileName <-paste(here::here("output", "cleanData"), "/",
                input$subjectId,"_", input$cond1, sep = "")
        }else{
          fileName <- paste(here::here("output", "cleanData"),"/",
                input$subjectId,"_", input$cond1,"_",
                input$cond2,"_", input$cond3, sep = "")
        }
      
      cvData <- cvPlotData()
      cvData    <- cvData[ valsCV$cvKeepRows, , drop = FALSE]
      if (!is.null(input$cvVars)){
        cvColRange <- which(!is.na(match(colnames(cvData), input$cvVars)))
      } else {cvColRange <- 2:6}
      timeCol <- which(colnames(cvData) == "Time")
      cvColRange <- c(timeCol, cvColRange)
      cvData <- cvData[,cvColRange]
      
      respData <- respPlotData()
      respData    <- respData[ valsResp$respKeepRows, , drop = FALSE]
      if (!is.null(input$respVars)){
        respColRange <- which(!is.na(match(colnames(respData), input$respVars)))
      } else {respColRange <- 8:15}
      timeCol <- which(colnames(respData) == "Time")
      respColRange <- c(timeCol, respColRange)
      respData <- respData[,respColRange]
      if(input$appendAsk){fileName <- paste(fileName,"_", input$appendTag, sep = "" )}
      
      if(input$burstDataAsk == TRUE){
      burstData <- burstPlotData()
      burstData    <- burstData[ valsBurst$burstKeepRows, , drop = FALSE]
      if (!is.null(input$burstVars)){
        burstColRange <- which(!is.na(match(colnames(burstData), input$burstVars)))
      } else {burstColRange <- 2:6}  
      timeCol2 <- which(colnames(burstData) == "Time")
      timeCol2 <- timeCol2[1]
      burstColRange <- c(timeCol2, burstColRange)
      burstData <- burstData[, burstColRange]
      write.csv(burstData, paste(fileName, "_clean_burst.csv", sep = ""), row.names = FALSE)
      }
      write.csv(cvData, paste(fileName, "_clean_beat.csv", sep = ""), row.names = FALSE)
      write.csv(respData, paste(fileName, "_clean_breath.csv", sep = ""), row.names = FALSE)
      
      })

# Save Raw Figures ------------------------------------------------------
    
    observeEvent(input$createRawFigs,{ 
      dataFilePath <- here::here("output", "cleanData") 
      fileID <- list.files(path = dataFilePath, pattern = ".csv") 
      fileID <- strtrim(fileID, nchar(fileID)-4)
      mat <- grep("_",fileID)
      vec <- vector(length = length(mat))
      for(i in mat){
        vec[i] <- length(unlist(strsplit(fileID[i], "_")))
      }
      maxCols <- max(vec)
      fileIndex <- as.data.frame(matrix(nrow = length(vec), 
                                        ncol = maxCols, byrow = FALSE))
      fileIndex  <- japply(fileIndex, which(sapply(fileIndex, class)=="factor"), as.character)
      suppressWarnings(for (i in mat){
        j <- unlist(strsplit(fileID[i], "_"))
        if(vec[i] != maxCols){j <- c(j, rep("", (maxCols-i)))}
        fileIndex[i,] <-j
      })
      len <- length(unique(fileIndex[,ncol(fileIndex)]))
      if(length(which(fileIndex[,ncol(fileIndex)]==""))!= 0){len <- len-1}
      iterator <- rep(seq(1,(nrow(fileIndex)/len)), each = len)
      ll <- 1
  for(i in seq(1,(nrow(fileIndex)/len))){
    dList <- list()
    figList <- list()
    fileName1 <- paste(fileIndex[ll,], collapse = "_")
    while(substr(fileName1, nchar(fileName1),nchar(fileName1)) == "_"){
      fileName1 <- substr(fileName1, 1, nchar(fileName1)-1)
    }
    fileName2 <- paste(fileIndex[ll+1,], collapse = "_")
    while(substr(fileName2, nchar(fileName2),nchar(fileName2)) == "_"){
      fileName2 <- substr(fileName2, 1, nchar(fileName2)-1)
    }
    
    fileName1 <- paste(here::here("output", "cleanData"),"/",fileName1, ".csv", sep = "")
    fileName2 <- paste(here::here("output", "cleanData"),"/",fileName2, ".csv", sep = "")
    
    dList[[1]] <- read.csv(fileName1)
    dList[[2]] <- read.csv(fileName2)

    if(len == 3){
      fileName3 <- paste(fileIndex[ll+2,], collapse = "_")
      while(substr(fileName3, nchar(fileName3),nchar(fileName3)) == "_"){
        fileName3 <- substr(fileName3, 1, nchar(fileName3)-1)
      }
      fileName3 <- paste(here::here("output", "cleanData"),"/",fileName3, ".csv", sep = "")
      dList[[3]] <- read.csv(fileName3)

    }
    fileName <- paste(fileIndex[ll,-((maxCols-1):maxCols)], collapse = "_")
    while(substr(fileName, nchar(fileName),nchar(fileName)) == "_"){
      fileName <- substr(fileName, 1, nchar(fileName)-1)
    }
    fileName <- paste(here::here("output", "cleanData"),"/",fileName, "_clean.pdf", sep = "")
    
    for(k in 1:len){
        for(j in 1:(ncol(dList[[k]])-1)){
          figList[[length(figList)+1]] <-ggplot(dList[[k]], aes_string(x="Time", y = colnames(dList[[k]])[j+1]))+
            geom_point(size = 0.65) + theme(axis.title.x = element_blank())
        
        }
    }    
      
        glist <- lapply(figList, ggplotGrob)
        ggsave(fileName, marrangeGrob(grobs = glist, nrow=3, ncol=1))
        ll <- ll+len
  }
      })


# Save Mean Figures -----------------------------------------------------
    
observeEvent(input$createMeanFigs,{ 

  dataFilePath <- here::here("output", "averageData") 
  fileNames <- list.files(path = dataFilePath, pattern = ".csv", full.names = TRUE) 
  '%!in%' <- function(x,y){!('%in%'(x,y))}
  
  for(i in fileNames){
    df <- read.csv(i)
    figList <- list()
    df <- df[, which(colnames(df)%!in%c("nBeats", "nBreaths", "nBursts", "bins"))]
    z <- 1
    for(j in seq(2, ncol(df), 2)){
     figList[[z]] <-  ggplot(df, aes_string(x = "Time_mean", y=colnames(df)[j])) +
      geom_point(size = 0.65) + theme(axis.title.x = element_blank()) +
      geom_errorbar(aes_string(ymin = paste(colnames(df)[j], "-", colnames(df)[j+1]),
                               ymax = paste(colnames(df)[j], "+", colnames(df)[j+1])),
                    size = 0.5)
     z<- z+1
    }
    glist <- lapply(figList, ggplotGrob)
    ggsave(paste(substr(i,1, nchar(i)-4), ".pdf", sep = ""), marrangeGrob(grobs = glist, nrow=3, ncol=1))
  }
})

}
# Create Shiny object
shinyApp(ui = ui, server = server)
