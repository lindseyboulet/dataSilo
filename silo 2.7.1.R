# Load packages
y <- c("shiny", "plyr", "dplyr", "reshape", "shinythemes", "ggplot2", "readr", "shinyFiles",
       "shinyjs", "taRifx", "shinydashboard", "here", "plotrix", "data.table", "gridExtra",
       "rmarkdown")
for(i in 1:length(y)){is_installed <- function(mypkg){is.element(mypkg, installed.packages()[,1])}
    if(!is_installed(y[i])){install.packages(y[i], repos="http://lib.stat.cmu.edu/R/CRAN")}
    library(y[i], character.only=TRUE, quietly=TRUE,verbose=FALSE)}

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
                                     uiOutput("subjectId"),
                                     uiOutput("cond1"),
                                     uiOutput("cond2"),
                                     uiOutput("cond3"),
                                     numericInput("fileStart", label = "File Start", value = NA),
                                     numericInput("fileEnd", label = "File End", value = NA),
                                     numericInput("protoStart", label = "Start of Protocol", value = NA),
                                     br(),
                                     actionButton("resetTimes", "Reset Time Points"),
                                    checkboxInput("appendAsk", label = "Append Id tag to Output File", value = FALSE),
                                    conditionalPanel(condition = "input.appendAsk",
                                                     textInput("appendTag", label = "Append Tag", value = NULL)
                                    ),
                                    actionButton("updateFileList", "Update File List")
      ),      
             dashboardBody(
              tabsetPanel(
                tabPanel("Raw Data",
                   # Select cond to be use
                   br(),
                   fluidRow(
                     column(3,
                         uiOutput("cvVarSelect")),
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
                                    value = NULL)),
                   column(1, 
                          actionButton('resetPlot1', "Reset"))
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
                            uiOutput("respVarSelect")),
                            
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
                                      value = NULL)),
                     column(1, 
                            actionButton('resetPlot2', "Reset"))),
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
             conditionalPanel(condition = "output.burstAsk",
               fluidRow(
                 column(3,
                        uiOutput("burstVarSelect")),
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
                                  value = NULL)),
                 column(1, 
                        actionButton('resetPlot3', "Reset"))
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
                  column(4, uiOutput("cvVars")),
                  column(4, uiOutput("respVars")),
                  conditionalPanel(condition = "input.burstAsk",
                       column(4, uiOutput("burstVars"))
                  )
                ),
                fluidRow(
                  column(4, uiOutput("zeroRound")),
                  column(4, uiOutput("twoRound"))
                )
             ),
             tabPanel("Averaged Data",
                      fluidRow(
                        column(1,
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
                                            value = NULL)),
                         column(1,
                                actionButton(inputId = "avReset", label = strong("Reset"),
                                             value = NULL))
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
  
# Function to load data based on filename
  txt_to_csv <- function(fileName){
    if(file.exists(fileName)){
      if(substr(fileName, nchar(fileName)-2, nchar(fileName)) == "csv"){
        df <- read.csv(fileName)
      } else {
        df <- read.delim(fileName)
        row1 <- data.frame(lapply(df[1,], as.character), stringsAsFactors=FALSE)
        colnames(df)[which(!is.na(match(row1, "Time")))] <- "Time"
        df <- df[-(1:2),]
      }
      return(df)
    } else {return(NULL)}
  }
  
  # determine number of conditions  -----------------------------------------------------
  noCond <- reactive({
    dataFilePath <- here::here("rawData") #### NEED TO CHANGE THIS TO FOLDER WITH RAW DATA ####
    fileID <- list.files(path = dataFilePath)
    fileID <- strtrim(fileID, nchar(fileID)-4)
    fileIndex <- as.data.frame(matrix(as.character(unlist(strsplit(fileID, "_"))), 
                                      ncol = sapply(gregexpr("_", fileID[1]), length) +1, byrow = TRUE))
    ncol(fileIndex)-2
  })
  # send condtion number to ui
  output$noCond <- reactive({noCond()})
  outputOptions(output, "noCond", suspendWhenHidden = FALSE) ## so the app can rectively guess the conditions
  
  # create file index -------------------------------------------------------

  fileIndex <- reactive({
    input$updateFileList
    fileID <- list.files(path =  here::here("rawData"))
    fileType <- substr(fileID, nchar(fileID)-2, nchar(fileID))
    fileID <- strtrim(fileID, nchar(fileID)-4)
    fileIndex <- as.data.frame(matrix(as.character(unlist(strsplit(fileID, "_"))), 
                                      ncol = sapply(gregexpr("_", fileID[1]), length) +1, byrow = TRUE))
    colnames(fileIndex)[1:3] <- c("type", "subject", "cond1")
    if(noCond() >=2 ){colnames(fileIndex)[4] <- "cond2"}
    if(noCond() ==3 ){colnames(fileIndex)[5] <- "cond3"}
    fileIndex <- mutate_all(fileIndex, as.character)
    fileIndex$fileType <- fileType
    fileIndex
  })
  
  # send info if burst exists to ui
  output$burstAsk <- reactive({
    fileIndex <- fileIndex()
    length(unique(fileIndex$type)) == 3 
    })
  outputOptions(output, "burstAsk", suspendWhenHidden = FALSE) ## so the app can rectively guess the conditions
  
  # output sidebar inputs -------------------------------------------------------
  output$subjectId <- renderUI({
    fileIndex <- fileIndex() 
    selectInput(inputId = "subjectId", label = strong("Subject"),choices = unique(fileIndex$subject))
  })
  output$cond1 <- renderUI({
    fileIndex <- fileIndex() 
    selectInput(inputId = "cond1", label = strong("Condition 1"),choices = unique(fileIndex$cond1))
  })
  output$cond2 <- renderUI({ 
  if(noCond() >= 2){
    fileIndex <- fileIndex() 
    selectInput(inputId = "cond2", label = strong("Condition 2"),choices = unique(fileIndex$cond2))
  }})
output$cond3 <- renderUI({
  if(noCond() == 3){
    fileIndex <- fileIndex()
    selectInput(inputId = "cond3", label = strong("Condition 3"),choices = unique(fileIndex$cond3))
  }})

# Create cv, breath and burst filenames -----------------------------------------
fileNames <- reactive({
  fileIndex <- fileIndex()
  useFileID <- subset(fileIndex, subject == input$subjectId & cond1 == input$cond1)
  if(noCond() >= 2){useFileID <- subset(useFileID, cond2 == input$cond2)}
  if(noCond() == 3){useFileID <- subset(useFileID, cond3 == input$cond3)}
  useFileID <- apply(useFileID,1, paste, collapse = "_")
  for(i in 1:length(useFileID)){
    useFileID[i] <- gsub("_([^_]*)$", ".\\1", useFileID[i])
  }
  fileNames <- paste(here::here("rawData"), useFileID, sep = "/")
  validate(
    need(file.exists(fileNames[1]), paste("File doesn't exist!")),
    need(file.exists(fileNames[2]), paste("File doesn't exist!"))
    )
    fileNames
})

# load cv, resp and burst data -----------------------------------------
cvData <- reactive({
  fileNames <- fileNames()
  cvData <- txt_to_csv(fileNames[grep("beat", fileNames)])
  cvRawTimeCol <- grep("Time", colnames(cvData))
  if(length(cvRawTimeCol)>1){ 
    cvData <- cvData[,-cvRawTimeCol[2]]
    cvRawTimeCol <- cvRawTimeCol[1]
  }
  cvData <- data.frame(lapply(cvData, as.character), stringsAsFactors=FALSE)
  suppressWarnings(cvData <- data.frame(lapply(cvData, as.numeric), stringsAsFactors=FALSE))
  colnames(cvData)[cvRawTimeCol] <- "Time"
  if(!is.na(input$fileStart)){cvData <- cvData[cvData$Time >= input$fileStart,]}
  if(!is.na(input$fileEnd)){cvData <- cvData[cvData$Time <= input$fileEnd,]}
  if(!is.na(input$protoStart)){cvData$Time <- cvData$Time - input$protoStart}
  cvData <- cvData[rowSums(is.na(cvData))!=ncol(cvData), ]
  cvData
})
respData <- reactive({
  fileNames <- fileNames()
   respData <- txt_to_csv(fileNames[grep("breath", fileNames)])
   respRawTimeCol <- grep("Time", colnames(respData))
   if(length(respRawTimeCol)>1){ 
     respData <- respData[,-respRawTimeCol[2]]
     respRawTimeCol <- respRawTimeCol[1]
   }
   respData <- data.frame(lapply(respData, as.character), stringsAsFactors=FALSE)
   suppressWarnings(respData <- data.frame(lapply(respData, as.numeric), stringsAsFactors=FALSE))
   colnames(respData)[respRawTimeCol] <- "Time"
   if(!is.na(input$fileStart)){respData <- respData[respData$Time >= input$fileStart,]}
   if(!is.na(input$fileEnd)){respData <- respData[respData$Time <= input$fileEnd,]}
   if(!is.na(input$protoStart)){respData$Time <- respData$Time - input$protoStart}
   respData <- respData[rowSums(is.na(respData))!=ncol(respData), ]
   respData
})
burstData <- reactive({
  fileNames <- fileNames()
  if(length(fileNames) ==3){
    burstData <- txt_to_csv(fileNames[grep("burst", fileNames)])
    burstData <- burstData[-1]
    burstRawTimeCol <- which(!is.na(match(colnames(burstData), "TimeDate")))
    burstData <- data.frame(lapply(burstData, as.character), stringsAsFactors=FALSE)
    burstData[, burstRawTimeCol] <-substr(burstData[, burstRawTimeCol],
                                              11, nchar(burstData[, burstRawTimeCol]))    
    suppressWarnings(burstData <- data.frame(lapply(burstData, as.numeric), stringsAsFactors=FALSE))
    colnames(burstData)[burstRawTimeCol]<- "Time"
    if(!is.na(input$fileStart)){burstData <- burstData[burstData$Time > input$fileStart,]}
    if(!is.na(input$fileEnd)){burstData <- burstData[burstData$Time < input$fileEnd,]}
    if(!is.na(input$protoStart)){burstData$Time <- burstData$Time - input$protoStart}
    burstData <- burstData[rowSums(is.na(burstData))!=ncol(burstData), ]
    burstData
  }else{NULL}
})

# output variable selections ----------------------------------------------
output$cvVarSelect <- renderUI({
  req(input$subjectId)
  selectInput(inputId = "cvVarSelect", label = strong("Select CV File Variable"),choices = colnames(cvData()))
})
output$respVarSelect <- renderUI({
  req(input$subjectId)
  selectInput(inputId = "respVarSelect", label = strong("Select Resp File Variable"),choices = colnames(respData()))
})
output$burstVarSelect <- renderUI({
  req(input$subjectId)
  selectInput(inputId = "burstVarSelect", label = strong("Select Burst File Variable"),choices = colnames(burstData()))
})



# Create Raw Plots --------------------------------------------------------
# For storing which rows have been excluded
rxVals <- reactiveValues(
  cvKeepRows = NULL, respKeepRows = NULL, burstKeepRows = NULL)

observeEvent(c(input$subjectId,
               input$cond1,
               input$cond2,
               input$cond3),{
  rxVals$cvKeepRows <-  data.frame(matrix(1,nrow = isolate(nrow(cvData())),
                                          ncol = isolate(ncol(cvData())), byrow = FALSE))
  colnames(rxVals$cvKeepRows) <- colnames(cvData())
  rxVals$respKeepRows <-  data.frame(matrix(1,nrow = isolate(nrow(respData())),
                                          ncol = isolate(ncol(respData())), byrow = FALSE))
  colnames(rxVals$respKeepRows) <- colnames(respData())
  if(!is.null(burstData())){
    rxVals$burstKeepRows <-  data.frame(matrix(1,nrow = isolate(nrow(burstData())),
                                              ncol = isolate(ncol(burstData())), byrow = FALSE))
    colnames(rxVals$burstKeepRows) <- colnames(burstData())
    }
})

cvCleanData <- reactive({
  cvData <- cvData()
  cvData[which(rxVals$cvKeepRows != 1, arr.ind = TRUE)] <- NA
  cvData
})

respCleanData <- reactive({
  respData <- respData()
  respData[which(rxVals$respKeepRows != 1, arr.ind = TRUE)] <- NA
  respData
})

burstCleanData <- reactive({
  burst <- burstData()
  burst[which(rxVals$burstKeepRows != 1, arr.ind = TRUE)] <- NA
  burst
})
# cv raw plot
output$cvRawplot <- renderPlot({
  req(input$cvVarSelect)
  cvData <- cvCleanData()
  ggplot(cvData, aes_string( 
    x="Time",
    y=input$cvVarSelect))+
    geom_point(colour='#e41a1c')+
    theme(axis.title.x=element_blank(),
          plot.background = element_rect(fill = "grey93")) +
    coord_cartesian() +
    scale_x_continuous(limits = as.numeric(c(input$xmin, input$xmax))) +
    scale_y_continuous(limits = as.numeric(c(input$ymin, input$ymax)))
  
},
height = function() {
  session$clientData$output_cvRawplot_width/4
  
}
)
# resp raw plot
output$respRawplot <- renderPlot({
  req(input$respVarSelect)
  respData <- respCleanData()
  ggplot(respData, aes_string( 
    x="Time",
    y=input$respVarSelect))+
    geom_point(colour='#377eb8')+
    theme(axis.title.x=element_blank(),
          plot.background = element_rect(fill = "grey93")) +
    coord_cartesian() +
    scale_x_continuous(limits = as.numeric(c(input$xminb, input$xmaxb))) +
    scale_y_continuous(limits = as.numeric(c(input$yminb, input$ymaxb)))
},
height = function() {
  session$clientData$output_respRawplot_width/4
}
)
# burst raw plot
output$burstRawplot <- renderPlot({
  if(!is.null(burstData())){
    req(input$burstVarSelect)
    burstData <- burstCleanData()

    ggplot(burstData, aes_string(x="Time",
                                 y=input$burstVarSelect))+
      geom_point(colour='#4daf4a')+
      theme(axis.title.x=element_blank(),
            plot.background = element_rect(fill = "grey93")) +
      coord_cartesian() +
      scale_x_continuous(limits = as.numeric(c(input$xminc, input$xmaxc))) +
      scale_y_continuous(limits = as.numeric(c(input$yminc, input$ymaxc)))
  }else{NULL}
},
height = function() {
  session$clientData$output_burstRawplot_width/4
}
)

# user input for raw plots ------------------------------------------------

# Toggle points that are clicked on plot 1
observeEvent(input$plot1_click, {
  res <- nearPoints(cvData(), input$plot1_click, allRows = TRUE)
  rxVals$cvKeepRows[input$cvVarSelect][which(res$selected_==TRUE),1] <- 0
})
# Toggle points that are brushed, when button is clicked on plot 1
observeEvent(input$exclude_toggle, {
  res <- brushedPoints(cvData(), input$plot1_brush, allRows = TRUE)
  rxVals$cvKeepRows[input$cvVarSelect][which(res$selected_==TRUE),1] <- 0
})
# Hover for plot 1
output$hover_info1 <- renderPrint({
  req(input$cvVarSelect)
  cvData <- cvData()
  yVal <- which(colnames(cvData) == input$cvVarSelect)
  HTML(hoverValue(input$plot1_hover, cvData$Time, cvData[,yVal], cvData$Time))
})
# Reset all points for plot 1
observeEvent(input$exclude_reset, {
  rxVals$cvKeepRows <-  data.frame(matrix(1,nrow = isolate(nrow(cvData())),
                                          ncol = isolate(ncol(cvData())), byrow = FALSE))
  colnames(rxVals$cvKeepRows) <- colnames(cvData())
})

# Toggle points that are clicked on plot 2
observeEvent(input$plot2_click, {
  res <- nearPoints(respData(), input$plot2_click, allRows = TRUE)
  rxVals$respKeepRows[input$respVarSelect][which(res$selected_==TRUE),1] <- 0
})
# Toggle points that are brushed, when button is clicked on plot 2
observeEvent(input$exclude_toggle2, {
  res <- brushedPoints(respData(), input$plot2_brush, allRows = TRUE)
  rxVals$respKeepRows[input$respVarSelect][which(res$selected_==TRUE),1] <- 0
})
# Hover for plot 2
output$hover_info2 <- renderPrint({
  req(input$respVarSelect)
  respData <- respData()
  yVal <- which(colnames(respData) == input$respVarSelect)
  HTML(hoverValue(input$plot2_hover, respData$Time, respData[,yVal], respData$Time))
})
# Reset all points for plot 2
observeEvent(input$exclude_reset2, {
  rxVals$respKeepRows <-  data.frame(matrix(1,nrow = isolate(nrow(respData())),
                                          ncol = isolate(ncol(respData())), byrow = FALSE))
  colnames(rxVals$respKeepRows) <- colnames(respData())
})
observe({
  if(!is.null(burstData())){
    # Toggle points that are clicked on plot 3
    observeEvent(input$plot3_click, {
      res <- nearPoints(burstData(), input$plot3_click, allRows = TRUE)
      rxVals$burstKeepRows[input$burstVarSelect][which(res$selected_==TRUE),1] <- 0
    })
    # Toggle points that are brushed, when button is clicked on plot 3
    observeEvent(input$exclude_toggle3, {
      res <- brushedPoints(isolate(burstData()), input$plot3_brush, allRows = TRUE)
      rxVals$burstKeepRows[input$burstVarSelect][which(res$selected_==TRUE),1] <- 0
    })
    # Hover for plot 3
    output$hover_info3 <- renderPrint({
      burstData <- burstData()
      yVal <- which(colnames(burstData) == input$burstVarSelect)
      HTML(hoverValue(input$plot3_hover, burstData$Time, burstData[,yVal], burstData$Time))
    })
    # Reset all points for plot 3
    observeEvent(input$exclude_reset3, {
      rxVals$burstKeepRows <-  data.frame(matrix(1,nrow = isolate(nrow(burstData())),
                                                ncol = isolate(ncol(burstData())), byrow = FALSE))
      colnames(rxVals$burstKeepRows) <- colnames(burstData())
    })
  }
})

observeEvent(input$resetTimes,{
  updateNumericInput(session, "fileStart", value = NA)
  updateNumericInput(session, "fileEnd", value = NA)
  updateNumericInput(session, "protoStart", value = NA)
 })

observeEvent(input$resetPlot1,{
  updateTextInput(session, "xmin", value = NA)
  updateTextInput(session, "xmax", value = NA)
  updateTextInput(session, "ymin", value = NA)
  updateTextInput(session, "ymax", value = NA)
})
observeEvent(input$resetPlot2,{
  updateTextInput(session, "xminb", value = NA)
  updateTextInput(session, "xmaxb", value = NA)
  updateTextInput(session, "yminb", value = NA)
  updateTextInput(session, "ymaxb", value = NA)
})
observeEvent(input$resetPlot3,{
  updateTextInput(session, "xminc", value = NA)
  updateTextInput(session, "xmaxc", value = NA)
  updateTextInput(session, "yminc", value = NA)
  updateTextInput(session, "ymaxc", value = NA)
})
observeEvent(input$avReset,{
  updateTextInput(session, "av1Xmin", value = NA)
  updateTextInput(session, "av1Xmax", value = NA)
  updateTextInput(session, "av1Ymin", value = NA)
  updateTextInput(session, "av1Ymax", value = NA)
})

# Tab 2 selections --------------------------------------------------------
# variables for each data type
output$cvVars <- renderUI({
  selectInput(inputId = "cvVars", label = strong("Select Cardiovascular Variables"),choices = colnames(isolate(cvData())),
              multiple=TRUE, selectize=TRUE)
})
output$respVars <- renderUI({
  selectInput(inputId = "respVars", label = strong("Select Respiratory Variables"),choices = colnames(isolate(respData())),
              multiple=TRUE, selectize=TRUE)
})
output$burstVars <- renderUI({
  selectInput(inputId = "burstVars", label = strong("Select Burst Variables"),choices = colnames(isolate(burstData())),
              multiple=TRUE, selectize=TRUE)
})

# variables for rounding
output$zeroRound <- renderUI({
  req(input$cvVars)
  selectInput(inputId = "zeroRound", label = strong("Round to Zero Decimal Places"),
              choices =  unique(c(input$cvVars, input$respVars, input$burstVars)),
              multiple=TRUE, selectize=TRUE)
})
output$twoRound <- renderUI({
  req(input$cvVars)
  selectInput(inputId = "twoRound", label = strong("Round to Two Decimal Places"),
              choices =  unique(c(input$cvVars, input$respVars, input$burstVars)),
              multiple=TRUE, selectize=TRUE)
})

# Averaging and merging data ----------------------------------------------
averageData <- reactive({
  cvData <- cvCleanData()
  respData <- respCleanData()
  if (!is.null(input$cvVars)){
    cvColRange <- which(!is.na(match(colnames(cvData), input$cvVars)))
  } else {cvColRange <- 2:6}
  cvTimeCol <- which(colnames(cvData) == "Time")
  if (!is.null(input$respVars)){
    respColRange <- which(!is.na(match(colnames(respData), input$respVars)))
  } else {respColRange <- 8:9}
  respTimeCol <- which(colnames(respData) == "Time")
  
  if(!is.null(burstData())){
    burstData <- burstCleanData()
    if (!is.null(input$burstVars)){
      burstColRange <- which(!is.na(match(colnames(burstData), input$burstVars)))
    } else {burstColRange <- 2:6}  
    burstTimeCol <- which(colnames(burstData) == "Time")
    burstTimeCol <- burstTimeCol[1]
  }

  # Run averaging function
  cvData[cvData == "#NUM!"] <- NA
  respData[respData == "#NUM!"] <- NA
  cvColRange <- c(cvTimeCol, cvColRange)
  respColRange <- c(respTimeCol, respColRange)
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
  
  if(!is.null(burstData())){
    burstData[burstData == "#NUM!"] <- NA
    burstColRange <- c(burstTimeCol, burstColRange)
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
  
  if(!is.null(burstData())){
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
  }
  if(!dir.exists(here::here("output", "averageData"))){
    dir.create(here::here("output", "averageData"))
  }
  if(!dir.exists(here::here("output", "selectData"))){
    dir.create(here::here("output", "selectData"))
  }
  if(!dir.exists(here::here("output", "cleanData"))){
    dir.create(here::here("output", "cleanData"))
  }
  finalData <- finalData[which(complete.cases(finalData$bins)),]
  finalData <- finalData[order(finalData$Time_mean),]
  finalData <- finalData[rowSums(is.na(finalData))!=ncol(finalData), ]
  finalData
})

# Plot Mean Data ----------------------------------------------------------
# create y var selections
observe({
  if(!is.null(burstData())){
    updateSelectInput(session,"plot1Var",choices=c(input$cvVars,input$respVars,input$burstVars))
  } else{
    updateSelectInput(session,"plot1Var",choices=c(input$cvVars,input$respVars))
  }
})

# create reactive vals for plot click
observeEvent(input$cvVars, {
  rxVals$meanKeepRows <-  rep(TRUE, isolate(nrow(averageData())))
  })

# mean plot
output$meanPlot1 <- renderPlot({
  req(input$bin)
  avData <- averageData()
  keepAv    <- avData[ rxVals$meanKeepRows , , drop = FALSE]
  excludeAv <- avData[!rxVals$meanKeepRows , , drop = FALSE]
  ggplot(keepAv, aes_string(x= "Time_mean", y=paste(input$plot1Var, "_mean",sep = ""))) +
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
  rxVals$meanKeepRows  <- xor(rxVals$meanKeepRows , res$selected_)
})
# Reset all points for plot 3
observeEvent(input$exclude_reset4, {
  rxVals$meanKeepRows  <- rep(TRUE, nrow(isolate(averageData())))
})

# Reactive Select Data -------------------------------------------------------------
selectData <- reactive({
  avData <- averageData()
  selectedAv <- avData[!rxVals$meanKeepRows , , drop = FALSE]
  selectedAv[rowSums(is.na(selectedAv))!=ncol(selectData), ]
  if(input$noStages > 0){
    stages <- c(input$st1, input$st2, input$st3,
                input$st4, input$st5, input$st6)
    stagesFinal <- vector()
    for(i in 1:input$noStages){stagesFinal <- c(stagesFinal, stages[i])}
    selectedAv$stage <-rep(stagesFinal, length.out=nrow(selectedAv))
  }
  selectedAv <- selectedAv[rowSums(is.na(selectedAv))!=ncol(selectedAv), ]
  selectedAv[,c(1:2,ncol(selectedAv), 3:(ncol(selectedAv)-1)) ]
})

# output filename
outFile <- reactive({
  fileIndex <- fileIndex()
  useFileID <- subset(fileIndex, subject == input$subjectId & cond1 == input$cond1)
  if(noCond() >= 2){useFileID <- subset(useFileID, cond2 == input$cond2)}
  if(noCond() == 3){useFileID <- subset(useFileID, cond3 == input$cond3)}
  useFileID <- apply(useFileID[,-c(1, ncol(useFileID))],1, paste, collapse = "_")
  useFileID[1]
})

# Save Average Data -------------------------------------------------------   
observeEvent(input$saveAverage, {
  fileName <- paste(here::here("output", "averageData"),"/", outFile(), '_', input$bin, "sec_mean_all.csv", sep = "")
  if(input$appendAsk==TRUE){fileName <- paste(strtrim(fileName, nchar(fileName)-4),"_", input$appendTag, ".csv", sep = "" )}
  write.csv(averageData(), fileName, row.names = FALSE)
})

# Save Select Data --------------------------------------------------------
observeEvent(input$saveSelect, {
  fileName <- paste(here::here("output", "selectData"),"/", outFile(), '_', input$bin, "sec_mean_select.csv", sep = "")
  if(input$appendAsk==TRUE){fileName <- paste(strtrim(fileName, nchar(fileName)-4),"_", input$appendTag, ".csv", sep = "" )}
  write.csv(selectData(), fileName, row.names = FALSE)
})

# Save Clean Data --------------------------------------------------------
observeEvent(input$saveClean,{ #### called from UI
  fileName <- outFile()
  fileName <- paste(fileName, "-clean.csv", sep = "")
  cvData <- cvCleanData()
  if (!is.null(input$cvVars)){
    cvColRange <- which(!is.na(match(colnames(cvData), input$cvVars)))
  } else {cvColRange <- 2:6}
  timeCol <- which(colnames(cvData) == "Time")
  cvColRange <- c(timeCol, cvColRange)
  cvData <- cvData[,cvColRange]
  respData <- respCleanData()
  if (!is.null(input$respVars)){
    respColRange <- which(!is.na(match(colnames(respData), input$respVars)))
  } else {respColRange <- 8:15}
  timeCol <- which(colnames(respData) == "Time")
  respColRange <- c(timeCol, respColRange)
  respData <- respData[,respColRange]
  if(input$appendAsk==TRUE){
    fileName <- paste(strtrim(fileName, nchar(fileName)-4),"_", 
                      input$appendTag, ".csv", sep = "" )
    }  
  if(!is.null(burstData())){
    burstData <- burstCleanData()
    if (!is.null(input$burstVars)){
      burstColRange <- which(!is.na(match(colnames(burstData), input$burstVars)))
    } else {burstColRange <- 2:6}  
    timeCol2 <- which(colnames(burstData) == "Time")
    timeCol2 <- timeCol2[1]
    burstColRange <- c(timeCol2, burstColRange)
    burstData <- burstData[, burstColRange]
    burstData <- burstData[rowSums(is.na(burstData))!=ncol(burstData), ]
    write.csv(burstData, paste(here::here("output", "cleanData"),"/","burst_", fileName, sep = ""), row.names = FALSE)
  }
  cvData <- cvData[rowSums(is.na(cvData))!=ncol(cvData), ]
  respData <- respData[rowSums(is.na(respData))!=ncol(respData), ]
  write.csv(cvData, paste(here::here("output", "cleanData"),"/","beat_", fileName, sep = ""), row.names = FALSE)
  write.csv(respData, paste(here::here("output", "cleanData"),"/","breath_", fileName, sep = ""), row.names = FALSE)
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

  targetFiles <- subset(fileIndex, V1 == "beat")
  fileNameList <- character(0)
  for(i in 1:nrow(targetFiles)){
    fileNameList[i] <-  paste(fileIndex[i,2:ncol(fileIndex)], collapse = "_")
  }
  nType <- length(unique(fileIndex[,1]))
  printList <- list(beat = NULL, breath = NULL)
  for(i in fileNameList){
    beatFile <- paste(here::here("output", "cleanData"),"/","beat_", i, ".csv", sep = "")
    breathFile <- paste(here::here("output", "cleanData"),"/","breath_", i, ".csv", sep = "")
    printList$beat <- read.csv(beatFile)
    printList$breath <- read.csv(breathFile)
    if(nType == 3){
      beatFile <- paste(here::here("output", "cleanData"),"/","burst_", i, ".csv", sep = "")
    }
    outFileName <- paste(here::here("output", "cleanData"),"/", i, "-clean.pdf", sep = "")
    figList <- list()
    for(k in printList){
      for(j in 1:(ncol(k)-1)){
        figList[[j]] <-ggplot(k, aes_string(x="Time", y = colnames(k)[j+1]))+
          geom_point(size = 0.65) + theme(axis.title.x = element_blank())
      }
    }    
    glist <- lapply(figList, ggplotGrob)
    ggsave(outFileName, marrangeGrob(grobs = glist, nrow=3, ncol=1))
  } 
  

   
})

# Save Mean Figures -----------------------------------------------------
observeEvent(input$createMeanFigs,{ 
  dataFilePath <- here::here("output", "averageData") 
  fileNames <- list.files(path = dataFilePath, pattern = ".csv", full.names = TRUE) 
  '%!in%' <- function(x,y){!('%in%'(x,y))}
  for(i in fileNames){
    if (!file.exists(paste(substr(i,1, nchar(i)-4), ".pdf", sep = ""))){
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
  }
})
}

# Create Shiny object
shinyApp(ui = ui, server = server)
