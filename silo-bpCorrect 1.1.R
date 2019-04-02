# Load packages
y <- c("shiny", "plyr", "dplyr", "reshape", "shinythemes", "ggplot2", "readr", "shinyFiles",
       "shinyjs", "taRifx", "shinydashboard", "here", "plotrix", "data.table", "gridExtra",
       "rmarkdown", "shinycssloaders", "ggthemes")
for(i in 1:length(y)){is_installed <- function(mypkg){is.element(mypkg, installed.packages()[,1])}
    if(!is_installed(y[i])){install.packages(y[i], repos="http://lib.stat.cmu.edu/R/CRAN")}
    library(y[i], character.only=TRUE, quietly=TRUE,verbose=FALSE)}

header <- dashboardHeader( titleWidth = 400)
anchor <- tags$a(tags$img(src="logo3.gif", width = 68, height = 50, align = 'left'),
                 'silo - bp correct')
header$children[[2]]$children <- tags$div(
  tags$head(tags$style(HTML('.name {font-family: "Georgia", Times;
                  background-color:white;
                  font-weight: bold;
                  font-size: 28px;}'))),
  anchor,
  class = 'name')



# Define UI
ui <- dashboardPage(skin = "black",
                    title="silo - Blood Pressure Correction",
                header,                    
                    dashboardSidebar(width = 150,
                                     uiOutput("subjectId"),
                                     uiOutput("cond1"),
                                     uiOutput("cond2"),
                                     uiOutput("cond3"),
                                     numericInput("fileStart", label = "File Start", value = NA),
                                     numericInput("fileEnd", label = "File End", value = NA),
                                     checkboxInput("appendAsk", label = "Append Id tag to Output File", value = FALSE),
                                     conditionalPanel(condition = "input.appendAsk",
                                                      textInput("appendTag", label = "Append Tag", value = NULL)
                                     ),
                                     br(),
                                    actionButton("updateFileList", "Update File List")
                                    # actionButton("resetAll", "Reset All Fields")
      ),      
             dashboardBody(
               tags$head(
                 tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
                    br(),
                    fluidRow(
                      column(2,
                             uiOutput("sysVar")
                      ),
                      column(1,
                             numericInput(inputId = "xmin", label = strong("X-Min"),
                                          value = NULL)
                      ),
                      column(1,
                             numericInput(inputId = "xmax", label = strong("X-Max"),
                                          value = NULL)
                      ),
                      column(2, offset = 2,
                             uiOutput("diaVar")
                      ),
                      column(1,
                             numericInput(inputId = "diaxmin", label = strong("X-Min"),
                                          value = NULL)
                      ),
                      column(1,
                             numericInput(inputId = "diaxmax", label = strong("X-Max"),
                                          value = NULL)
                      )
                    ),
                    fluidRow(
                      column(6, 
                             plotOutput(outputId = "bpPlot",
                                        click = "bpPlotClick",
                                        brush = brushOpts(id = "bpPlot_brush"),
                                        height = "auto")
                      ),
                      column(6, 
                             plotOutput(outputId = "diabpPlot",
                                        click = "diabpPlotClick",
                                        brush = brushOpts(id = "diabpPlot_brush"),
                                        height = "auto")
                      )
                    ),
               br(),
                    fluidRow(
                      column(1, 
                             actionButton(inputId = "blSelect", label = strong("Select BL"),
                                          value = NULL)
                      ),
                      column(1, 
                             actionButton(inputId = "blReset", label = strong("Reset Selection"),
                                                    value = NULL)
                                    ),
                      column(1,actionButton('applyCorr', strong('Apply Correction'))),
                      column(1, offset = 2, checkboxInput('showCorr', "Show Corrected Data", value = TRUE)),
                      column(2, tags$span(style="color:orange", h4("Orange = Corrected Data")))
                      
                      ),
                    br(),
                    br(),
                    fluidRow(
                    column(2, 
                           uiOutput("numCors"))
                    ),
                    fluidPage(uiOutput("corrIns")),
               fluidRow(
                 column(12, 
                        plotOutput(outputId = "bpCorrPlot",
                                   click = "bpCorrPlotClick",
                                   hover = "bpCorrPlot_hover",
                                   height = "auto")
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
        colnames(df)[which(!is.na(match(row1, "Cmt Text")))] <- "cmt"
        df <- df[-(1:2),]
      }
      return(df)
    } else {return(NULL)}
  }
  
  
  
# determine number of conditions  -----------------------------------------------------
  noCond <- reactive({
    dataFilePath <- here::here("rawData") 
    fileID <- list.files(path = dataFilePath)
    fileID <- strtrim(fileID, nchar(fileID)-4)
    fileIndex <- as.data.frame(matrix(as.character(unlist(strsplit(fileID, "_"))), 
                                      ncol = sapply(gregexpr("_", fileID[1]), length) +1, byrow = TRUE))
    ncol(fileIndex)-2
  })

# send condtion number to ui ----------------------------------------------
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
  rxVals <- reactiveValues(sysCorrFac = NULL, sysInputs = NULL, diaInputs = NULL)
  
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
    need(file.exists(fileNames[1]), paste("File doesn't exist!"))
    )
    rxVals$sysCorrFac <- NULL; rxVals$sysInputs <- NULL; rxVals$diaInputs <- NULL
    fileNames
})



# load cv data -----------------------------------------
cvData <- reactive({
  input$applyCorr
  fileNames <- fileNames()
  cvData <- txt_to_csv(fileNames[grep("beat", fileNames)])
  cvRawTimeCol <- grep("Time", colnames(cvData))
  if(length(cvRawTimeCol)>1){
    cvData <- cvData[,-cvRawTimeCol[2]]
    cvRawTimeCol <- cvRawTimeCol[1]
  }
  cvData <- data.frame(lapply(cvData, as.character), stringsAsFactors=FALSE)
  if(length(grep("cmt",colnames(cvData)))>0){cmtData <- cvData$cmt} 
  suppressWarnings(cvData <- data.frame(lapply(cvData, as.numeric), stringsAsFactors=FALSE))
  if(length(grep("cmt",colnames(cvData)))>0){cvData$cmt <- cmtData} 
  colnames(cvData)[cvRawTimeCol] <- "Time"
  cvData <- cvData[rowSums(is.na(cvData))!=ncol(cvData), ]
  bpsCmts <- unique(cvData$cmt[grep("bpCor",cvData$cmt)])
  rxVals$sysInputs <- as.numeric(unlist(lapply(strsplit(substr(bpsCmts, 7, nchar(bpsCmts)), "/"), '[[',1)))
  rxVals$diaInputs <- as.numeric(unlist(lapply(strsplit(substr(bpsCmts, 7, nchar(bpsCmts)), "/"), '[[',2)))
  if(!is.null(rxVals$sysCorrFac)){
    cvData$SBP_corr <- cvData[,input$sysVar] - rxVals$sysCorrFac
    cvData$DBP_corr <- cvData[,input$diaVar] - rxVals$diaCorrFac
    cvData$MAP_corr <- (cvData$SBP*1/3)+(cvData$DBP*2/3)
  }
  if(!is.na(input$fileStart)){cvData <- cvData[cvData$Time >= input$fileStart,]}
  if(!is.na(input$fileEnd)){cvData <- cvData[cvData$Time <= input$fileEnd,]}
  cvData
})


# output variable selections ----------------------------------------------
output$sysVar <- renderUI({
  req(input$subjectId)
  sbpNum <- grep("sbp", colnames(cvData()), ignore.case = TRUE) ### fix this to not selectthe corrected one 
  sbpNum <- sbpNum[1]
  if(length(sbpNum)>0){selectedVar <- colnames(cvData())[sbpNum]}else{selectedVar <- input$sysVar}
  selectInput(inputId = "sysVar", label = strong("SBP Variable"),
              selected = selectedVar,
              choices = colnames(cvData()))
})
output$diaVar <- renderUI({
  req(input$subjectId)
  dbpNum <- grep("dbp", colnames(cvData()), ignore.case = TRUE)
  dbpNum <- dbpNum[1]
  if(length(dbpNum)>0){selectedVar <- colnames(cvData())[dbpNum]}else{selectedVar <- input$sysVar}
  selectInput(inputId = "diaVar", label = strong("DBP Variable"),
              selected = selectedVar,
              choices = colnames(cvData()))
})
output$cmtVar <- renderUI({
  req(input$subjectId)
  cmtNum <- grep("cmt", colnames(cvData()), ignore.case = TRUE)
  if(length(cmtNum)>0){selectedVar <- colnames(cvData())[cmtNum]}else{selectedVar <- input$cmtVar}
  selectInput(inputId = "cmtVar", label = strong("CMT Column"),
              selected = selectedVar,
              choices = colnames(cvData()))
})
# Create Raw Plot --------------------------------------------------------
observeEvent(c(input$subjectId,
               input$cond1,
               input$cond2,
               input$cond3),{
         rxVals$meanKeepRows <-  rep(TRUE, isolate(nrow(cvData())))
})

output$bpPlot <- renderPlot({
  input$applyCorr
  req(input$sysVar)
  cvData <- cvData()
  keepAv    <- cvData[ rxVals$meanKeepRows , , drop = FALSE]
  excludeAv <- cvData[!rxVals$meanKeepRows , , drop = FALSE]
  p1 <- ggplot(keepAv, aes_string( 
    x="Time",
    y=input$sysVar))+
    geom_point(colour='#268bd2')+
    geom_point(data =excludeAv, colour='#d33682')+
    theme(axis.title.x=element_blank(),
          plot.background = element_rect(fill = "grey93")) +
    coord_cartesian() +
    scale_x_continuous(limits = c(input$xmin, input$xmax)) +
    theme_solarized()+
    scale_colour_solarized('blue')
  # 
  if(length(grep("SBP_corr", colnames(cvData())))>0 & input$showCorr){
    p1<- p1+ geom_point(aes(y = SBP_corr), colour='orange', alpha = 0.5)
    }
  p1
  
},
height = function() {
  session$clientData$output_bpPlot_width/4
}
)

output$diabpPlot <- renderPlot({
  input$applyCorr
  req(input$diaVar)
  cvData <- cvData()
  keepAv    <- cvData[ rxVals$meanKeepRows , , drop = FALSE]
  excludeAv <- cvData[!rxVals$meanKeepRows , , drop = FALSE]
  p1 <- ggplot(keepAv, aes_string( 
    x="Time",
    y=input$diaVar))+
    geom_point(colour='#d33682')+
    geom_point(data =excludeAv, colour='#268bd2')+
    theme(axis.title.x=element_blank(),
          plot.background = element_rect(fill = "grey93")) +
    coord_cartesian() +
    scale_x_continuous(limits = c(input$diaxmin, input$diaxmax)) +
    theme_solarized()+
    scale_colour_solarized('blue')
  # 
  if(length(grep("DBP_corr", colnames(cvData())))>0 & input$showCorr){
    p1<- p1+ geom_point(aes(y = DBP_corr), colour='orange', alpha = 0.5)
  }
  p1
  
},
height = function() {
  session$clientData$output_bpPlot_width/4
}
)

# user input for raw plots ------------------------------------------------
# Toggle points that are clicked on plot 1
observeEvent(input$bpPlotClick, {
  res <- nearPoints(cvData(), input$bpPlotClick, allRows = TRUE)
  rxVals$meanKeepRows  <- xor(rxVals$meanKeepRows , res$selected_)
})
# Toggle points that are brushed, when button is clicked on plot 1
observeEvent(input$blSelect, {
  res <- brushedPoints(cvData(), input$bpPlot_brush, allRows = TRUE)
  rxVals$meanKeepRows  <- xor(rxVals$meanKeepRows , res$selected_)
})

# Reset all points for plot 1
observeEvent(input$blReset, {
  rxVals$meanKeepRows <-  rep(TRUE, isolate(nrow(cvData())))
})
# Toggle points that are clicked on plot 1
observeEvent(input$diabpPlotClick, {
  res <- nearPoints(cvData(), input$diabpPlotClick, allRows = TRUE)
  rxVals$meanKeepRows  <- xor(rxVals$meanKeepRows , res$selected_)
})
# Toggle points that are brushed, when button is clicked on plot 1
observeEvent(input$blSelect, {
  res <- brushedPoints(cvData(), input$diabpPlot_brush, allRows = TRUE)
  rxVals$meanKeepRows  <- xor(rxVals$meanKeepRows , res$selected_)
})

# Reset all points for plot 1
observeEvent(input$blReset, {
  rxVals$meanKeepRows <-  rep(TRUE, isolate(nrow(cvData())))
})

# # Reset plot coordinates --------------------------------------------------
# observeEvent(input$plotReset,{
#   updateTextInput(session, "xmin", value = NA)
#   updateTextInput(session, "xmax", value = NA)
#   updateTextInput(session, "ymin", value = NA)
#   updateTextInput(session, "ymax", value = NA)
# })


# dynamic select text input ------------------------------------------------------
output$numCors <- renderUI({
  if(!is.null(rxVals$sysInputsNu)){startVal <- length(rxVals$sysInputs)}else{startVal <- 3}
  numericInput(inputId = "noCorrs", value = startVal, label = strong("# of Manual BPs"),
               min = 1)
})

output$corrIns <- renderUI({
  validate(need(input$noCorrs>0, "Must be at least one!"))
  pvars <- 1:input$noCorrs
  # cvData[input$cmtVar]
  div(
  fluidRow(lapply(seq(pvars), function(i) {
    column(1,numericInput(inputId = paste0("sys", pvars[i]),
                label = paste("Systolic",pvars[i]), value = rxVals$sysInputs[i]))
  })),
  fluidRow(lapply(seq(pvars), function(i) {
    column(1,numericInput(inputId = paste0("dia", pvars[i]),
                       label = paste("Diastolic",pvars[i]), value = rxVals$diaInputs[i]))
  }))
)
})

observeEvent(input$applyCorr,{
    cvData <- cvData()
    avData <- cvData[!rxVals$meanKeepRows , , drop = FALSE]
    avData <- summarise_all(avData, .funs = mean,na.rm = TRUE)
    if(input$noCorrs > 0){
      sysVal <- paste0("sys", 1:input$noCorrs)
      diaVal <- paste0("dia", 1:input$noCorrs)
      sysStages <- character(0)
      diaStages <- character(0)
      for(i in sysVal){sysStages <- append(sysStages, input[[i]])}
      for(i in diaVal){diaStages <- append(diaStages, input[[i]])}
      sysStages <- as.numeric(sysStages); diaStages <- as.numeric(diaStages)
    }
    rxVals$sysCorrFac <- avData[,input$sysVar] - mean(sysStages, na.rm = TRUE)
    rxVals$diaCorrFac <- avData[,input$diaVar] - mean(diaStages, na.rm = TRUE)
})

# Save Data -------------------------------------------------------
observeEvent(input$applyCorr, {
  fileNames <- fileNames()
  fileName <- fileNames[1]
  fileExt <- substr(fileName, nchar(fileName)-3, nchar(fileName))
  fileName <- strtrim(fileName,nchar(fileName)-4)
  if(input$appendAsk==TRUE){
    fileName <- paste0(substr(fileName,1, nchar(fileName)-3),"-", input$appendTag, fileExt)
  }else{fileName <- paste0(fileName, fileExt)}
  if(fileExt ==".csv"){
    write.csv(cvData(), fileName, row.names = FALSE)
  }else{
    write.table(cvData(), fileName, row.names = FALSE, sep = "\t")
  }
})
}

# Create Shiny object
shinyApp(ui = ui, server = server)
