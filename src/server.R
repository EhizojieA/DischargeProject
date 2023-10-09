# Redacted version of server.R
# Redacted by Z. Nickerson (nickerson@battelleecology.org) on 2023-10-09
# Redactions will appear as "ZN-REDACTED" with a brief description of the function of the redacted line

function(input, output, session) {
  # Sets the requirement for server code to run as file upload so the code does not run prematurely and crash the application. 
  datapath <- shiny::reactive({ 
    shiny::req(input$fileUpload)
  })
  
  ingestTable <- shiny::eventReactive(input$fileUpload, {
    
    # An error log created that will have errors that occur during the event reactives appended and be made available as a text file download.
    # This is so that the errors can be compounded and presented to the user all at once.
    errorLog <- data.frame(matrix(data=NA,ncol = 2,nrow = 20))
    names(errorLog) <- c("type","description")
    row <- 1
    
    # Searches for a file pattern of an extension ending .mmt to accurately select the .mmt file.
    mmtFileList <- input$fileUpload$datapath[grep('(?i)\\.mmt', input$fileUpload$datapath)]
    # Wrapped a try function around the parsing attempt so that if an error occurs it does not immediately crash the app, also enables the ability to capture error and notify user.
    mmtAdcpXml <- try(XML::xmlParse(mmtFileList))

    # Checks if the value returned matches a known error so that the error can then prompt a notification and inform the user.
    if(isTRUE(attr(mmtAdcpXml, "class") == "try-error")){
      errorLog$type[row] <- 'Error'
      errorLog$description[row] <- 'The .mmt file from the WinRiverII output is missing. This file is required by the app.'
      row <- row+1
    }
    
    # Searches for a file pattern of an extension ending .XML to accurately select the .XML file.
    qRevFileList <- input$fileUpload$datapath[grep('(?i)\\.xml', input$fileUpload$datapath)]
    # Wrapped a try function around the parsing attempt so that if an error occurs it does not immediately crash the app, also enables the ability to capture error and notify user.
    qRevFile <- try(XML::xmlParse(qRevFileList), silent = TRUE)
    
    # Checks if the value returned matches a known error so that the error can then prompt a notification and inform the user.
    if(isTRUE(attr(qRevFile, "class") == "try-error")){
      errorLog$type[row] <- 'Error'
      errorLog$description[row] <- 'The .xml file from the Q-Rev output is missing. This file is required by the app.'
      row <- row+1
    }
    
    # Nodes of importance to the output Ingest Table are set as objects so that they may be more easily referenced, edited and added into the output Ingest Table.
    mMTDF <- try(XML::xmlToDataFrame(XML::xpathApply(mmtAdcpXml,paste("//Site_Information"))),silent = T)
    qRevDF <- try(XML::xmlToDataFrame(XML::xpathApply(qRevFile, paste("//ChannelSummary/Other"))),silent = T)
    estimatedUC <- try(XML::xmlToDataFrame(XML::getNodeSet(qRevFile, path='//ChannelSummary/Uncertainty/Total'))[1,1],silent = T)
    totalDischarge <- try(XML::xmlToDataFrame(XML::getNodeSet(qRevFile, path='//ChannelSummary/Discharge/Total'))[1,1],silent = T)
    magVar <- try(XML::xmlToDataFrame(XML::getNodeSet(qRevFile, path='//Processing/Navigation/MagneticVariation'))[1,1],silent = T)
    compassError <- try((gsub('.*Vector Addition Of Single And Double Cycle Errors: |°.*','',
                         (XML::xmlToDataFrame(XML::getNodeSet(qRevFile, path='//CompassCalibration/Text'))))),silent = T)
    mBT <- try(XML::xmlToDataFrame(XML::getNodeSet(qRevFile, path='//QA/MovingBedTest/TestType'))[1,1],silent = T)
    widthUnits <- try(XML::xpathSApply(qRevFile, "//ChannelSummary/Other/MeanWidth", XML::xmlGetAttr, 'unitsCode'),silent = T)
    dischUnits <- try(XML::xpathSApply(qRevFile, "//ChannelSummary/Discharge/Total", XML::xmlGetAttr, 'unitsCode'),silent = T)
    velUnits <- try(XML::xpathSApply(qRevFile, "//ChannelSummary/Other/MaximumWaterSpeed", XML::xmlGetAttr, 'unitsCode'),silent = T)
    dates <- try((XML::xpathSApply(mmtAdcpXml, paste('//Discharge_Summary/None'), XML::xmlChildren)),silent = T) 
    
    # Timezone recorded in the .mmt file is checked  for format so that the user can be notified and correct error if not recorded in UCT
    if(mMTDF$TimeZone!="Coordinated Universal Time"){
      errorLog$type[row] <- 'Error'
      errorLog$description[row] <- 'The time zone recorded is not in UTC. ADCP bouts are required to be in UTC to be uploaded to the NEON database.'
      row <- row+1
    }
    
    # For every row in the dates object another data frame is created and bound to a singular dataframe
    if(!(isTRUE(attr(dates, 'class') == 'try-error'))){
      for (i in 1:nrow(dates)){ 
        df <- XML::xmlToDataFrame(dates[i,1])
        if(i==1){
          bindedDF <- df
        }else{
          bindedDF <- rbind(bindedDF,df)
        }
      }
    }
    
    # Start and end time configuration, the chunk below filters the Use in Summary column for where value is equal to1.
    # This allows for the selection of fields to assist in establishing the start and end dates.
    # Dates are converted into NEON specifications, YYYY-MM-DD.
    bindedDF <- bindedDF[bindedDF$UseInSummary==1,]
    bindedDF$StartTime_POSIXct <- format(as.POSIXct.numeric(
      as.numeric(bindedDF$StartTime),
      origin = as.POSIXct('1970-01-01', tz = ""),
      tz = ""),'%Y-%m-%dT%H:%M:%S' )
    bindedDF$EndTime_POSIXct <- format(as.POSIXct.numeric(
      as.numeric(bindedDF$EndTime),
      origin = as.POSIXct('1970-01-01', tz = ""),
      tz = ""), '%Y-%m-%dT%H:%M:%S')
    formatMin <- paste0(as.character(as.Date(min(bindedDF$StartTime_POSIXct))-1), "T00:00:00.000Z")
    formatMax <- paste0(as.character(as.Date(max(bindedDF$EndTime_POSIXct))+1), "T00:00:00.000Z")
    
    # Returns a NULL if dates section is missing. This is so the rest of the program will run and present all available errors to the user.
    if((isTRUE(attr(dates, 'class') == 'try-error'))){
      errorLog$type[row] <- 'Error'
      errorLog$description[row] <- 'Start and/or end dates are missing from each transect in the .mmt file. Start and end dates are required for each transect selected from the ADCP bout.'
      row <- row+1
    }else{
      # Checks that end date is at a later date than start date so that length of experiment makes sense and to notify user if the error is present.
      if(isTRUE(formatMax < formatMin)){
        errorLog$type[row] <- 'Error'
        errorLog$description[row] <- 'The minimum transect start date is less than the maximum transect end date.'
        row <- row+1
      }
    }
    
    # Gathering archived data, wrapped in a try, so that in the case of an error, the application does not immediately crash.
    # Errors for the attempt to gather data from the L0 database can be checked and enable the ability to prompt specific notifications for known/common errors.
    ghAdcpDat <- try(silent = TRUE, "ZN-REDACTED") # Internal function is used to query data from level L0 NEON database
    
    # Taking the value from the try above, the value of the attempt to retrieve archived ADCP data from the L0 database is checked against certain
    # known errors. Depending on the error a notification is displayed to the user, the application
    # stops and a resolution to the error may be printed onto the screen.
    if(isTRUE(grepl('Error in curl::curl_fetch_memory',ghAdcpDat))){
      errorLog$type[row] <- 'Error'
      errorLog$description[row] <- 'Cannot access the L0 database. Check VPN connection.'
      row <- row+1
    }else{
      ghAdcpDat <- ghAdcpDat[ghAdcpDat$transectID %in% bindedDF$TransectNmb]
    }
    if(isTRUE(grepl('Data GET failed with status code 404. Check the formatting of your inputs',ghAdcpDat))){
      errorLog$type[row] <- 'Error'
      errorLog$description[row] <- 'Unable to get GAG data from the L0 database. Check the formatting of the site name and transect start and end dates in the WinRiverII .xml file.'
      row <- row+1
    }else{
      ghAdcpDat <- ghAdcpDat[ghAdcpDat$transectID %in% bindedDF$TransectNmb]
    }
    if(is.null(ghAdcpDat)){
      errorLog$type[row] <- 'Error'
      errorLog$description[row] <- 'There is no GAG data in the L0 database for this bout. Bouts should be processed in this app > 14 days after the collect date to allow GAG data to be ingested into the L0 database from Fulcrum.'
      row <- row+1
    }
    
    # The chunk below obtains the domain number from the site ID present on the uploaded files to reduce the amount of required user input and simplify process for the user.
    DXX <- NA
    DXX[mMTDF$Name%in%c("HOPB")] <- "D01"
    DXX[mMTDF$Name%in%c("POSE","LEWI")] <- "D02"
    DXX[mMTDF$Name%in%c("FLNT")] <- "D03"
    DXX[mMTDF$Name%in%c("CUPE","GUIL")] <- "D04"
    DXX[mMTDF$Name%in%c("KING","MCDI")] <- "D06"
    DXX[mMTDF$Name%in%c("WALK","LECO")] <- "D07"
    DXX[mMTDF$Name%in%c("MAYF","BLWA","TOMB")] <- "D08"
    DXX[mMTDF$Name%in%c("ARIK")] <- "D10"
    DXX[mMTDF$Name%in%c("BLUE","PRIN")] <- "D11"
    DXX[mMTDF$Name%in%c("BLDE")] <- "D12"
    DXX[mMTDF$Name%in%c("COMO","WLOU")] <- "D13"
    DXX[mMTDF$Name%in%c("SYCA")] <- "D14"
    DXX[mMTDF$Name%in%c("REDB")] <- "D15"
    DXX[mMTDF$Name%in%c("MART","MCRA")] <- "D16"
    DXX[mMTDF$Name%in%c("BIGC","TECR")] <- "D17"
    DXX[mMTDF$Name%in%c("TOOK","OKSR")] <- "D18"
    DXX[mMTDF$Name%in%c("CARI")] <- "D19"
    
    
    # Pulling Ingest Table available in L0 database so that certain row entries may become columns of the output Ingest Table.
    # The attempt to pull the Ingest Table is wrapped in a try function so that in case of an error the application does not immediately crash and can return an error value.
    pullIngest <- try(silent = T, "ZN-REDACTED") # Internal function is used to query data from level L0 NEON database
    
    # The value of the attempt to pull the Ingest Table above is checked so that notifications may be presented to the user if errors are common/known.
    if(isTRUE(attr(pullIngest, 'class') == 'try-error')){
      errorLog$type[row] <- 'Error'
      errorLog$description[row] <- 'Cannot access the L0 database. Check VPN connection.'
      row <- row+1
    }
    if(is.null(pullIngest)){
      errorLog$type[row] <- 'Error'
      errorLog$description[row] <- 'Unable to retrieve ingest workbook data from the L0 database. If this error persists, contact the author of the shiny app.'
      row <- row+1
    }
    
    # Creating a dataframe allows for row values of the Ingest Table obtained from the L0 server to become column names of the output Ingest Table. 
    ingestTable <- as.data.frame(matrix(data = NA, ncol = nrow(pullIngest),nrow = 1))
    names(ingestTable) <- pullIngest$fieldName
    
    # Ingest tables fields that come from the MMT file
    if(!any(attr(mmtAdcpXml, "class") == "try-error")){
      if(mMTDF$Name=="TKIN"){
        ingestTable$stationID <- "TOOK.AOS.discharge.inflow"
      }else{
        if(mMTDF$Name=="TKOT"){
          ingestTable$stationID <- "TOOK.AOS.discharge.outflow"
        }else{
          ingestTable$stationID <- paste0(mMTDF$Name,".AOS.discharge")
        }
      }
      ingestTable$startDate <-  min(bindedDF$StartTime_POSIXct)
      ingestTable$endDate <- max(bindedDF$EndTime_POSIXct)   
      ingestTable$eventID <- paste(mMTDF$Name,format(as.Date(ingestTable$endDate),"%Y%m%d"),"adcp",sep = ".")
      ingestTable$waterTemperature <- mMTDF$Water_Temperature
      ingestTable$windSpeedPrior <- if(!is.na(mMTDF$Wind_Speed)){(gsub("km/hr","",mMTDF$Wind_Speed))}     
      ingestTable$windDirRelativeToFlow <- mMTDF$Wind_Direction 
      # Get unused version letter for GCS data package
      currentFileVersion <- max(gsub(".zip","",gsub("inFlowAppTest_[0-9]{8}_NEON_D[0-9]{2}_[A-Z]{4}_[0-9]{8}_ADCP_DISCHARGE_L0_V","",currGCSFiles$name[grepl(paste0("NEON_", DXX, "_", mMTDF$Name,"_", format(as.Date(ingestTable$endDate),"%Y%m%d"), "_ADCP_DISCHARGE_L0"),currGCSFiles$name)])))
      if(length(currentFileVersion)==0|all(is.na(currentFileVersion))){VX <- "A"}else{VX <- LETTERS[which(LETTERS==currentFileVersion)+1]}
      ingestTable$rawDataFileName <- paste0("inFlowAppTest_",format(Sys.Date(),"%Y%m%d"),"_NEON_",DXX,"_",mMTDF$Name,"_",format(as.Date(ingestTable$endDate),"%Y%m%d"),"_ADCP_DISCHARGE_L0_V",VX,".zip")
    }    

    # Ingest table fields that come from the Q-Rev file
    if(!any(attr(qRevDF, "class") == "try-error")){
      ingestTable$totalDischarge <- as.numeric(totalDischarge)*1000 # Convert from cms to lps  
      ingestTable$estimated95percentUC <- as.numeric(estimatedUC)*1000 # Convert from cms to lps   
      ingestTable$riverVelocityMaximum <- qRevDF$MaximumWaterSpeed  
      ingestTable$riverWidthMean <- qRevDF$MeanWidth
      ingestTable$maxDepth <- qRevDF$MaximumDepth                 
      ingestTable$riverDepthMean <- qRevDF$MeanDepth   
      ingestTable$riverDischargeMeasDuration <- qRevDF$Duration
      ingestTable$magneticVariation <- ifelse(is.null(magVar), NA, magVar) 
      ingestTable$adcpCompassError <- ifelse(is.null(compassError), NA, compassError)     
      ingestTable$adcpCompassCalibrated <- ifelse(is.null(compassError),"N","Y")
      ingestTable$stationaryMBT <- ifelse((mBT == 'Stationary'), 'Y', 'N')                    
      ingestTable$loopMBT <- ifelse((mBT == 'Loop'), 'Y', 'N')   
    }
    
    # Ingest table fields that come from the GAG data
    if(!(isTRUE(grepl('Error in curl::curl_fetch_memory',ghAdcpDat))
        |isTRUE(grepl('Data GET failed with status code 404. Check the formatting of your inputs',ghAdcpDat))
        |is.null(ghAdcpDat))){
      ingestTable$aCollectedBy <- unique(ghAdcpDat$collectedBy)[1]
      ingestTable$bCollectedBy <- unique(ghAdcpDat$recordedBy)[1]            
      ingestTable$streamStage <- mean(as.numeric(ghAdcpDat$initialStageHeight, trim = 0, na.rm = TRUE))             
    }
    
    # The line below allows for obtaining Sampling Protocol used within Ingest Table recently referenced, instead of putting the onus on the user
    # by having another select input within the left pane of the UI. The Sampling protocol is also embedded into the output Ingest Table.
    protocolLOV <- try(silent = T, "ZN-REDACTED") # Internal function is used to query data from level L0 NEON database
    
    # Taking the value returned from the try above, the value of the sampling protocol version is checked against certain
    # known errors. Depending on the error a notification is displayed to the user, the application
    # stops and a resolution to the error may be printed onto the screen.
    if(isTRUE(protocolLOV == 'Error in curl::curl_fetch_memory(url, handle = handle) : \n  Could not resolve host: prod-os-ds-1.ci.neoninternal.org\n')){
      errorLog$type[row] <- 'Error'
      errorLog$description[row] <- 'Cannot access the L0 database. Check VPN connection.'
      row <- row+1
    }else{
      protocolLOV$rank <- as.numeric(protocolLOV$rank)
      ingestTable$samplingProtocol <- protocolLOV$lovElementCode[max(protocolLOV$rank)]      
    }
    
    # Filtering out columns that are not needed within the ingest table, these columns were empty, no entries existed within the created ingest table.
    columnsToRemove <- c('uid','streamStageUnits','totalDischargeUnits','totalDischargeRU','velocityUnits','widthUnits','rawDataFilePath','timeZone','curveIDToInclude','curveIDToIncludeNotes','dataQF')
    ingestTable <- ingestTable[,!(names(ingestTable)%in%columnsToRemove)]

    # The created ingest table is checked for any missing values and if so, a notification is presented.
    if(any(is.na(ingestTable))) {
      errorLog$type[row] <- 'Warning'
      errorLog$description[row] <- 'There are missing values within the Ingest Table. Check for missing values and fill in where possible.'
      row <- row+1
    }
        
    Lov <- try(silent = TRUE, "ZN-REDACTED") # Internal function is used to query data from level L0 NEON database
    # Taking the value from the try above, the value of the attempt to load the wind direction entries within L0 database is checked against
    # certain known errors.   
    # Allows for wind direction to be compared and determined if wind direction entries within uploaded files are valid and to notify a user if error occurs.
    if(isTRUE(attr(Lov, 'class') == 'try-error')){ 
      errorLog$type[row] <- 'Error'
      errorLog$description[row] <- 'Cannot access the L0 database. Check VPN connection.'
      row <- row+1
    }
    if(is.null(Lov)){
      errorLog$type[row] <- 'Error'
      errorLog$description[row] <- 'Unable to retrieve standardized list of values from the L0 database. If this error persists, contact the author of the shiny app.'
      row <- row+1
    }

    # Checks if entries for wind direction are missing from Ingest Table so that a user can be notified that the entry within the output Ingest Table 
    # is automatically set to NA.
    if(isTRUE(ingestTable$windDirRelativeToFlow == "")){
      ingestTable$windDirRelativeToFlow <- 'NA'
      errorLog$type[row] <- 'Warning'
      errorLog$description[row] <- 'Wind Direction absent from uploaded files and will be set to NA. Please include wind direction in future bouts.'
      row <- row+1
    }
    
    # Entries for Wind direction are compared to existing entries within the L0 database so that a user may be notified and the program terminate
    # if entries are deemed invalid/not matching existing entries within the L0 database.
    if(isTRUE(!(ingestTable$windDirRelativeToFlow %in% Lov$lovElementCode))){
      errorLog$type[row] <- 'Error'
      errorLog$description[row] <- 'Wind direction entry does not fit the standard list of values. Accepted values are: Upstream, Downstream, Cross-wind, Mixed.'
      row <- row+1
    }
    
    # The entry for total discharge is checked if it is equal or below 0 to alert user that if that is the case and to allow a user to rectify if
    # entries are false.
    if(isTRUE(ingestTable$totalDischarge <= 0)){
      errorLog$type[row] <- 'Warning'
      errorLog$description[row] <- 'Reported Discharge is equal to or less than 0.'
      row <- row+1
    }
    
    # The total number of transects performed are checked and if less than 4, terminates the program and presents error to establish a requirement of
    # 4 transects.
    if(isTRUE(as.numeric(qRevDF$NumberofTransects) < 4)){
      errorLog$type[row] <- 'Error'
      errorLog$description[row] <- 'There are less than 4 selected transects in this bout. A bout must be post-processed with at least 4 selected transects.'
      row <- row+1
    }

    # Checks values for estimated uncertainty against 20% of total discharge and checks if uncertainty is greater, to alert user in case of error and 
    # prompt the user to acknowledge/review entries.
    if(isTRUE(as.numeric(ingestTable$estimated95percentUC) > as.numeric(ingestTable$totalDischarge)*.20)){
      errorLog$type[row] <- 'Warning'
      errorLog$description[row] <- 'Estimated 95% Uncertainty calculated in Q-Rev is >20% of the final discharge value. Consider reviewing and/or reprocessing the data.'
      row <- row+1
    }
    
    # Create the zip archive and assigns the name of the folder.
    # Users will need to be able to upload the compressed archive into GCS
    confDir <- paste(gsub(pattern =  "\\", replacement = '/', x = input$fileUpload$datapath[grep('(?i)\\.mmt', input$fileUpload$datapath)],
                          fixed = TRUE))
    confDir1 <- paste0(gsub(pattern = '\\/[^//////]*$', '', x = confDir),"/")
    fileNameComp <- input$fileUpload
    fileNameComp$tempFileName <- dir(confDir1)
    dir.create(paste0(confDir1,gsub(".zip","",ingestTable$rawDataFileName),"/"))
    file.copy(from = paste0(confDir1,fileNameComp$tempFileName),
              to = paste0(confDir1,gsub(".zip","",ingestTable$rawDataFileName),"/",fileNameComp$name))
    file.rename(from = paste0(confDir1,gsub(".zip","",ingestTable$rawDataFileName),"/",fileNameComp$name[grepl(".mmt",fileNameComp$name)]),
                to = paste0(confDir1,gsub(".zip","",ingestTable$rawDataFileName),"/",gsub("zip","mmt",ingestTable$rawDataFileName)))
    zipOutput <- zip::zip(ingestTable$rawDataFileName,
                          files = dir(paste0(confDir1,gsub(".zip","",ingestTable$rawDataFileName),"/")),
                          root = paste0(confDir1,gsub(".zip","",ingestTable$rawDataFileName),"/"))
    
    # Checks if entries are entered into the error log and will hide the output Ingest Table and plot then, display a popup referring to an error.
    # A .txt file download will be available within the pop-up so the user can review and make all changes at once.
    errorLog <- errorLog[!is.na(errorLog$type),]
    if(sum(errorLog$type=="Error")>0){
      write.table(errorLog)
      shiny::showModal(shiny::modalDialog(
        title = 'Critical Errors: Cannot produce ingest table. Review app log below or download log for review.',
        paste0("*** ",paste0(errorLog$description,collapse = " *** ")," ***"),
        footer = htmltools::tagList(
           shiny::downloadHandler(
            filename = function() {
               paste('appLog_',gsub('.zip', '' ,ingestTable$rawDataFileName), '.txt', sep = '')
            },
            content = function(file) {
              utils::write.table(gsub('X', '', errorLog), file, sep = '\n')
            }
          ),
          modalButton('Close')
        ), easyClose = FALSE
      ))
      stop('Critical errors present within the uploaded files. Please review the error log. ')
    }else{
      if(sum(errorLog$type=="Error")==0&sum(errorLog$type!="Error")>0){
        write.table(errorLog)
        shiny::showModal(modalDialog(
          title = 'Data failed certain QAQC checks, but can still produce ingest table. Review app log below or download log for review.',
          paste0("*** ",paste0(errorLog$description,collapse = " *** ")," ***"),
          footer = htmltools::tagList(
            shiny::downloadHandler(
              filename = function() {
                paste('appLog_',gsub('.zip', '' ,ingestTable$rawDataFileName), '.txt', sep = '')
              },
              content = function(file) {
                utils::write.table(gsub('X', '', errorLog), file, sep = '\n')
              }
            ),
            modalButton('Close')
          ), easyClose = FALSE
        ))
        return(ingestTable)
      }else{
        return(ingestTable)
      }
    }
  })
  
  # The chunk below allows the Ingest Table to hold a scroll feature and edits the appearance. 
  output$ingestTable <- DT::renderDataTable({
    if(is.null(input$fileUpload)){return()}
    ingestTable()},
    options=list(scrollX=T,
                 sDom  = '<"top">lrt<"bottom">ip',
                 pageLength = 1, 
                 info = FALSE, 
                 lengthChange = FALSE, 
                 searching = FALSE, 
                 paging=FALSE),
    rownames= FALSE)
  
  # The lines below renders the plot and will not execute until a file has been uploaded.
  output$graph <- plotly::renderPlotly({
    if(is.null(input$fileUpload)){return()}
    
    # Pull rating curve plotting data from public Github repository within a try function.
    # If an error occurs when accessing a link, rcPlotData is returned as that error.
    rcPlotData <- try(readRDS(url("https://raw.githubusercontent.com/NEONScience/NEON-stream-discharge/main/shiny-openFlow/rcPlottingData.rds")),silent = T)

    # The chunk below allows for the Rating Curve plot data and gauging to be sorted from least to greatest and filtered for the most recent rating curve ID.
    # To accurately display uploaded data against the most recent and relevant data. A TOMB site ID is checked and will plot data against the ghAdcpDat
    # archived stream stage and total discharge data, not filtered by the curve ID, since NEON does not possess archived TOMB data.
    if(!is.null(rcPlotData)|isFALSE(grepl('cannot open the connection to', rcPlotData))){
      if(!(grepl('TOMB' ,ingestTable()$stationID))){
        rcGaugeCurveID <- sort(unique(rcPlotData$rcData$curveID)[grepl(gsub("\\..*$","",ingestTable()$eventID),unique(rcPlotData$rcData$curveID))],decreasing = T)[1]
        rcGaugings <- subset(rcPlotData$rcGaugings, curveID == rcGaugeCurveID)
        rcData <- subset(rcPlotData$rcData, curveID == rcGaugeCurveID)
      }else{
        tombData <- try(silent = TRUE, restR2::get.os.l0.data(
          dpID = 'DP0.20048.001',
          ingestTable = 'dsc_fieldDataADCP_in',
          namedLocationName = 'TOMB',
          minStartDate = NA,
          maxStartDate = NA,
          inclDescendants = T,
          format_for_L0_editor = TRUE))
        rcGaugings <- as.data.frame(matrix(nrow = nrow(tombData)))
        rcGaugings['Q'] <- as.numeric(tombData$totalDischarge) *1000
        rcGaugings['H'] <- tombData$streamStage
        rcData <- rcPlotData$rcData[0,]
      }

      # The chunk below constructs the appearance of the graph.
      # Specifications of the graph ensures that the presentation of the rendered plot is easy to read and axis are properly labeled. 
      rcPlot <- try(plotly::plot_ly(data = rcData))%>%
        plotly::layout(
          xaxis = base::list(tick = 14,
                             automargin = T,
                             title = stringr:: str_c("Stage (m)" ),
                             tickfont = base::list(size = 16),
                             titlefont = base::list(size = 18)),
          yaxis = base::list(automargin = T,
                             title = stringr:: str_c("Dischage (lps)"),
                             tickfont = base::list(size = 16),
                             titlefont = base::list(size = 18),
                             showgrid = T,
                             zeroline = T),
          legend = base::list(font = base::list(size = 12),
                              orientation = "h",
                              xanchor = "center",  
                              x = 0.5, y = 7),
          updatemenus = base::list(
            base::list(
              x=-0.05,y=1.1,
              type = 'buttons',
              showactive = FALSE,
              buttons = base::list(
                base::list(label = 'Scale Discharge\n- Linear -',
                           method = 'relayout',
                           args = base::list(base::list(yaxis = base::list(type = 'Linear',
                                                                           title = "Discharge (L/s)",
                                                                           tickfont = base::list(size = 16),
                                                                           titlefont = base::list(size = 18),
                                                                           showgrid = T,
                                                                           zeroline = T)))),
                base::list(label = 'Scale Discharge\n- Log -',
                           method = 'relayout',
                           args = base::list(base::list(yaxis = base::list(type = 'log',
                                                                           title = "Discharge (L/s) - log",
                                                                           tickfont = base::list(size = 16),
                                                                           titlefont = base::list(size = 18),
                                                                           showgrid = T,
                                                                           zeroline = T))))))))%>%
        
        # Remnant Uncertainty, adds the following trace to the presented plot.
        plotly::add_trace(data = rcData ,x=~try(Hgrid, silent = TRUE),y=~try(totalUTop, silent = TRUE),name=paste0("Rating Curve Uncertainty: ",unique(rcGaugings$curveID)[1]),type='scatter',mode='line',line=base::list(color='#D55E00'),hovertemplate = "Stage: %{x} <br> Discharge: %{y}",showlegend=F,legendgroup="RC Uncertainty")%>%
        plotly::add_trace(data = rcData, x=~try(Hgrid, silent = TRUE),y=~try(totalUBottom, silent = TRUE),name=paste0("Rating Curve Uncertainty: ",unique(rcGaugings$curveID)[1]),type='scatter',mode='line',fill='tonexty',fillcolor='#D55E00',line=base::list(color='#D55E00'),hovertemplate = "Stage(m): %{x} <br> Discharge(lps): %{y}",showlegend=T,legendgroup="RC Uncertainty")%>%
       
         # Parametric Uncertainty, adds the following trace to the presented plot.
        plotly::add_trace(data = rcData, x=~try(Hgrid, silent = TRUE),y=~try(pramUTop, silent = TRUE),name=paste0("Rating Curve Uncertainty: ",unique(rcGaugings$curveID)[1]),type='scatter',mode='line',line=base::list(color='#E69F00'),hovertemplate = "Stage: %{x} <br> Discharge: %{y}",showlegend=F,legendgroup="RC Uncertainty")%>%
        plotly::add_trace(data = rcData, x=~try(Hgrid, silent = TRUE),y=~try(pramUBottom, silent = TRUE),name=paste0("Rating Curve Uncertainty: ",unique(rcGaugings$curveID)[1]),type='scatter',mode='line',fill='tonexty',fillcolor='#E69F00',line=base::list(color='#E69F00'),hovertemplate = "Stage: %{x} <br> Discharge: %{y}",showlegend=F,legendgroup="RC Uncertainty")%>%
        
        # Max Post Q, adds the following trace to the presented plot.
        plotly::add_trace(data = rcData, x=~try(Hgrid, silent = TRUE),y=~try(maxPostQ, silent = TRUE),name=paste0("Rating Curve: ",unique(rcGaugings$curveID)[1]),type='scatter',mode='line',line=base::list(color= 'black'),hovertemplate = "Stage: %{x} <br> Discharge: %{y}",showlegend=T,legendgroup="RC MaxPost Q")%>%
        
        # Empirical H/Q Pairs, adds the following trace to the presented plot.
        plotly::add_trace(data = rcGaugings,x=~H,y=~Q,name=paste0("Empirical Gaugings: ",unique(rcGaugings$curveID)[1]),type='scatter',mode='markers',marker=base::list(color= 'black'),hovertemplate = "Stage: %{x} <br> Discharge: %{y}",showlegend=T,legendgroup="RC Gaugings")%>%
        
        # Add data from ADCP bout +/- Uncertainty, adds the following trace to the presented plot.
        plotly::add_trace(data = ingestTable(),x=~as.numeric(streamStage),y=~as.numeric(totalDischarge),name=paste('New Measurement:', gsub('.adcp','', ingestTable()$eventID)),error_y=~list(array=as.numeric(estimated95percentUC),color="black"),type='scatter',mode='markers',marker = base::list(color = '#009E73',symbol = 22, size=12,line = base::list(color = "black",width = 1)),hovertemplate = "Stage: %{x} <br> Discharge: %{y}",showlegend=T, legendgroup=~eventID)
    }else{
      rcPlot <- plotly::plotly_empty()%>%
        plotly::layout(
          title=base::list(
            text = "No Stage-discharge rating curves (DP4.00133.001) data available.\nContact app author.",
            yref = "paper",
            y = 0.5,
            font=base::list(size=28)))
    }
  })
    
  
  # The lines below hide the download button until a file is uploaded to prevent users from downloading blank html files and ensure the user goes through the process of uploading files.
  shiny::observe({
    if(!is.null(input$fileUpload)){
      shinyjs::show("download")}
    else{
      shinyjs::hide("download")
    }
  })
  
  # The lines below will show the radio button after clicking the download button so that the user does not begin the acknowledgement process before uploading files.
  shiny::observeEvent(input$download, {
    if(!is.null(input$download)){
      shinyjs::show("dataReview")
      shinyjs::hide("download")}
    else{
      if(is.null(input$download)){
        shinyjs::hide("dataReview")
        shinyjs::hide("zipAck")
        shinyjs::hide('fileUploader')}
    }
  })
  
  # The lines below will prompt a radio button, serving as the 2nd acknowledgement check, so that the user has to go through the acknowledgement checks in order 
  # and when selecting no will prompt an error, notification, hide the succeeding content that is dependent on previous radio buttons and require the user to again answer the acknowledgement.  
  shiny::observeEvent(input$dataReview, {
    if(input$dataReview == "Yes"){
      shinyjs::show("zipAck")
    }else{
      if(input$dataReview == "No"){
        shinyjs::hide("zipAck")
        shinyjs::hide('gcsLink')
        shinyjs::hide('fileDownload')
        shinyjs::hide('fileUploader')
        shinyalert::shinyalert(title = "Data not Reviewed",
                               "QAQC the data before downloading the ingest table.",
                               type = "error")
        shiny::updateRadioButtons(session, "zipAck",
                                  choices = c('Yes', 'No'),
                                  selected = character(0))
      }
    }
  })
  
  # The lines below will show a button that links to GCS conditional on if the user has answered yes to the previous acknowledgement checks to ensure
  # that a user has properly answered yes to checking, reviewing data and also stating that they are ready to upload to GCS. 
  shiny::observeEvent(input$zipAck, {
    if(input$zipAck == 'Yes'){
      shinyjs::show("gcsLink")
    }else{
      if(input$zipAck == 'No'){
        shinyalert::shinyalert(title = "Error",
                               "Make sure all files from the bout are uploaded to the app and data has been QAQCed before pushing the data package to GCS",
                               type = "error")
        shinyjs::hide("gcsLink")
        shinyjs::hide("fileDownload")
        shinyjs::hide('fileUploader')
      }
    }
  })
  
  # Once the button above a download button containing the Ingest Table and a link to GCS appears so that a user may download the .csv Ingest Table file
  # and in the case of closing GCS, may reopen a new tab to the site instead of having to go through the acknowledgement process again.
  shiny::observeEvent(input$gcsLink, {
    if(input$gcsLink){
      if(file.exists(paste0(paste0(gsub(pattern = '\\/[^//////]*$', '', x = paste(gsub(pattern =  "\\", replacement = '/', x = input$fileUpload$datapath[grep('(?i)\\.mmt', input$fileUpload$datapath)],fixed = TRUE))),"/"),gsub(".zip","",ingestTable()$rawDataFileName),"/",ingestTable()$rawDataFileName))){
        googleCloudStorageR::gcs_upload(file = paste0(paste0(gsub(pattern = '\\/[^//////]*$', '', x = paste(gsub(pattern =  "\\", replacement = '/', x = input$fileUpload$datapath[grep('(?i)\\.mmt', input$fileUpload$datapath)],fixed = TRUE))),"/"),gsub(".zip","",ingestTable()$rawDataFileName),"/",ingestTable()$rawDataFileName),
                                        bucket = googleCloudStorageR::gcs_get_global_bucket(),
                                        name = ingestTable()$rawDataFileName,
                                        predefinedAcl = "bucketLevel")
        shinyalert::shinyalert(title = 'Success',
                               'Your L0 ADCP data package has been successfully zipped and uploaded to GCS.\n
                               Download results of the ADCP bout as a CSV and proceed to upload the file to SOM via the link to the spreadsheet uploader.',
                               type = 'success')
        shinyjs::hide("dataReview")
        shinyjs::hide("zipAck")
        shinyjs::hide("gcsLink")
        shinyjs::show('fileDownload')
        shinyjs::show('fileUploader')
      }else{
        shinyalert::shinyalert(title = 'Error',
                               'Your L0 ADCP data package cannot be uploaded to GCS.\n
                               Attempt upload again. If the issue persists, contact app author.',
                               type = 'error')
        stop('Your L0 ADCP data package cannot be uploaded to GCS. Attempt upload again. If the issue persists, contact app author.')
      }
    }
  })
  
  # Section below allows the setting download content within the download button referenced to be .csv file of the output Ingest Table.
  output$fileDownload <- shiny::downloadHandler(
    filename = function() {
      paste(gsub('.zip', '.csv', ingestTable()$rawDataFileName))
    },
    content = function(file) {
      utils::write.csv(ingestTable(), file)
    }
  )
}