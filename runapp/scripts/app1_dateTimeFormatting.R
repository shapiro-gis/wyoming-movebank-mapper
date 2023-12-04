showDateTimeSelectionPanel<-function(){
  hide(id='uidSeletorRow',anim=TRUE)
  if(thisIsTestingRun){
    hasObjectsHandler()
  }
  showElement(id = 'dateTimeRow', anim = TRUE)


  ##------------------ add to date columns whenever one is selected
    observeEvent(input$dateColumnSelector, {
      dateColumns <<- input$dateColumnSelector
    })

    observe({
      req(input$dateColumnSelector)
   # observeEvent(input$doneChoosingDateColumnsButton,{
      processDates();
      })

  ##------------------ show the first 20 rows of data
  rowsToShow<-importedDatasetMaster@data[1:20,]
  
  if('timestamp'%in%names(rowsToShow)){
   # rowsToShow$timestamp<-as.character(rowsToShow$timestamp)
    rowsToShow$timestamp <- format(rowsToShow$timestamp, "%Y-%m-%d %H:%M:%S")
    
  }
  if('timestamp'%in%names(importedDatasetMaster@data)){
    # rowsToShow$timestamp<-as.character(rowsToShow$timestamp)
    importedDatasetMaster@data$timestamp <- format(importedDatasetMaster@data$timestamp, "%Y-%m-%d %H:%M:%S")

  }
  

  
  output$dateConfigTable1 <- renderTable(rowsToShow)

  uniqueColumns<-names(importedDatasetMaster@data)
  selectedDateColumns<-NULL
  
  if ("timestamp" %in% names(importedDatasetMaster@data)) {
    updateCheckboxGroupInput(session, "dateColumnSelector", choices=uniqueColumns, selected = "timestamp",inline=TRUE)
  } else{
    
  updateCheckboxGroupInput(session,
    "dateColumnSelector",
      choices=uniqueColumns,
      selected=selectedDateColumns,
      inline=TRUE
    )}
}

processDates<-function(){

  if(!exists('dateColumns')){
    modalMessager('ERROR','You need to select the column(s) which contain your dates')
    dtvRunning<<-FALSE;
    show("glideDiv")
    w$hide()
    
    loadingScreenToggle('hide', 'processing dates')
    return()
  }

  if(length(dateColumns)==0){
    modalMessager('ERROR','You need to select the column(s) which contain your dates')
    dtvRunning<<-FALSE;
    show("glideDiv")
    w$hide()
    
    loadingScreenToggle('hide', 'processing dates')
    return()
  }

  dateColumns <<- input$dateColumnSelector

  if(is.null(dateColumns)){
    modalMessager('ERROR','You need to select the column(s) which contain your dates')
    dtvRunning<<-FALSE;
    show("glideDiv")
    w$hide()
    
    loadingScreenToggle('hide', 'processing dates')
    return()
  }



  hideElement(id = 'dateTimeRow', anim = TRUE)
  showElement(id = 'dateTimeElementsRow', anim = TRUE)




  ##------------------ show the first 20 rows of data
  rowsToShow<-importedDatasetMaster@data[1:20,]
  if('timestamp'%in%names(rowsToShow)){
    #rowsToShow$timestamp<-as.character(rowsToShow$timestamp)
    rowsToShow$timestamp <- format(rowsToShow$timestamp, "%Y-%m-%d %H:%M:%S")
    
  }
  output$dateConfigTable2 <- renderTable(rowsToShow)
  ###-- create multiselectors for each of the selected date/time columns
  lapply(1:length(dateColumns), function(i) {
    tempVar<-paste("dateConfigUi",i,sep="")
    tempSelectorName<-paste(dateColumns[i],"Indicator",sep="")


    #if the columnFieldList exists in the progress tracker, we have to fill
    #in the appropriate selections in the selectors - furthermore, they have to be
    # in a specific order or R will rearrange elements
    if('columnFieldList'%in%names(configOptions)){
      if(dateColumns[i]%in%names(configOptions$columnFieldList)){
        # these are the date time elements that have already been selected for
        # this field dateColumns[i]
        selectedColumns<-configOptions$columnFieldList[[dateColumns[i]]]
        #these are the selected key and value from the datetime list
        selectedKeys<-dateTimesList[configOptions$columnFieldList[[dateColumns[i]]]]
        #next we need to grab those list() items from the date times flipped
        #to reconstruct the select menu in the correct order
        newSelectedColumns<-c()
        for(j in 1:length(selectedKeys)){
          newSelectedColumns<-c(newSelectedColumns,selectedKeys[[j]])
        }
        #these are the elements that need to be added to select menu first in
        #appropriate order
        includedDateTimeElements<-dateTimesListFlipped[newSelectedColumns]
        excludedDateTimeElements<-which(!names(dateTimesListFlipped) %in% newSelectedColumns)
        excludedDateTimeElements<-dateTimesListFlipped[excludedDateTimeElements]
        newOrderedChoices<-c(includedDateTimeElements,excludedDateTimeElements)

        # updateSelectInput(session, tempSelectorName, label = NULL, selected = selectedColumns,newOrderedChoices)

        output[[tempVar]] <- renderUI({
          selectInput(
            tempSelectorName,
            paste("Choose date time elements for: ",
            dateColumns[i],sep=""),
            selected=selectedColumns,
            newOrderedChoices,
            multiple=TRUE
            )
        })

        observeEvent(input[[tempSelectorName]], {
          collectDateTimeElements(input[[tempSelectorName]],dateColumns[i])
        },ignoreInit=TRUE)
    }
  } else{
    selectedColumns<-NULL

    output[[tempVar]] <- renderUI({
      selectInput(
        tempSelectorName,
        paste("Choose date time elements for: ",
        dateColumns[i],sep=""),
        selected=selectedColumns,
        dateTimesListFlipped,
        multiple=TRUE
        )
    })

    observeEvent(input[[tempSelectorName]], {
      collectDateTimeElements(input[[tempSelectorName]],dateColumns[i])
    },ignoreInit=TRUE)
  }



    observeEvent(input$testButton, {
      d<-c("%Y" ,"%m" ,"%d" ,"%H" ,"%M" ,"%S")
      updateSelectInput(session, tempSelectorName, label = NULL, choices = NULL, selected = d)
    },ignoreInit=TRUE)
  })
}


## this fires every times there is a change to the date/time selector
  ## the values are then stored in an object where the col name is the key
  ## and the value is the order of date/time elements in that value
  columnFieldList<<-list()
  collectDateTimeElements<-function(selectorValues,column){
    columnFieldList[[column]]<<-selectorValues
  }




  dateTimeValidator<-function(){
    dtvRunning<<-TRUE
    shinyjs::disable("yearStartDateSelector")
    #recreate the validatorObject anytime process dates is clicked
    #in case back actions and changes
    if(length(columnFieldList)==0){
      modalMessager('ERROR','You have not selected value for your date time fields. Please try again.')
      dtvRunning<<-FALSE
      show("glideDiv")
      w$hide()
      
      loadingScreenToggle('hide', 'processing dates')
      return()
    }
    validatorObject<<-list()
    for(i in 1:length(columnFieldList)){
      # these are the names of all fields in columnFieldList
      tempNames<-names(columnFieldList)
      tempColumn<-tempNames[i]
      #these are all the dt elements in that column
      tempColumnItems<-columnFieldList[[i]]
      #since there could be multiple DT elements in one column
      #we'll iterate through even if there is only one
      for(j in 1:length(tempColumnItems)){
        #this is the particular dt value in that column
        tempValue<-tempColumnItems[j]
        print(tempValue)
        ## j ends up being the position in that column
        updateKeyObj(tempValue,j,tempColumn)
      }
      ##if its the last iteration then fire the next fxn
      if(i==length(columnFieldList)){
        hasObjectsHandler()
      }
    }
  }

  updateKeyObj<-function(tempValue,j,column){
    #tempvariable for literal dt value
    tempDTelementType<-dateTimeLookup[[tempValue]]
    #check if this has already been recorded in the validator object
    # print(!is.null(validatorObject[[tempDTelementType]]))
    # send this object to the
    # if there is ampm data.. we'll store this in a seperate validatorObject and return
    if(tempValue=='%p'){
      ampmtime<<-TRUE
      validatorObjectAmPm<<-list()
      validatorObjectAmPm[[tempDTelementType]]<<-c(tempDTelementType,j,tempValue,column)
      return()
    }
    validatorObject[[tempDTelementType]]<<-c(tempDTelementType,j,tempValue,column)
  }


  hasObjectsHandler<-function(){
    startTime<<-Sys.time()
    #progressIndicator('Processing Dates.. Please wait','start')
    w <- Waiter$new(
      html = tagList(
        spin_3(),
        h4("Processing dates", style = "color: grey") # Add style attribute to h4 element
      ),
      color = transparent(.5)
    )
    
    w$show()
    #loadingScreenToggle('show','processing dates')
    importedDatasetMaster@data$newMasterDate<<--999

    # if there happens to be am/pm time.. lets quickly create a vector to hold these
    # we'll paste it back to the time later
    if(ampmtime){
      # this is the column where ampm lives
      tempDtCol<-validatorObjectAmPm[[1]][4]
      # temp vector of this column
      ampm<<-importedDatasetMaster@data[,tempDtCol]
      # remove everything in this column thats not a,m,p,A,M,P
      ampm<<-gsub("[^amp|AMP]","",ampm)
    }



    #  since the loop below runs from all possible date time elements
    # errors are thrown when not all seconds, minutes etc have been selected
    # need to run this looop based in validatorObject

    selectedDateTimeElements<-names(validatorObject)

    validatorsLength<-length(selectedDateTimeElements)

    combined_format <- paste(columnFieldList, collapse = " ")

    importedDatasetMaster@data$timestamp <- format(importedDatasetMaster@data$timestamp, combined_format)

    for(j in 1:validatorsLength){
      # what is the type
      tempDtType<-selectedDateTimeElements[j]

      # what position is it in this specific column?
      tempIndexOfLoc<-as.numeric(validatorObject[[tempDtType]][2])

      tempDtCol<-validatorObject[[tempDtType]][4]

      # -------------
      # NEW 2019 - DEALING WITH TWO DIGIT %y
      # -------------
      if(tempDtType=='year'){
        if(validatorObject$year[3]=='%y'){
          importedDatasetMaster@data[,tempDtCol]<<-paste0('20',importedDatasetMaster@data[,tempDtCol])
        }
      }
      # -------------
      # -------------

      # create a temp vector for this DT element named as such
      assign(selectedDateTimeElements[j],importedDatasetMaster@data[,tempDtCol])

      #tempDateDataObjSplt <- unlist(strsplit(as.character(get(selectedDateTimeElements[j])), ",[^0-9]*"))
      tempDateDataObjSplt<-gsub("[^0-9.]",',',get(selectedDateTimeElements[j]))
      
      # split on the commas
      tempDateDataObjSplt <- strsplit(tempDateDataObjSplt, ",")
      
      # remove the empties
      tempDateDataObjSplt<-lapply(tempDateDataObjSplt,function(x){
        x[!x ==""]
      })
    



      # this generates a vector of T/F checking for invalid data time positions
      # in tempDateDateObjSplt... this helps weed out strange date time elements
      # for example a merged dataset where some time elements have H,M,S and
      # other only have H,M
      naList<-do.call(rbind, lapply(tempDateDataObjSplt, function(x) is.na(x[tempIndexOfLoc])))
      
      if (any(naList)) {
        firstErrorRow <- which(naList)[1]
        firstErrorRowData <- importedDatasetMaster@data[firstErrorRow, tempDtCol]
        errorMsg <- paste0(
          'You have date time elements in your data that have a',
          ' different number of elements than you indicated. For example, row',
          firstErrorRow, ' column ', tempDtCol, ' = ', firstErrorRowData
        )
        
        shinyalert::shinyalert(
          title = "ERROR",
          text = errorMsg,
          type = "error"
        )
        
        dtvRunning <<- FALSE
        show("glideDiv")
        w$hide()
        loadingScreenToggle('hide', 'processing dates')
        return()
      }
      

      # assign back to the temp holder just the correct index
      # of that date time element
      assign(selectedDateTimeElements[j],sapply(tempDateDataObjSplt,"[[",tempIndexOfLoc))

      if(j==validatorsLength){
        if(!exists('year')){
          modalMessager('ERROR','no year')
          dtvRunning<<-FALSE
          show("glideDiv")
          w$hide()
          loadingScreenToggle('hide', 'processing dates')
          #loadingScreenToggle('hide','processing dates')
          #progressIndicator('Done importing dates','stop')
          return()
        }
        
        if(!exists('month')){
          modalMessager('ERROR','no month')
          dtvRunning<<-FALSE
          show("glideDiv")
          w$hide()
          
          loadingScreenToggle('hide', 'processing dates')
         # loadingScreenToggle('hide','processing dates')
         # progressIndicator('Done importing dates','stop')
          return()
        }
        if(!exists('day')){
          modalMessager('ERROR','no day')
          dtvRunning<<-FALSE
          show("glideDiv")
          w$hide()
          
          loadingScreenToggle('hide', 'processing dates')
         # loadingScreenToggle('hide','processing dates')
         # progressIndicator('Done importing dates','stop')
          return()
        }
        if(!exists('hour')){
          hour<-"00"

          modalMessager('ERROR','no hour')
          show("glideDiv")
          w$hide()
          
          loadingScreenToggle('hide', 'processing dates')
          #dtvRunning<<-FALSE
          #loadingScreenToggle('hide','processing dates')
          #progressIndicator('Done importing dates','stop')
          return()
        }
        if(!exists('minute')){
          minute<-"00"
          return()
          
        }
        if(!exists('second')){
          second<-"00"
          return()
          
        }
# 
        # need to check for any decimals in the data
        checkForDec<-function(element,from){
          element<-as.numeric(element)
          isDecimal<-testInteger(element)
          if(!isDecimal){
            message<-paste0('You have decimals in one of you date elements.
            You need to remove decimals from the data and start over. Decimals were
            found in column ',from,'.')
            modalMessager('ERROR',message)
            show("glideDiv")
            dtvRunning<<-FALSE
           # progressIndicator('Done importing dates','stop')
            return(FALSE)
          }
        }

        testInteger <- function(x){
          test <- all.equal(x, as.integer(x), check.attributes = FALSE)
          if(test == TRUE){ return(TRUE) }
          else { return(FALSE) }
        }
        
        if (!("hour" %in% selectedDateTimeElements)) {
          selectedDateTimeElements <- c(selectedDateTimeElements, "hour")
          hour <- "00"
        }
        if (!("minute" %in% selectedDateTimeElements)) {
          selectedDateTimeElements <- c(selectedDateTimeElements, "minute")
          minute <- "00"
        }
        if (!("second" %in% selectedDateTimeElements)) {
          selectedDateTimeElements <- c(selectedDateTimeElements, "second")
          second <- "00"
        }
     
        
        checkForDec(year,'year')
        checkForDec(month,'month')
        checkForDec(day,'day')
        checkForDec(hour,'hour')
        checkForDec(minute,'minute')
        checkForDec(second,'second')
        
        if(testInteger(second)){
          oldSec<<-second
          second<<-round(second)
        }

        newDate<<-paste(year,month,day,sep="-")

        
        newTime <<- paste(as.character(hour), as.character(minute), as.character(second), sep = ":")
        #newTime<<-paste(hour,minute,second,sep=":")
        if(ampmtime){
          newTime<<-paste0(newTime,' ',ampm)
          rm(ampm)
        }
        newDateTime<<-paste(newDate,newTime,sep=" ")
        print(head(newDateTime,5))
        combineDateElements(newDateTime)

      }
    }


    
  }

  combineDateElements<-function(newDateTime){

    endTime<<-Sys.time()
    endTime-startTime

    stringFormat<-"%Y-%m-%d %H:%M:%S"

    if(ampmtime){
      stringFormat<-"%Y-%m-%d %I:%M:%S %p"
    }
    
    importedDatasetMaster$dateTest<<-newDateTime


    importedDatasetMaster@data$newMasterDate<<-tryCatch({
      as.POSIXct(strptime(
        newDateTime,
        format = stringFormat),
        tz =selectedTimezone
      )},
      error = function(cond) {
        modalMessager('ERROR',cond)
        show("glideDiv")
        w$hide()
        
        loadingScreenToggle('hide', 'processing dates')
        return()
      },
      warning = function(cond) {
        modalMessager('Warning',cond)
        show("glideDiv")
        w$hide()
        
        loadingScreenToggle('hide', 'processing dates')
        return()
      }
    )
    
    # #Add begin date field
    unique_ids <- unique(importedDatasetMaster$newUid)
    
    importedDatasetMaster$start_date <<- as.POSIXct(NA)
    
    for (uid in unique_ids) {
      subset_data <<- importedDatasetMaster[importedDatasetMaster$newUid == uid, ]
      min_start_date <<- min(subset_data$newMasterDate)
      importedDatasetMaster$start_date[importedDatasetMaster$newUid == uid] <<- min_start_date
    }
    

    
    #Add end date field
    importedDatasetMaster$end_date <<- as.POSIXct(NA)
    for (uid in unique_ids) {
      subset_data <<- importedDatasetMaster[importedDatasetMaster$newUid == uid, ]
      max_end_date <<- max(subset_data$newMasterDate)
      importedDatasetMaster$end_date[importedDatasetMaster$newUid == uid] <<- max_end_date
    }
    
    naDatesLength<<-nrow(importedDatasetMaster@data[is.na(as.Date(importedDatasetMaster@data$newMasterDate)),])
    configOptions$naDates<<-NULL
    configOptions$naDatesLength<<-naDatesLength
    # if(naDatesLength==nrow(importedDatasetMaster@data)){
    #   modalMessager('ERROR','Your selection of date elements failed.
    #   Check your selection of date/time elements and try again')
    #   dtvRunning<<-FALSE
    #   #progressIndicator('Done importing dates','stop')
    #   return()
    # }
    
    if (naDatesLength == nrow(importedDatasetMaster@data)) {
      errorMsg <- "Your selection of date elements failed. Check your selection of date/time elements and try again"
      
      shinyalert::shinyalert(
        title = "ERROR",
        text = errorMsg,
        type = "error"
      )
      
      dtvRunning <<- FALSE
      show("glideDiv")
      w$hide()
      
      loadingScreenToggle('hide', 'processing dates')
      return()
    }
    
    ### if NA dates are produced
    # importedDatasetMaster$naDates<<-0
    if(naDatesLength>0){
        naDatesObservations<-importedDatasetMaster@data[is.na(as.Date(importedDatasetMaster@data$newMasterDate)),]
        modalMessager('Warning',paste0('You had ',naDatesLength,' NA dates in your dataset. These have been saved to your working directory as naDates.csv if you would like to review them.'))
        write.csv(naDatesObservations,paste0(masterWorkingDirectory,'\\naDates.csv'))
        importedDatasetMaster<<-importedDatasetMaster[!is.na(as.Date(importedDatasetMaster@data$newMasterDate)),]
        #progressIndicator('Done importing dates','stop')
        createUniqueIdsHanlder()
      } else{
        #progressIndicator('Done importing dates','stop')
        createUniqueIdsHanlder()
      }
    
  }


  createUniqueIdsHanlder<-function(){
   # progressIndicator('Creating unique IDs','start')
    #progressIndicator('Success','stop')
    movementParamsProcessing();
  }


##------------------ lookup for R tech values and literal values
dateTimesList<-list(
  '%H'='Hour (1-24)',
  '%I'='Hour (1-12)',
  '%p'='AM/PM',
  '%M'='Minute (1-60)',
  '%S'='Second (0-60)',
  '%m'='Month (number: 1-12)',
  '%d'='Day (number: 1-31)',
  '%y'='Year (two digits: 17)',
  '%Y'='Year (four digits: 2017)',
  '%mon'='Month (abbreviated: Jan)',
  '%month'='Month (full name: January)'
  )

  ##------------------ reverse lookup
  dateTimesListFlipped<-list(
    'Hour (1-24)'='%H',
    'Hour (1-12)'='%I',
    'AM/PM'='%p',
    'Minute (1-60)'='%M',
    'Second (0-60)'='%S',
    'Month (number: 1-12)'='%m',
    'Day (number: 1-31)'='%d',
    'Year (two digits: 17)'='%y',
    'Year (four digits: 2017)'='%Y'

    )



possibleTimeSeperators<-c(':',' ')
possibleDateSeperators<-c('/','-','\\',' ','No Seperator')

dates<<-c('month','day','year')
times<<-c('hour','minute','second')



dateTimeLookup<<-list(
  '%H'='hour',
  '%I'='hour',
  '%M'='minute',
  '%S'='second',
  '%m'='month',
  '%d'='day',
  '%Y'='year',
  '%y'='year',
  '%p'='am/pm'

  )

validatorValues<<-c('year','month','day','hour','minute','second')
selectedTimezone<<-"GMT"
ampmtime<<-FALSE
