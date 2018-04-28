
## server function########


function(input, output, session) {
  ## load the data
  load_data()
  
  ## update the input items with the info read from files
  # preparing the list for input selection
  cipNames<-fourDigitsCrossWalk$title #dictionaryData$valuelabel
  cipCodes<-as.list(fourDigitsCrossWalk$CIP4d) #dictionaryData$codevalue)
  names(cipCodes)<-cipNames
  
  # make the dropdown for the degree level
  awardLevel <- unique(jobEntryData$ConsolidatedAwardLEVEL)

  # update programs menu items
  updateSelectizeInput(session,inputId = c("cipNamesJG"), choices= cipCodes
                       ,selected = "")
  updateSelectizeInput(session,inputId = c("cipNamesGeneral"), choices= cipCodes
                       ,selected = "")
  updateSelectizeInput(session,inputId = c("cipNamesGender"), choices= cipCodes
                       ,selected = "")
  updateSelectizeInput(session,inputId = c("cipNamesRace"), choices= cipCodes
                       ,selected = "")
  # update degree type menu items
  updateSelectizeInput(session,inputId = c("jobDegreeJG"), choices= awardLevel
                       ,selected = "Doctor's Degree")
  updateSelectizeInput(session,inputId = c("jobDegreeGeneral"), choices=awardLevel
                       ,selected = "Doctor's Degree")
  updateSelectizeInput(session,inputId = c("jobDegreeGender"), choices=awardLevel
                       ,selected = "Doctor's Degree")
  updateSelectizeInput(session,inputId = c("jobDegreeRace"), choices=awardLevel
                       ,selected = "Doctor's Degree")

 
  # Hide the loading message when the data is loaded and the menues are updated
  hide(id = "loading-content", anim = TRUE, animType = "fade") 
  
  #### Global Variables:
  # if needed to control the zoom, if you use it the background mapbox wipes-out
  lOptions <-leafletOptions(zoomControl = FALSE, minZoom = 3.1)
  ## General Map (normalized) ###########################################
  # draw the base map
  output$generalLeaflet <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
    
  })
  
  # this observeEvent is responsible to handle user interaction
  observeEvent(c(input$jobDegreeGeneral,input$cipNamesGeneral),{

   # print(paste("general cip =",input$cipNamesGeneral))
    
    myCode<-input$cipNamesGeneral
    
    ## handle the education level input:
    # make a lis of level codes(IPEDS compatible):
    awardLevelNumbers<-jobEntryData[jobEntryData$ConsolidatedAwardLEVEL %in% input$jobDegreeGeneral  ,]$IPEDSawdLevel
    
    #print(paste("general award level",paste(awardLevelNumbers)))
    
    # filtering the ipeds data
    data <- phdTables(myCode,completionData,institutionalData,raceCensusData,awardLevelNumbers)
    
    general2 <- sp::merge(readStates, data$g2, by = "name")

  ## graphing
    bins <- c(0, 10, 25, 50, 75, 100, Inf)
    pal <- colorBin("Purples", domain = general2$value, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>
      %.3g graduates per 1 million state residents<br/>
      %i total program graduates",
      general2$name, general2$value, general2$total
      ) %>% lapply(htmltools::HTML)
    
      leafletProxy("generalLeaflet",data=general2) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~pal(value),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(pal = pal, values = ~value,
                title = "Per 1 million state residents",
                opacity = 0.7,
                position = "bottomleft",layerId = "generalLegend")
    
  })
  
  ## Gender Map ###########################################
  # draw the base map
  output$genderLeaflet <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
    
  })
  # this observeEvent is responsible to handle user interaction
  observeEvent(c(input$jobDegreeGender,input$cipNamesGender),{
    myCode<- input$cipNamesGender
    
    #print(input$cipNamesGender)

    ## handle the education level input:
    # make a lis of level codes(IPEDS compatible):
    awardLevelNumbers<-jobEntryData[jobEntryData$ConsolidatedAwardLEVEL %in% input$jobDegreeGender  ,]$IPEDSawdLevel
    
    #print(paste("gender award level",paste(awardLevelNumbers)))
    
    # filtering the ipeds data
    data <- phdTables(myCode,completionData,institutionalData,raceCensusData,awardLevelNumbers)
    
    sex <- sp::merge(readStates, data$s, by = "name")
    
    ## graphing
    bins <- c(0, 5, 10, 25, 50, 75, 100)
    pal <- colorBin("Oranges", domain = sex$value, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>
      %.3g&#37 (female program graduates / total program graduates)<br/>
      %i program graduates",
      sex$name, sex$value, sex$total
      ) %>% lapply(htmltools::HTML)
    
    # update the leaflet
    leafletProxy("genderLeaflet",data=sex) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~pal(value),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(pal = pal, values = ~value, opacity = 0.7,
                title = "Females ratio (%)</br>",
                position = "bottomleft",layerId = "genderLegend")
    
  }
  )

  ## Race Map ###########################################

  # draw the base map
    output$raceLeaflet <- renderLeaflet({
      
      leaflet() %>%
        addTiles(
          urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
        ) %>%
        setView(lng = -93.85, lat = 37.45, zoom = 4) 
      
    })
    
    # This eventReactive is responsible for calculating the values based on the user inputs
  calculated_race_data <- eventReactive(c(input$jobDegreeRace,input$cipNamesRace),{

      myCode<- input$cipNamesRace
      
      #print(input$cipNamesRace)
      #print("race data is being calculated")
      
      ##### handle the education level input:
      
      # make a lis of level codes(IPEDS compatible):
      awardLevelNumbers<-jobEntryData[jobEntryData$ConsolidatedAwardLEVEL %in% input$jobDegreeRace  ,]$IPEDSawdLevel
      
      #print(awardLevelNumbers)
      
      # filtering the ipeds data
      data <- phdTables(myCode,completionData,institutionalData,raceCensusData,awardLevelNumbers)
      
      race <- sp::merge(readStates, data$r, by = "name")
      
      return(list(race=race))
      
    })
  
  # this observeEvent is responsible for redrawing the map
  # if only the race is changed, the calculations won't be done again
  
  observeEvent(c(input$jobDegreeRace,input$cipNamesRace,input$raceType),{
    
    race<-calculated_race_data()$race
    
    #print("drawing race data")
    
    #legend intervals
    bins <- c(0, 5, 10, 25, 50, 75, 100)
    pal <- colorBin("Greens", domain = race@data[[input$raceType]], bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>
      %.3g&#37 (American Indian or Alaska Native graduates)<br/>
      %.3g&#37 (Asian graduates)<br/>
      %.3g&#37 (Black or African American graduates)<br/>
      %.3g&#37 (Hispanic or Latino graduates)<br/>
      %.3g&#37 (Native Hawaiian or Other Pacific Islanders graduates)<br/>
      %.3g&#37 (White graduates)<br/>
      %.3g&#37 (Two or more races graduates)<br/>
      %.3g&#37 (Race/ethnicity unknown graduates)<br/>
      %.3g&#37 (Nonresident alien graduates)<br/>",
      race$name, race$`American Indian or Alaska Native`, race$`Asian`, race$`Black or African American`, race$`Hispanic or Latino`, race$`Native Hawaiian or Other Pacific Islanders`, race$`White`, race$`Two or more races`, race$`Race/ethnicity unknown`, race$`Nonresident alien`
    ) %>% lapply(htmltools::HTML)
    
    leafletProxy("raceLeaflet",data=race) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = pal(race@data[[input$raceType]]),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))%>%
      addLegend(pal = pal, values =race@data[c(input$raceType)] , opacity = 0.7, 
                title = paste("Ratio of<br/>",input$raceType," (%)",sep=""),
                position = "bottomleft", layerId="raceLegend")
    
    
  })

    
    #########################
    ## Grads-job
    # Create the map
  
  # draw the base map
    output$jgMap <- renderLeaflet({
      leaflet() %>%
        addTiles(
          urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
        ) %>%
        setView(lng = -93.85, lat = 37.45, zoom = 4)
        
    })
 
    # This eventReactive is responsible for calculating the numerical results 
    globalRes <- eventReactive(c(input$jobDegreeJG,input$cipNamesJG) ,{
      
      ## handle the education level input:
      
      # make a lis of level codes(IPEDS compatible):
      awardLevelNumbers<-jobEntryData[jobEntryData$ConsolidatedAwardLEVEL %in% input$jobDegreeJG  ,]$IPEDSawdLevel
      #print(awardLevelNumbers)
      
      # make a lis of Entry level education (BLS compatible):
      jobEntrylevel<-jobEntryData[jobEntryData$ConsolidatedAwardLEVEL %in% input$jobDegreeJG  ,]$JobEntryEducationLevel
      jobEntrylevel <- as.vector(unique(jobEntrylevel))
      #print(jobEntrylevel)
      
      ## handle the rest
      
      myCode<- input$cipNamesJG
      #myCode<- c(22.0101)
      
      #print(paste("selected cip for grads and jobs",paste(input$cipNames) ))
      
      # finds what subject that is
      #subject <- cipToMajor2(myCode,fourDigitsCrossWalk)
      #print(paste("subject is : " ))
      #print(subject)
      # use the subject to make a title if needed
      
      # finds the relevant data for that subject
      myData44 <- subjectPhDs44States(myCode,completionData,institutionalData,socData,occData
                                      ,msaData,metro,awardLevelNumbers,jobEntrylevel)
      myData6<-subjectPhDs6States(myCode,completionData,sixStatesData,socData,occData
                                  ,msaData,awardLevelNumbers,jobEntrylevel) 
      
      DataTotal<-rbind(myData44$mGradJobs,myData6$mGradJobs)
      
      #Attach the Long Lat
      r<-AttachLongLat(DataTotal,myData44$mFTranslation)
      r<-r$result
      
      # now r (results are ready for ploting)
      # rwturn the results
      return(list(r=r))
    })
    
    # this observer is responsibe for maintaining the circles and legend (choices to show either the jobs or the grads)
    # if other inputs are not changes the values won't be recalculated
    observeEvent(c(input$jobDegreeJG, input$cipNamesJG, input$jgChoice) , {
      r <- globalRes()$r
      
      #make the color map
      cCols <- c("red", "navy")
      pal <- colorFactor(cCols,  c("grads", "jobs"))
      
      pal1 <- colorFactor(c("yellow"),  c("grads"))
      pal2 <- colorFactor(c("red"),  c("jobs"))
      
      # handling empty results  
      if (length(r[, 1]) > 0) {
        # ordering the data (descending) for jobs:
        # cast the values to numeric
        r$value <- as.numeric(r$value)
        r <- r[order(-r$value), ]
        
        #format the big numbers, add commas, make a new column
        r$valueFormatted <-
          formatC(r$value, big.mark = ",", digits = 0,format = "f")
        
        #calculating total number of jobs and graduates
        totalJobs<-sum(r[r$type == "jobs",]$value, na.rm = TRUE)
        totalgrads<-sum(r[r$type == "grads",]$value, na.rm = TRUE)
        totalRatio<-totalJobs/totalgrads
        
        # formatting the numbers
        totalJobs<-formatC(totalJobs, big.mark = ",", digits = 0,format = "f")
        totalgrads<-formatC(totalgrads, big.mark = ",", digits = 0,format = "f")
        totalRatio<-formatC(totalRatio, big.mark = ",",format = "g" )
        
        # updating the overal statistics section on the bottom of the pannel
        output$nJobs<-output$text <- renderUI({
          title <- "Stats:"
          jobs <- paste("-Total number of jobs: ","<strong>",
                        "<font color='",cCols[2],"'>", totalJobs,"</strong>","</font>")
          grads <- paste("-Total number of grads: ","<strong>",
                         "<font color='",cCols[1],"'>", totalgrads,"</strong>","</font>")
          ratio <- paste("Ratio of total jobs to total grads: ","<strong>",
                         "<font color='","black","'>", totalRatio,"</strong>","</font>")
          
          HTML(
            
            paste(title,grads, jobs,"",ratio, sep = '<br/>')
          
          )
          
        }
        )
        
        
        ##make the labels:
        #grad labels
        gradlabels <- sprintf("<strong>%s</strong><br/>%s  %s",
                              r[r$type == "grads", ]$metroArea,
                              r[r$type == "grads", ]$valueFormatted ,
                              r[r$type == "grads", ]$type) %>% lapply(htmltools::HTML)
        
        # table of jobs and WAS
        jobsNumAws<-merge(r[r$type == "jobs", ],r[r$type == "WAS", ],by=c("metroArea","long","lat"))
        
        #rescaling the data for a better user experience
        #logistic function to scale
  
        # do not involve the WAS (weighted average salary) in the rescaling process
        scaleMax <- 1 / (1 + exp(-(max(r[!(r$type=="WAS"),]$value, na.rm = TRUE) / 20000)))
        
        r[!(r$type=="WAS"),]$value <- rescale(r[!(r$type=="WAS"),]$value, c(6000, scaleMax * 200000))
        #print(scaleMax*200000)
        
        #add the rescaled values to the jobsNumAws
        jobsNumAws<-merge(jobsNumAws,r[r$type == "jobs", ],by=c("metroArea","long","lat"))
        #value is what should be used in visualization
        
        # order values descending
        jobsNumAws <- jobsNumAws[order(-jobsNumAws$value.x), ]
        
        #job labels
        joblabels <- sprintf("<strong>%s</strong><br/>
                              <font color='navy'> <strong> %s </strong> </font> %s<br/> 

                             <font color='green'> <strong> $%s </strong> </font>
                              weighted average salary",
                             jobsNumAws$metroArea,
                             jobsNumAws$valueFormatted.x ,
                             jobsNumAws$type.x,
                             jobsNumAws$valueFormatted.y ) %>% lapply(htmltools::HTML)
       
         #clear all the shapes
        leafletProxy("jgMap") %>%
          clearShapes()
        
        #jobs and salary
        if (2 %in% input$jgChoice) {
        leafletProxy("jgMap", data = jobsNumAws) %>%
          addCircles(
            lng = ~ long,
            lat = ~ lat,
            weight = 1,
            radius = ~ value,
            fillOpacity = 0.2,
            label = joblabels,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto"
            ),
            color =~ pal(type)
          )
        }
        #grads
        if (1 %in% input$jgChoice) {
          leafletProxy("jgMap", data = r[r$type == "grads",]) %>%
            addCircles(
              lng = ~ long,
              lat = ~ lat,
              weight = 1,
              radius = ~ value,
              fillOpacity = 0.1,
              label = gradlabels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"
              ),
              color = ~ pal(type)
            ) 
        }
        
        #add the legend
        leafletProxy("jgMap", data = r)%>%
          addLegend(
            "bottomleft",
            colors = cCols,
            opacity=0.7,
            labels = c(
              paste("Number of Graduates (", input$jobDegreeJG, "-Level)", sep = "")
              ,
              paste("Number of Jobs (", input$jobDegreeJG, "-Level)", sep = "")
            )
            ,
            layerId = "jgLegend"
          )
        
      } else{
        leafletProxy("jgMap", data = r) %>%
          clearShapes()
        output$nJobs<-output$text <- renderUI({
          HTML("No Data Available")
          }
        )
        
      }
      
      
      
    })
    
  
}
