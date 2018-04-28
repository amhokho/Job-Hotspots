##FUNCTIONS

# function to convert state abb to full state name, from https://favorableoutcomes.wordpress.com/2012/10/19/create-an-r-function-to-convert-state-codes-to-full-state-name/#8230
# modified to return uppercase
abbrToState <- function(x) {
  #read 52 state codes into local variable [includes DC (Washington D.C. and PR (Puerto Rico)]
  st.codes<-data.frame(
    state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                      "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                      "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                      "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
    full=as.factor(c("Alaska","Alabama","Arkansas","Arizona","California","Colorado",
                     "Connecticut","District of Columbia","Delaware","Florida","Georgia",
                     "Hawaii","Iowa","Idaho","Illinois","Indiana","Kansas","Kentucky",
                     "Louisiana","Massachusetts","Maryland","Maine","Michigan","Minnesota",
                     "Missouri","Mississippi","Montana","North Carolina","North Dakota",
                     "Nebraska","New Hampshire","New Jersey","New Mexico","Nevada",
                     "New York","Ohio","Oklahoma","Oregon","Pennsylvania","Puerto Rico",
                     "Rhode Island","South Carolina","South Dakota","Tennessee","Texas",
                     "Utah","Virginia","Vermont","Washington","Wisconsin",
                     "West Virginia","Wyoming"))
  )
  #create an nx1 data.frame of state codes from source column
  st.x<-data.frame(state=x)
  #match source codes with codes from 'st.codes' local variable and use to return the full state name
  refac.x<-st.codes$full[match(st.x$state,st.codes$state)]
  #return the full state names in the same order in which they appeared in the original source
  return(refac.x)
}


#function to get the needed information from the files (state and number of grads, categorical data)
phdTables <- function(x,completionData,institutionalData,raceCensusData,awardLevelNumbers) {
 
  # keep the data we need
  levels <- c(awardLevelNumbers) #doctor's degrees, research PhDs is 17
  phDs <- completionData[completionData$AWLEVEL %in% levels, ]
  
  # keep the majors we are interested in
  CIPlist<- c(x) #specific subject(s)
  subPhDs <- phDs[phDs$CIP4D %in% CIPlist, ]
  
  #keep the columns we need from Completion data
  c1=c("UNITID","CIP4D","AWLEVEL","CTOTALT","CTOTALW", "CAIANT", "CASIAT", "CBKAAT", "CHISPT", "CNHPIT", "CWHITT", "C2MORT", "CUNKNT", "CNRALT")
  subPhDs<-subPhDs[c1]
  
  # keep the columns from Institution data
  c2=c("UNITID","STABBR")
  institutionalData <- institutionalData[c2]
  
  # merging state name for particular phd grads
  subPhDClean <- merge(subPhDs, institutionalData, by="UNITID")
  
  # data (state and number of phD grads) for map
  c3 <- c("STABBR","CTOTALT","CTOTALW","CAIANT", "CASIAT", "CBKAAT", "CHISPT", "CNHPIT", "CWHITT", "C2MORT", "CUNKNT", "CNRALT")
  states <- subPhDClean[c3]
  
  # state population'
  statePop <- raceCensusData[c("region","total")]
  colnames(statePop) = c("state","pop")
  
  # merging state name for total phd grads
  phDClean <- merge(phDs, institutionalData, by="UNITID")
  # only keep state and number of total phd grads
  c7 <- c("STABBR", "CTOTALT")
  phDClean <- phDClean[c7]
  
  ##AGGREGATING DATA
  # aggregate the data for state totals for majors
  ##----
  if(length(states$CTOTALT)>0){
    agg <- aggregate(list(total = states$CTOTALT,
                          women = states$CTOTALW,
                          aian = states$CAIANT,
                          asia = states$CASIAT,
                          bkaa = states$CBKAAT,
                          hisp = states$CHISPT,
                          nhpi = states$CNHPIT,
                          whit = states$CWHITT,
                          tmor = states$C2MORT,
                          unkn = states$CUNKNT,
                          nral = states$CNRALT),
                     list(state = abbrToState(states$STABBR)),sum)

  }
  else{
    agg <- states
    colnames(agg) <- c("state","total","women","aian","asia","bkaa","hisp","nhpi","whit","tmor","unkn","nral")
    
  }
  
  if(length(phDClean$CTOTALT)>0){
    # aggregate the data for state totals for all majors
    agg2 <- aggregate(list(grads = phDClean$CTOTALT),
                      list(state = abbrToState(phDClean$STABBR)),sum)
  }
  else{
    agg2 <- phDClean
    colnames(agg2) <- c("state","grads")
    
  }
  ##-----
    
  ##MORE MERGING and CALCULATING NUMBERS FOR MAPS
  # merging state graduates and particular graduates
  c8 <- c("state","grads","total")
  stateGrads <- merge(agg2, agg, by="state")[c8]
  stateGrads$ptgrad <- (stateGrads$total)/(stateGrads$grad) * 100
  
  # merging state pop and particular graduates
  c9 <- c("state","pop","total")
  statePop <- merge(statePop, agg, by="state")[c9]
  statePop$ptpop <- (statePop$total)*1000000/as.numeric(as.character(statePop$pop)) #population is in million
  
  agg$prop <- agg$women/agg$total*100
  
  agg$paian <- (agg$aian/agg$total)*100
  agg$pasia <- (agg$asia/agg$total)*100
  agg$pbkaa <- (agg$bkaa/agg$total)*100
  agg$phisp <- (agg$hisp/agg$total)*100
  agg$pnhpi <- (agg$nhpi/agg$total)*100
  agg$pwhit <- (agg$whit/agg$total)*100
  agg$ptmor <- (agg$tmor/agg$total)*100
  agg$punkn <- (agg$unkn/agg$total)*100
  agg$pnral <- (agg$nral/agg$total)*100
  
  ##PUTTING NUMBERS IN CORRECT FORMAT
  # make tables for specific phd grads/total phd grads
  c4 <- c("state","ptgrad")
  general1 <- stateGrads[c4]
  colnames(general1) = c("name", "value")
  
  # make tables for specific phd grads/total population (in 1,000,000)
  c10 <- c("state","ptpop")
  general2 <- statePop[c10]
  colnames(general2) = c("name", "value")
  
  # make tables for phd grads by sex
  c5 <- c("state", "prop")
  sex <- agg[c5]
  colnames(sex) = c("name", "value")
  
  # make tables for phd grads by race
  c6 <- c("state", "paian", "pasia", "pbkaa", "phisp", "pnhpi", "pwhit", "ptmor", "punkn", "pnral")
  race <- agg[c6]
  colnames(race) = c("name", "American Indian or Alaska Native", "Asian", "Black or African American", "Hispanic or Latino", "Native Hawaiian or Other Pacific Islanders", "White", "Two or more races", "Race/ethnicity unknown", "Nonresident alien")
  
  c11 <- c("state","total")
  total <- agg[c11]
  colnames(total) <- c("name", "total")
  
  general1 <- merge(general1, total, by = "name")
  general2 <- merge(general2, total, by = "name")
  sex <- merge(sex, total, by = "name")
  
  
  return(list(g1=general1, g2=general2, s=sex, r = race, t = total))
}



graphG1 <- function(x) {
  bins <- c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.5, 2, 100)
  pal <- colorBin("YlOrRd", domain = x$value, bins = bins)
  
  labels <- sprintf(
    "<strong>%s</strong><br/>
    %g program graduates / total graduates (&#37)<br/>
    %i program graduates",
    x$name, x$value, x$total
  ) %>% lapply(htmltools::HTML)
  
  
  leaflet(x) %>%
    setView(-96, 37.8, 4) %>%
    addProviderTiles("MapBox", options = providerTileOptions(
      id = "mapbox.light",
      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
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
    addLegend(pal = pal, values = ~value, opacity = 0.7, title = NULL,
              position = "bottomright")
}

graphG2 <- function(x) {
  
  bins <- c(0, 0.5, 1, 2.5, 5, 7.5, 10, 100)
  pal <- colorBin("YlOrRd", domain = x$value, bins = bins)
  
  # the lables are wrong realized on july 20 2017
  labels <- sprintf(
    "<strong>%s</strong><br/>
    %g program graduates / population (&#37)<br/>
    %i program graduates",
    x$name, x$value, x$total
    ) %>% lapply(htmltools::HTML)
  
  
  leaflet(x) %>%
    setView(-96, 37.8, 4) %>%
    addProviderTiles("MapBox", options = providerTileOptions(
      id = "mapbox.light",
      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
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
    addLegend(pal = pal, values = ~value, opacity = 0.7, title = NULL,
              position = "bottomright")
}

graphS <- function(x) {
  
  bins <- c(0, 5, 10, 25, 50, 75, 100)
  pal <- colorBin("YlOrRd", domain = x$value, bins = bins)
  
  labels <- sprintf(
    "<strong>%s</strong><br/>
    %g female program graduates / total program graduates (&#37)<br/>
    %i program graduates",
    x$name, x$value, x$total
    ) %>% lapply(htmltools::HTML)
  
  
  leaflet(x) %>%
    setView(-96, 37.8, 4) %>%
    addProviderTiles("MapBox", options = providerTileOptions(
      id = "mapbox.light",
      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
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
    addLegend(pal = pal, values = ~value, opacity = 0.7, title = NULL,
              position = "bottomright")
}

graphR <- function(x) {
  
  bins <- c(0, 5, 10, 25, 50, 75, 100)
  pal <- colorBin("YlOrRd", domain = x$White, bins = bins)
  
  labels <- sprintf(
    "<strong>%s</strong><br/>
    %g American Indian or Alaska Native program graduates (&#37)<br/>
    %g Asian program graduates (&#37)<br/>
    %g Black of African American program graduates (&#37)<br/>
    %g Native Hawaiian or Other Pacific Islanders program graduates (&#37)<br/>
    %g White program graduates (&#37)<br/>
    %g Two or more races program graduates (&#37)<br/>",
    x$name, x$`American Indian or Alaska Native`, x$`Asian`,
    x$`Black or African American`, x$`Native Hawaiian or Other Pacific Islanders`, 
    x$`White`, x$`Two or more races`
    ) %>% lapply(htmltools::HTML)
  
  
leaflet(x) %>%
    setView(-96, 37.8, 4) %>%
    addProviderTiles("MapBox", options = providerTileOptions(
      id = "mapbox.light",
      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
    addPolygons(
      fillColor = ~pal(White),
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
    addLegend(pal = pal, values = ~White, opacity = 0.7, title = NULL,
              position = "bottomright")
  

}

