#library(xlsx)
library(choroplethr)
library(choroplethrMaps)
library(ggplot2)
library(formattable)
library(googleVis)
##FUNCTIONS##

# function to convert CIP code into the subject name
#cipToMajor <- function(x,dictionaryData) {
  #look into the dictionary for the translation
  
 # myRow = which(dictionaryData$codevalue %in% x)
  #return(dictionaryData[myRow,4])
#}


#function to get the needed information from the files (state and number of grads, categorical data)
subjectPhDs <- function(x,completionData,institutionalData,raceCensusData) {

  # keep the data we need
  levels <- c(17) #doctor's degrees, research PhDs
  phDs <- completionData[completionData$AWLEVEL %in% levels, ]
  
  # keep the majors we are interested in
  CIPlist<- c(x) #specific subject(s)
  subPhDs <- phDs[phDs$CIPCODE %in% CIPlist, ]
  
  #keep the columns we need from Completion data
  c1=c("UNITID","CIPCODE","AWLEVEL","CTOTALT","CTOTALW", "CAIANT", "CASIAT", "CBKAAT", "CHISPT", "CNHPIT", "CWHITT", "C2MORT", "CUNKNT", "CNRALT")
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
  statePop$region <- tolower(statePop$region)
  colnames(statePop) = c("state","pop")
  
  # merging state name for total phd grads
  phDClean <- merge(phDs, institutionalData, by="UNITID")
  # only keep state and number of total phd grads
  c7 <- c("STABBR", "CTOTALT")
  phDClean <- phDClean[c7]
  
  ##AGGREGATING DATA
  # aggregate the data for state totals for majors
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
                   list(state = stateFromLower(states$STABBR)),sum)
  
  # aggregate the data for state totals for all majors
  agg2 <- aggregate(list(grads = phDClean$CTOTALT),
                    list(state = stateFromLower(phDClean$STABBR)),sum)
  
  ##MORE MERGING and CALCULATING NUMBERS FOR MAPS
  # merging state graduates and particular graduates
  c8 <- c("state","grads","total")
  stateGrads <- merge(agg2, agg, by="state")[c8]
  stateGrads$ptgrad <- stateGrads$total/stateGrads$grads*100
  
  # merging state pop and particular graduates
  c9 <- c("state","pop","total")
  statePop <- merge(statePop, agg, by="state")[c9]
  statePop$ptpop <- (statePop$total)*1000000/as.numeric(as.character(statePop$pop)) #population is in million
  
  agg$prop <- agg$women/agg$total*100
  
  agg$paian <- percent(agg$aian/agg$total)*100
  agg$pasia <- percent(agg$asia/agg$total)*100
  agg$pbkaa <- percent(agg$bkaa/agg$total)*100
  agg$phisp <- percent(agg$hisp/agg$total)*100
  agg$pnhpi <- percent(agg$nhpi/agg$total)*100
  agg$pwhit <- percent(agg$whit/agg$total)*100
  agg$ptmor <- percent(agg$tmor/agg$total)*100
  agg$punkn <- percent(agg$unkn/agg$total)*100
  agg$pnral <- percent(agg$nral/agg$total)*100
  
  ##PUTTING NUMBERS IN CORRECT FORMAT
  # make tables for specific phd grads/total phd grads
  c4 <- c("state","ptgrad")
  general1 <- stateGrads[c4]
  colnames(general1) = c("region", "value")
  
  # make tables for specific phd grads/total population (in 10,000)
  c10 <- c("state","ptpop")
  general2 <- statePop[c10]
  colnames(general2) = c("region", "value")
  
  # make tables for phd grads by sex
  c5 <- c("state", "prop")
  sex <- agg[c5]
  colnames(sex) = c("region", "value")
  
  # make tables for phd grads by race
  c6 <- c("state", "paian", "pasia", "pbkaa", "phisp", "pnhpi", "pwhit", "ptmor", "punkn", "pnral")
  race <- agg[c6]
  colnames(race) = c("region", "American Indian or Alaska Native", "Asian", "Black or African American", "Hispanic or Latino", "Native Hawaiian or Other Pacific Islanders", "White", "Two or more races", "Race/ethnicity unknown", "Nonresident alien")
  
  return(list(g1=general1, g2=general2, s=sex, r = race, total=agg[,1:2]))
}


# function to convert state abb to full state name, from https://favorableoutcomes.wordpress.com/2012/10/19/create-an-r-function-to-convert-state-codes-to-full-state-name/#8230
stateFromLower <- function(x) {
  #read 52 state codes into local variable [includes DC (Washington D.C. and PR (Puerto Rico)]
  st.codes<-data.frame(
    state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                      "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                      "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                      "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
    full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                     "connecticut","district of columbia","delaware","florida","georgia",
                     "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                     "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                     "missouri","mississippi","montana","north carolina","north dakota",
                     "nebraska","new hampshire","new jersey","new mexico","nevada",
                     "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                     "rhode island","south carolina","south dakota","tennessee","texas",
                     "utah","virginia","vermont","washington","wisconsin",
                     "west virginia","wyoming"))
  )
  #create an nx1 data.frame of state codes from source column
  st.x<-data.frame(state=x)
  #match source codes with codes from 'st.codes' local variable and use to return the full state name
  refac.x<-st.codes$full[match(st.x$state,st.codes$state)]
  #return the full state names in the same order in which they appeared in the original source
  return(refac.x)
}


# function to create the map for general phd data
mappingG1 <- function(x, subject) {
  #Choropleth map of the US
  g1<-state_choropleth(x,
                   title = paste(paste(subject, collapse = " & "),
                                 "PhD Graduates Over Total State PhD Graduates (by state, 2014)",
                                 sep = " "),
                   legend = "Percent of State's PhD Graduates",
                   num_colors = 0,
                   zoom = NULL,
                   reference_map = FALSE)
  
  #make the directory if not exists
  dir.create(file.path("../output"), showWarnings = FALSE)
  # save the pic to the directory
  png(file="../output/g1.png")
  plot(g1)
  dev.off()
  
  plot(g1)
  
  }


mappingG2 <- function(x, subject) {
  #Choropleth map of the US
  g2<-state_choropleth(x,
                   title = paste(paste(subject, collapse = " & "),
                                 "PhD Graduates Over Total State Population (by state, 2014)",
                                 sep = " "),
                   legend = "Number of Graduates per 1MM People",
                   num_colors = 0,
                   zoom = NULL,
                   reference_map = FALSE)
  
  #make the directory if not exists
  dir.create(file.path("../output"), showWarnings = FALSE)
  # save the pic to the directory
  png(file="../output/g2.png")
  plot(g2)
  dev.off()
  
  plot(g2)
}

# function to create the map for sex
mappingS <- function(x, subject) {
  #Choropleth map of the US
  map <- state_choropleth(x,
                          title = paste("Sex of",
                                        paste(subject, collapse = " & "),
                                        "PhD Graduates",
                                        "(by state, 2014)",
                                        sep = " "),
                          legend = "Percentage of Women",
                          num_colors = 0,
                          zoom = NULL,
                          reference_map = FALSE)
  map<-map + scale_fill_continuous(low = "white", high = "green", space = "Lab", na.value = "black")
  
  
  #make the directory if not exists
  dir.create(file.path("../output"), showWarnings = FALSE)
  # save the pic to the directory
  png(file="../output/gender.png")
  plot(map)
  dev.off()
  
  plot(map)
  
}

# function to create the map for races
RaceMaps <- function(general,rGraguates, raceData) {
  
  # lowercase the region names
  raceData$region<- tolower(raceData$region)
  
  #keep the mutual rows 
  #raceData=raceData[raceData$region %in% general$region, ]
  # converting numbers to percentages
  for(i in 3:length(raceData[1,])) {
    raceData[,i]<-percent(raceData[,i]/raceData[,2])*100
  }
  
  #make the column names consistent
  #Amir changed 'Indian or Alaska Native' to 'American Indian or Alaska Native'
  c1=c("region","total","White","Black or African American","American Indian or Alaska Native","Asian", "Native Hawaiian or Other Pacific Islanders","Two or more races")
  
  
  colnames(raceData) = c1
  
  temp<-merge(raceData,rGraguates, by='region', all=TRUE)
  c2<- c("region", "Of population", "Of graduates")
  
  for(i in 3:length(c1)) {
    #keep the same race columns
    tempGraph<-temp[c(c1[1],paste(c1[i],"x",sep="."),paste(c1[i],"y",sep="."))]
    colnames(tempGraph) = c2
    GeoStates <- gvisGeoChart(tempGraph, c2[1], c2[3], c2[2], 
                              
                              options=list(
                                region="US", 
                                displayMode="regions",
                                resolution="provinces",
                                width=800, height=600)
    )
    
    #add the title to chart
    GeoStates<-addTitle(GeoStates,h1(c1[i]))
    #make the directory
    dir.create(file.path("../output"), showWarnings = FALSE)
    #store the graph in the directory
    cat(GeoStates$html$chart, file=paste("../output/",c1[i],".html",sep = ""))
    
    plot(GeoStates)
  }
  
}

# make the data frame
getRaceDataFrame <- function(general,rGraguates,raceData){
  
  # lowercase the region names
  raceData$region<- tolower(raceData$region)
  
  #keep the mutual rows 
  #raceData=raceData[raceData$region %in% general$region, ]
  # converting numbers to percentages
  for(i in 3:length(raceData[1,])) {
    raceData[,i]<-percent(raceData[,i]/raceData[,2])*100
  }
  
  #make the column names consistent
  #Amir changed 'Indian or Alaska Native' to 'American Indian or Alaska Native'
  c1=c("region","total","White","Black or African American","American Indian or Alaska Native","Asian", "Native Hawaiian or Other Pacific Islanders","Two or more races")
  
  
  colnames(raceData) = c1
  
  temp<-merge(raceData,rGraguates, by='region', all=TRUE)
return(temp)
}

# adds title to the html graph of gvisATL
addTitle <- function(gvisATL,title)  {
  
  if (!all(class(gvisATL) == c("gvis","list"))) {
    stop('ERROR in addGvisATLTitle: Incorrect type, expect gvisAnnotatedTimeLine.')
  }
  if (class(title) == "character") {
    gvisATL$html$chart['divChart'] <- paste(title,gvisATL$html$chart['divChart'],sep="")
  } else if (class(title) == "shiny.tag") {
    gvisATL$html$chart['divChart'] <- paste(as.character(title)[1],gvisATL$html$chart['divChart'],sep="")
  } else {
    stop('ERROR in addGvisATLTitle: Unknown title type.')
  }
  return(gvisATL)
}


# convert the text to htm header
h1 <- function(txt){
  
  txt<- paste("<h1> ",txt," (%)"," </h1>",sep="")
  return(txt)
}


