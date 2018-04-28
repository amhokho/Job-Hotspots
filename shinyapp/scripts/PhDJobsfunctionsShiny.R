for empty data.frames on merges

##FUNCTIONS##

# function to convert CIP code into the subject name

cipToMajor2 <- function(x,fourDigitsCrossWalk) {
  #look into the cip soc crosswalk for the translation
  
  myRow = which(fourDigitsCrossWalk$CIP4d %in% x)
  return(as.character(fourDigitsCrossWalk$title[myRow]) ) 
  
}

#metroData <- read.xlsx(mDataAdd, sheetIndex = 1)

#function to get the needed information from the files (state and number of grads, categorical data)
subjectPhDs44States<- function(x,completionData,institutionalData,socData,occData,msaData,metro,gradLevels,jobLevels) {

  #trouble making states  
states<-c("CT","MA","ME","NH","RI","VT") 

# filter out trouble making states 
institutionalData<-institutionalData[!(institutionalData$STABBR %in% states),]

  c13 <- c("soc2010", "CIP4D")
  soc <- socData[c13]
  
  occ <- occData[ ,2:3]
  colnames(occ) <- c("soc", "educ")
 
   # filter out trouble making states
  msaData<-msaData[!(msaData$PRIM_STATE %in% states),]
  
  c14 <- c("AREA_NAME", "OCC_CODE","TOT_EMP","A_MEAN")
  msa <- msaData[c14]
  
  
  ##METRO FIPS TABLE#################
  c12 <- c("countyFIPS", "metArea")
  metro <- metro[c12]
  colnames(metro) <- c("county", "metro")
  
  
  ##GRAUDATION#############
  # keep the graduation data we need
 # gradLevels <- c(17) #doctor's degrees, research PhDs
  phDs <- completionData[completionData$AWLEVEL %in% gradLevels, ]
  
  # keep the majors we are interested in
  CIPlist<- c(x) #specific subject(s)
  subPhDs <- phDs[phDs$CIP4D %in% CIPlist, ]
  
  # keep the columns we need from Completion data
  c1=c("UNITID","CIP4D","AWLEVEL","CTOTALT")
  subPhDs<-subPhDs[c1]
  
  # keep the columns from Institution data
  c2=c("UNITID","COUNTYCD")
  institutionalData <- institutionalData[c2]
  
  # merging institution name for particular phd grads
  subPhDClean <- merge(subPhDs, institutionalData, by="UNITID")
  
  # clean data (county and number of phD grads)
  c3 <- c("COUNTYCD","CTOTALT")
  counties <- subPhDClean[c3]
  
  # aggregate grad data for by counties
  ##----
  if(length(counties$CTOTALT)>0){
    agg <- aggregate(list(total = counties$CTOTALT),
                     list(county = counties$COUNTYCD),sum)
  }else{
    agg <- counties
    colnames(agg)<-c("total","county")
  }
  ##-----
  
  # merge the metro and grads
  c11 <- c("metro", "total")
  metGrads <- merge(metro, agg, by="county")
  metGrads <- metGrads[c11]
  
  # aggregate grad data by metro area
  ##----
  if(length(metGrads$total)>0){
    metGrads <- aggregate(list(totalGrads = metGrads$total),
                          list(metroArea = metGrads$metro), sum)
  }else{
    metGrads <- metGrads
    colnames(metGrads)<-c("totalGrads","metroArea")
  }
  ##-----

  ##EMPLOYMENT##############
  # keep the job titles we need
  #jobLevels <- c("Doctoral or professional degree")
  occ <- occ[occ$educ %in% jobLevels, ]
  
  # finding the socs for the cip
  degree <- c(x) #specific degree
  availableJobs <- soc[soc$CIP4D %in% degree, ]
  colnames(availableJobs) <- c("soc", "cip")
  
  # keep the jobs with phD level requirement
  phDJobs <- merge(availableJobs, occ, by="soc")
  socs <- phDJobs$soc
  
  #############
  # find the employment for those specific phd jobs
  emp <- msa[msa$OCC_CODE %in% phDJobs$soc, ]
  c15 <- c("AREA_NAME", "TOT_EMP","A_MEAN")
  emp <- emp[c15]
  emp1 <- emp
  
  # remove missing data and format numbers
  
  emp1 <- subset(emp,!(TOT_EMP=="**" ) )
  emp1 <- subset(emp1,!(A_MEAN=="*" ) )
  emp1$TOT_EMP <- as.numeric(gsub(",", "", as.character(emp1$TOT_EMP)))
  emp1$A_MEAN <- as.numeric(gsub(",", "", as.character(emp1$A_MEAN)))
  
  #multiply the salary by the numbers
  emp1$A_MM<-emp1$TOT_EMP*emp1$A_MEAN
  
  # aggregate by metro area
  if(length(emp1$AREA_NAME)>0){
    metJobs <- aggregate(list(totalEmployment = emp1$TOT_EMP, totalPaid= emp1$A_MM),
                         list(metroArea = emp1$AREA_NAME), sum)
    #claculate the weighted average salary
    metJobs$weightedAverageSalary<-metJobs$totalPaid/metJobs$totalEmployment
    
    metJobs<-metJobs[c("metroArea","totalEmployment","weightedAverageSalary")]
  }else{
    metJobs<-emp1
    colnames(metJobs)<- c("metroArea","totalEmployment","weightedAverageSalary")
  }
  
  ##MERGE Grads and Employment########
  merged = merge(metGrads, metJobs, by="metroArea",all = TRUE)
  
  
  return(list(mGradJobs = merged, mFTranslation = metro))
}

#function to get the needed information from the files (state and number of grads, categorical data)
subjectPhDs6States<- function(x,completionData,sixStatesData,socData,occData,msaData,gradLevels,jobLevels) {
  
  c13 <- c("soc2010", "CIP4D")
  soc <- socData[c13]
  
  occ <- occData[ ,2:3]
  colnames(occ) <- c("soc", "educ")

  # Trouble making states
  states<-c("CT","MA","ME","NH","RI","VT")
  msaDataNew<-msaData[msaData$PRIM_STATE %in% states,]
  c14 <- c("AREA_NAME", "OCC_CODE","TOT_EMP","A_MEAN")
  msa <- msaDataNew[c14]
  
  
  ##GRAUDATION#############
  # keep the graduation data we need
  #gradLevels <- c(17) #doctor's degrees, research PhDs
  phDs <- completionData[completionData$AWLEVEL %in% gradLevels, ]
  
  # keep the majors we are interested in
  CIPlist<- c(x) #specific subject(s)
  subPhDs <- phDs[phDs$CIP4D %in% CIPlist, ]
  
  # keep the columns we need from Completion data
  c1=c("UNITID","CIP4D","AWLEVEL","CTOTALT")
  subPhDs<-subPhDs[c1]
  
  # keep the columns from Institution data
  c2=c("UNITID","metArea")
  sixStatesData <- sixStatesData[c2]
  
  # merging institution name for particular phd grads
  subPhDClean <- merge(subPhDs, sixStatesData, by="UNITID")
  
  # clean data (county and number of phD grads)
  c3 <- c("metArea","CTOTALT")
  metros <- subPhDClean[c3]
  
  # aggregate grad data for by mrt area
  ##-------
  if(length(metros$CTOTALT)>0){
    agg <- aggregate(list(totalGrads = metros$CTOTALT),
                     list(metroArea = metros$metArea),sum)
  }else{
    agg <- metros
    colnames(agg)<-c("totalGrads","metroArea")
  }
  ##----
  metGrads<-agg
  
  
  ##EMPLOYMENT##############
  # keep the job titles we need
  
  #jobLevels <- c("Doctoral or professional degree")
  occ <- occ[occ$educ %in% jobLevels, ]
  
  # finding the socs for the cip
  degree <- c(x) #specific degree
  availableJobs <- soc[soc$CIP4D %in% degree, ]
  colnames(availableJobs) <- c("soc", "cip")
  
  # keep the jobs with phD level requirement
  #phDJobs <- soc[soc$soc2010 %in% occ$soc, ]
  phDJobs <- merge(availableJobs, occ, by="soc")
  socs <- phDJobs$soc
  
  # find the employment for those specific phd jobs
  emp <- msa[msa$OCC_CODE %in% phDJobs$soc, ]
  c15 <- c("AREA_NAME", "TOT_EMP","A_MEAN")
  emp <- emp[c15]
  emp1 <- emp
  
  # remove missing data and format numbers
  
  emp1 <- subset(emp,!(TOT_EMP=="**" ) )
  emp1 <- subset(emp1,!(A_MEAN=="*" ) )
  emp1$TOT_EMP <- as.numeric(gsub(",", "", as.character(emp1$TOT_EMP)))
  emp1$A_MEAN <- as.numeric(gsub(",", "", as.character(emp1$A_MEAN)))
  
  #multiply the salary by the numbers
  emp1$A_MM<-emp1$TOT_EMP*emp1$A_MEAN
  
  # aggregate by metro area
  if(length(emp1$AREA_NAME)>0){
    metJobs <- aggregate(list(totalEmployment = emp1$TOT_EMP, totalPaid= emp1$A_MM),
                         list(metroArea = emp1$AREA_NAME), sum)
    #claculate the weighted average salary
    metJobs$weightedAverageSalary<-metJobs$totalPaid/metJobs$totalEmployment
    
    metJobs<-metJobs[c("metroArea","totalEmployment","weightedAverageSalary")]
  }else{
    metJobs<-emp1
    colnames(metJobs)<- c("metroArea","totalEmployment","weightedAverageSalary")
  }
  

  
  
  ##MERGE Grads and Employment########
  merged = merge(metGrads, metJobs, by="metroArea",all = TRUE)
  
  return(list(mGradJobs = merged))
}

AttachLongLat <- function(inf,convertor) {
  
  # make a data frame of FIPS | Met | Long | Lat | Information
  
  county_df <- map_data("county")
  county_df$county=paste(county_df$region,county_df$subregion,sep = ",")
  
  #only keep Long,Lat,county
  county_df<-county_df[c("long","lat","county")]
  
  #Average the geographical coordinates
  county_df<-aggregate(
    list(long=county_df$long,lat=county_df$lat),
    by=list(county=county_df$county),
    FUN=mean)
  
  #county fips
  d<-county.fips
  colnames(d)<-c("countyFIPS","county")
  # make the county names consistent with geographical location data table
  
  d$county<-unlist( 
    lapply(
      d$county, function (x) unlist( 
        strsplit(as.character(x), ":") 
      ) [1]
    )
  )
  
  #delete redundant data
  d<-unique(d)
  
  # join geographical coordinated with the Fips
  geoTable<-merge(county_df,d,by=c("county") )
  
  # convertor and if are our input
  
  colnames(convertor)[which( colnames(convertor)=="county" )]<-"countyFIPS"
  midTable<-merge(geoTable,convertor,by=c("countyFIPS") )
  midTableGeo<-midTable[c("long","lat","metro")]
  
  #average the coordinates
  midTableGeo<-aggregate(
    list(long=midTableGeo$long,lat=midTableGeo$lat),
    by=list(metroArea=midTableGeo$metro),
    FUN=mean)
  # attach the coordinates to met name
  metGeoData <- merge(midTableGeo,inf[,1:2],by=c("metroArea"))
  
  #handling the empty dataframe
  temp<-data.frame(type = character(length(metGeoData[,1])))
  metGeoData<-cbind(metGeoData,temp)
  if(length(metGeoData[,1])>0){
    metGeoData$type <- "grads"
  }
  
  # adding the number of jobs
  colnames(metGeoData) <- c("metroArea", "long", "lat", "value", "type")
  c20 <- c("metroArea", "totalEmployment")
  metGeoData1 <- merge(midTableGeo,inf[c20],by=c("metroArea"))
  
  #handling the empty dataframe
  temp<-data.frame(type = character(length(metGeoData1[,1])))
  metGeoData1<-cbind(metGeoData1,temp)
  if(length(metGeoData1[,1])>0){
    metGeoData1$type <- "jobs"
  }
  colnames(metGeoData1) <- c("metroArea", "long", "lat", "value", "type")
  
  # adding the average salary
  c20 <- c("metroArea", "weightedAverageSalary")
 
  metGeoData2 <- merge(midTableGeo,inf[c20],by=c("metroArea"))
  
  #handling the empty dataframe
  temp<-data.frame(type = character(length(metGeoData2[,1])))
  metGeoData2<-cbind(metGeoData2,temp)
  if(length(metGeoData2[,1])>0){
    metGeoData2$type <- "WAS"
  }
  colnames(metGeoData2) <- c("metroArea", "long", "lat", "value", "type")
  
  
  
  metGeoDataFinal <- rbind(metGeoData2,metGeoData1, metGeoData)
  
  return(list(result=metGeoDataFinal))
}


plotOnMap <- function(theData) {
  usa <- map_data("state")
  plot<-ggplot() +
    geom_path(data = usa, aes(x = long, y = lat, group = group)) +
    coord_fixed(1.3) + 
    geom_point(data = theData,
               aes(x = long, y = lat, group = type,
                   shape = type, size = value, colour = type),
               alpha = 0.7) +
    labs(title = "Number of grads and jobs for Computer Scinece PhD", 
         subtitle = "Data from 2014 ",
         caption = "Data from IPEDS and BLS",
         x = "latitude", y = "longitude")

  return(list(plot))
}


