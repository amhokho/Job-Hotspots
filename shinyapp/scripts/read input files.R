#reading the claned race data

raceData <- read.csv(rDataAdd, header = T)  
#data from https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_15_5YR_B02001&prodType=table

#dictionaryData <- read.xlsx(dictionaryAdd, sheetIndex = 6)
# using the soc cip crosswalk table instead (socData)

##SETTING UP DATA FRAMES
completionData <- read.csv(cDataAdd, header = T)      #data from https://nces.ed.gov/ipeds/Home/UseTheData
institutionalData <- read.csv(iDataAdd, header = T)   
raceCensusData <- read.csv(rDataAdd, header = T)      #data from https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_15_5YR_B02001&prodType=table

##### additional for PhD Jobs

socData <- read.csv(socDataAdd, header = T)
occData <- read.csv(oDataAdd, header = T)
msaData <- read.csv(msaDataAdd, header = T)
metro <- read.csv(fipsDataAdd, header = T)

sixStatesData <- read.csv(sixDataAdd, header = T)   
#data from https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_15_5YR_B02001&prodType=table

# read IPEDS award level to Job Entry education required
jobEntryData <- read.csv(jobEntryDataAdd, header = T)

# read the 4 digit CIP crosswalk (for dropdown menus)
fourDigitsCrossWalk <- read.csv(fourDCrossAdd, header = T) 

readStates <- readRDS(statesRDS)


