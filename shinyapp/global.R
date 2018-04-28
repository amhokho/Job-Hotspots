library(leaflet)
library(markdown)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
#library(RCurl)
library(shiny)
library(sp)
library(maps)
library(ggplot2)
library(shinyjs)


###############------------------

load_data <- function(){
  
  ###***** first set your working directory - the location you have 'shiny app' folder
  wd<-"WORKING DIRECTORY"
  ##*****
  
  ## source dependencies
  #source(paste(wd,"scripts/PhDGraduationShiny_functions.R",sep = ""))
  source(paste(wd,"scripts/PhDJobsfunctionsShiny.R",sep = ""))
  source(paste(wd,"scripts/appFunctions.R",sep = ""))
  print("Functions sourced")
  source(paste(wd,"scripts/Addresses-shiny-within app.R",sep = ""))
  print("Addresses sourced")
  print("Loading files ...")
  source(paste(wd,"scripts/read input files.R",sep = ""),local=FALSE)
  print("Input files loaded. You can now start!")
  
}

