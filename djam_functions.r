################################################################################
# DATA JAM
# djam_functions.r
# Hana Sakai
# April 2017
################################################################################


# SETUP ENVIROMENT --------------------------------------------------------

setwd("E:\\Career\\PersonalProjects\\DataJam\\data")

library(data.table)
library(magrittr)
library(Hmisc)
library(lubridate)

# FUNCTIONS ---------------------------------------------------------------

# fReadData - This function reads in the data from the text files
fReadData <- function(fileloc, fileext="txt", set){
  
  print(paste("Reading in", fileloc))
  
  # Get the list of files in that folder
  fileloc <- paste0("received/", fileloc)
  files <- list.files(path=fileloc)
  
  # Ignore any readme files
  ignorefiles <- grep(pattern='ReadMe', files)
  
  # Update list of files to ignore readmes
  files <- files[-ignorefiles]
  
  for (i in files){
    
    print(paste("Reading in", i))
    
    # Extract the name of the file
    datname <- unlist(strsplit(i, split='\\.'))[1]
    
    # Read in the data
    dat <- fread(paste(fileloc, i, sep='/'))  
    
    # print(warnings())
    
    # Add a new column to the data denoting the set
    dat[, type:=set]
    
    # Assign the data to the name given
    assign(paste(tolower(datname), set, sep="_"), dat, envir = .GlobalEnv)
    
    print("#########################################")
    
  } # end i for 
  
  
} # end of fReadData function

################################################################################
# fSummariseData - This function creates the data summaries that I like to look at
# data is a character vector of dataset names
# sinkloc is the folder in which you want to save the output files.  Should have a / after it
# The file name is auto generated from the dataset name

fSummariseData <- function(data, sinkloc=F){
  
  # Make sure we have the package for the describe
  require(Hmisc)
  
  for(i in data){
    if (sinkloc!=F) {
      
      path <- paste0(sinkloc, "data_summary_", i, "_dat_", Sys.Date(), ".txt")
      
      sink(path)
      
    } # end if
    
    # Get the data
    dat <- get(i)
    
    # Print the data summaries to a file
    print(describe(dat))
    
    print("#####################################################################")
    print(str(dat))
    
    print("#####################################################################")
    print(summary(dat))
    
    if(sinkloc!=F){
      # End sink
      sink()
    } # end if
    
    rm(dat)
    
  } # end for loop
  
} # fSummariseData

################################################################################

fCleanTRXData <- function(dat){
  
  # ID vars are character in the trx datasets and they are numeric in the other datasets
  # Seems like data has been stacked with the headers included
  # trx_NRL[7840:7850]
  # trx_NRL[MatchId=='MatchId']
  # trx_NSWCup[MatchId=='MatchId']
  # trx_NYC[MatchId=='MatchId']
  # trx_QLDCup[MatchId=='MatchId']
  
  # Get the data from the string
  data <- get(dat)
  
  # Remove rows with duplicate header information
  data <- data[MatchId!='MatchId']
  
  # Now convert columns to numeric
  colnames <- c('MatchId', 'SeqNumber', 'SeasonId', 'SeriesId', 'RoundId', 
                'VenueId', 'WeatherConditionId', 'ClubId', 'OppositionId', 
                'PlayerId', 'Jumper', 'PositionId', 'Half', 'ElapsedMin', 
                'GameMin', 'Set', 'Tackle', 'DistanceMs', 'Points', 'Score', 
                'OppScore', 'Xm', 'Ym', 'ZoneId')
  
  # data <- data[, lapply(.SD, as.numeric), .SDcols=colnames]
  
  for (i in colnames) {
    set(data, i=NULL, j=i, as.numeric(data[[i]]))
  }
  
  # Now reassign the data back to the same name
  assign(dat, data, envir = .GlobalEnv)
  
} # end fCleanTRXData function

################################################################################
# ftrx_agg <-

ftrx_agg <- function(dat){
  
  receives <- dat[EventCode=="RCVS", .(receives=.N), by=.(MatchId, ClubId)]

  linebreak_assist <- dat[EventCode=="LAPS", .(linebreak_assist=.N), by=.(MatchId, ClubId)]
  
  tries <- dat[EventCode=="TRY", .(tries=.N), by=.(MatchId, ClubId)]
  
  try_assist <- dat[EventCode=="TRAS", .(try_assist=.N), by=.(MatchId, ClubId)]
  
  line_engage <- dat[EventCode=="RORS" & Qualifier2Name=="Run Impact", .(line_engage=.N), by=.(MatchId, ClubId)]
  
  supports <- dat[EventCode=="RSPS", .(supports=.N), by=.(MatchId, ClubId)]
  
  line_not_engage <- dat[EventCode=="LNNS", .(line_not_engage=.N), by=.(MatchId, ClubId)]
  
  breakcause <- dat[EventCode=="BKCS", .(breakcause=.N), by=.(MatchId, ClubId)]
  
  missed_tackles <- dat[EventCode=="TKMS" | EventCode=="TKMD", .(missed_tackles=.N), by=.(MatchId, ClubId)]
  
  # dat[EventCode=="ERRS", .N, by=Qualifier5]
  
  kick_error <- dat[EventCode=="ERRS" & Qualifier5=="Kicking error", .(kick_error=.N), by=.(MatchId, ClubId)]
  
  hand_error <- dat[EventCode=="ERRS" & Qualifier5=="Handling error", .(hand_error=.N), by=.(MatchId, ClubId)]
  
  ## Merge the trx aggs together
  trx_agg <- merge(receives, linebreak_assist, by=c("MatchId", "ClubId"), all=T)
  
  trx_agg <- merge(trx_agg, tries, by=c("MatchId", "ClubId"), all=T)          
  
  trx_agg <- merge(trx_agg, try_assist, by=c("MatchId", "ClubId"), all=T)
  
  trx_agg <- merge(trx_agg, line_engage, by=c("MatchId", "ClubId"), all=T)
  
  trx_agg <- merge(trx_agg, line_not_engage, by=c("MatchId", "ClubId"), all=T)
  
  trx_agg <- merge(trx_agg, kick_error, by=c("MatchId", "ClubId"), all=T)
  
  trx_agg <- merge(trx_agg, hand_error, by=c("MatchId", "ClubId"), all=T)
  
  trx_agg <- merge(trx_agg, supports, by=c("MatchId", "ClubId"), all=T)
  
  trx_agg <- merge(trx_agg, breakcause, by=c("MatchId", "ClubId"), all=T)
  
  trx_agg <- merge(trx_agg, missed_tackles, by=c("MatchId", "ClubId"), all=T)

  summary(trx_agg)
  return(trx_agg)
} # end function
