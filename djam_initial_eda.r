################################################################################
# DATA JAM
# djam_initial_eda.r
# Hana Sakai
# April 2017
################################################################################

source("E:\\Career\\PersonalProjects\\DataJam\\code\\djam_functions.r")

load("djam_data.Rd")


# DATA CHECKS -------------------------------------------------------------

## Can we stack the Performance data into a database?

datnames <- c("clubs", "events")

dattypes <- c("NRL", "NYC", "NSWCup", "QLDCup")

for(i in datnames){
  
  for(j in dattypes){
    
    dat_str <- paste0(i, "_", j)
    
    if(dat_str %in% ls()){
      
      dat <- get(dat_str)
      
    } else { next }
    
    
    
  } # end j for
  
} # end i for

