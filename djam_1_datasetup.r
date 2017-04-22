################################################################################
# DATA JAM
# djam_1_datasetup.r
# Hana Sakai
# April 2017
################################################################################

source("E:\\Career\\PersonalProjects\\DataJam\\code\\djam_functions.r")
load("djam_data.Rd")

################################################################################
# PERFORMANCE DATA
################################################################################

# READ IN THE PERFORMANCE DATA ---------------------------------------------

sink("output/log_readindata.txt")

# NRL
fReadData(fileloc = "Extract_2014-2016 NRL", set="NRL")

warnings()

# NYC
fReadData(fileloc = "Extract_2014-2016 NYC", set="NYC")

warnings()

# NSWCup
fReadData(fileloc = "Extract_2015-2016 NSW Cup", set="NSWCup")

warnings()

# QLDCup
fReadData(fileloc = "Extract_2015-2016 QLD Cup", set="QLDCup")

warnings()

sink()

################################################################################
# SUMMARISE THE RAW PERFORMANCE DATA ------------------------------------------

# List to loop over
types <- c("NRL", "NYC", "NSWCup", "QLDCup")

for (j in types){
  
  print(j)
  
  # Get the dataset names
  datnames <- ls()[grep(pattern=j, ls())]
  print(datnames)
  
  # Out put all of the data summaries for one type in the same file
  sink(paste0("output/data_summary_", j, ".txt"))
  
       for (i in datnames){
         
         print("##############################################################")
         print("##############################################################")
         print(i)
         print("##############################################################")
         fSummariseData(i)
         print("##############################################################")
       } # end i for loop
   
  sink()    
  
} # end j for loop

################################################################################
# DATA CLEANSING ----------------------------------------------------------

## Clean trx datasets
# ID vars are character in the trx datasets and they are umeric in the other datasets
# Seems like data has been stacked with the headers included

for (i in types){
  
  dat <- paste0("trx_", i)
  
  fCleanTRXData(dat)
  
} # end i for loop


################################################################################
# PARTICIPATION DATA
################################################################################

part_player <- fread("djam_players_combined.csv")

# Checking and Fixing up warnings on read
colnames <- c(names(part_player)[3], names(part_player)[22], names(part_player)[24])

# Convert to numeric
for (i in colnames) {
  set(part_player, i=NULL, j=i, as.numeric(part_player[[i]]))
}

summary(part_player)

# Now read in the player adresses
part_address <- fread("players_add_combined.csv")

# Checking and Fixing up warnings on read
# colnames <- c(names(part_address)[1], names(part_address)[5])
# colnames
# Want to keep post code as character
colnames <- c(names(part_address)[1])

# Convert to numeric
for (i in colnames) {
  set(part_address, i=NULL, j=i, as.numeric(part_address[[i]]))
}


# Remove dodgy postcode
part_address[, Post:=ifelse(Post=='#N/A', "", Post)]

# summary(part_address)
# describe(part_address)

fSummariseData(data=c("part_player", "part_address"), sinkloc="output/")

################################################################################
# SAVE --------------------------------------------------------------------

save.image("djam_data.Rd")


