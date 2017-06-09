################################################################################
# DATA JAM
# djam_1_datasetup.r
# Hana Sakai
# April 2017
################################################################################

source("E:\\Career\\PersonalProjects\\DataJam\\code\\djam_functions.r")
load("djam_data_new.Rd")

################################################################################
# PERFORMANCE DATA
################################################################################

# READ IN THE PERFORMANCE DATA ---------------------------------------------

sink("output/log_readindata.txt")

# NRL
fReadData(fileloc = "Extract_2014-2016 NRL Update", set="NRL")

warnings()

# NYC
fReadData(fileloc = "Extract_2014-2016 NYC Update", set="NYC")

warnings()

# NSWCup
fReadData(fileloc = "Extract_2015-2016 NSW Cup Update", set="NSWCup")

warnings()

# QLDCup
fReadData(fileloc = "Extract_2015-2016 QLD Cup Update", set="QLDCup")

warnings()

sink()


# NRL
fReadData(fileloc = "Extract_2017 NRL Update", set="NRL")

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
# Venue Geocoordinates
################################################################################

clubgeo <- fread("Clubs_venues_geocoordinates.csv")

names(clubgeo) <- c("club_name", "league_name", "geographical_focus_area", "zone_name", 
                    "division_name", "club_venue_name", "club_venue_address", "club_venue_suburb", 
                    "club_venue_state", "club_venue_postcode", 
                    "club_venue_lat", "club_venue_lon", "club_lga")

# Replace venues_NRL with the geocoded version
venues_NRL <- fread("venues_NRL_geo.csv")

################################################################################
# SAVE --------------------------------------------------------------------

save.image("djam_data_new.Rd")

nrldatalist <- c("clubgeo", "venues_NRL", "clubs_NRL", "events_NRL", "lineups_NRL", 
                 "matches_NRL", "players_NRL", "positions_NRL", "seasons_NRL", 
                 "series_NRL", "trx_NRL", "weatherconditions_NRL")

save(list=nrldatalist, file="djam_data_nrl.Rd")

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
# SAVE PARTICIPATION ---------------------------------------------------------

# save(part_player, part_address, )