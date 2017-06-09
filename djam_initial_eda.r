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


# ONE NRL GAME ------------------------------------------------------------

g1 <- trx_NRL[MatchId==2630001]

View(g1)

length(unique(matches_NRL$MatchId))

players_NRL[PlayerId==500082]

View(g1[EventCode=='STPO'])

players_NRL[PlayerId==500001]


View(g1[EventCode=='STPO'])
table(g1$OppositionId)


table(g1$EventName)

sort(table(g1$EventName))

# PARTICIPATION -----------------------------------------------------------

# Is there one row per play per year?
part_player[, .N, by=.(Season, NRLID)] %>% .[N>1] # %>% .[N>2]

# Players can play in more than one League / Club in a season
# But no more than 2 in the data any way
View(part_player[Season == 2013 & NRLID==32381])

# How many distinct NRL IDs are there
length(unique(part_player$NRLID))
#316,051

for(i in sort(unique(part_player$Season))){
  
  dat <- part_player[Season==i]
  print(paste("There were", length(unique(dat$NRLID)), "unique players in Season", i))
  print(paste("There were", length(unique(dat$Club)), "unique clubs in Season", i))
  print(paste("There were", length(unique(dat$League)), "unique leagues in Season", i))
  
  print("################################")
  
}





# Get a list of event types
eventlist <- trx_NRL[, .N, by=EventName] %>% setorder(-N)
fwrite(eventlist, "output/eventlist.csv")

eventlist2 <- trx_NRL[, .N, by=.(EventName, Qualifier1Name, Qualifier2Name, Qualifier3Name, Qualifier4Name, Qualifier5Name, Qualifier6Name)] %>% setorder(-N)
fwrite(eventlist2, "output/eventlist2.csv")
