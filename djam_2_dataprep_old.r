################################################################################
# DATA JAM
# djam_dataprep.r
# Hana Sakai
# April 2017
################################################################################

source("E:\\Career\\PersonalProjects\\DataJam\\code\\djam_functions.r")

load("djam_data.Rd")

dattypes <- c("NRL", "NYC", "NSWCup", "QLDCup")

# MATCH-TEAM --------------------------------------------------------------

## Create some game outcomes for each team

# Create a dataset with one row for each team-match

# Create a new variable for the winner
matches_NRL[, winner:=ifelse(TeamAScore > TeamBScore, "A", 
                             ifelse(TeamBScore > TeamAScore, "B", 
                                    ifelse(TeamAScore == TeamBScore, "D", NA)))]

# Calculate points difference as wel!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


describe(matches_NRL$winner)

# Team A matches
colnames <- names(matches_NRL)[!names(matches_NRL) %in% c("TeamBId", "TeamBName", "TeamBScore")]

teama <- matches_NRL[, colnames, with=F]

# Update the cloumn names
setnames(teama, c("TeamAId", "TeamAName", "TeamAScore"), c("TeamID", "TeamName", "TeamScore"))

# Update the winner column
teama[, win:=ifelse(winner=="A", 1, 0)]
teama[, lose:=ifelse(winner=="B", 0, 1)]
teama[, draw:=ifelse(winner=="D", 1, 0)]

# Create an indicator for home game or not
# teama[, home:=1]

################################################################################
# Team B matches
colnames <- names(matches_NRL)[!names(matches_NRL) %in% c("TeamAId", "TeamAName", "TeamAScore")]

teamb <- matches_NRL[, colnames, with=F]

# Update the cloumn names
setnames(teamb, c("TeamBId", "TeamBName", "TeamBScore"), c("TeamID", "TeamName", "TeamScore"))

# Update the winner column
teamb[, win:=ifelse(winner=="B", 1, 0)]
teamb[, lose:=ifelse(winner=="A", 0, 1)]
teamb[, draw:=ifelse(winner=="D", 1, 0)]

# Create an indicator for home game or not
# teamb[, home:=0]

################################################################################
# Stack teama and teamb

teamgames <- rbind(teama, teamb)

# rm(teama, teamb)

teamgames[, match_date:=ymd(paste(MatchYear, MatchMonth, MatchDay))]

describe(teamgames$match_date)

################################################################################
# How many matches has the club played since 2014
teamlist <- sort(unique(teamgames[, TeamID]))

for(i in 1:length(teamlist)){
  
  tmp <- teamgames[TeamID==teamlist[i]]
  
  # Order by date
  setorder(tmp, match_date)
  tmp[, NRL_club_match_count:=1:nrow(tmp)]
  
  if(i==1){
    p <- tmp
  } else {
    p <- rbind(p, tmp)
  }
  
}

################################################################################



# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Merge on geo coordinates for venue and team home stadium
# club_ prefix denotes the home venue location
teamgames <- merge(teamgames, 
                   clubgeo[league_name=="National Rugby League", 
                           .(club_name, club_venue_name, club_venue_address, 
                             club_venue_lat, club_venue_lon)], 
                   by.x="TeamName", by.y="club_name", al.x=T)

# non club_ prefix denotes the location of the actual game
# Can't merge on vebue name because the datasets use diferent names in each :(

teamgames <- merge(teamgames, 
                   clubgeo[league_name=="National Rugby League", 
                           .(club_name, venue_name, venue_address, 
                             venue_lat, venue_lon)], 
                   by.x="TeamName", by.y="club_name", al.x=T)

# Maybe I can estimate the home ground venue id
# View(unique(teamgames[home==1, .(TeamName, VenueId, VenueNam, club_venue_name, club_venue_address)]))

# View(teamgames[home==1, .N, by=.(TeamName, VenueId, VenueNam, club_venue_name, club_venue_address)])

# Calculate the distance between 
# If home =1 then distance should be zero

# clubgeo[league_name=="National Rugby League", .(club_name, club_venue_name, club_venue_address, club_venue_state, club_venue_lat, club_venue_lon)]

# Now have venues_NRL geocoded
teamgames <- merge(teamgames, venues_NRL[, .(VenueId, venue_lat, venue_lon)], by="VenueId")

################################################################################
# player-game

lineups_NRL[]


################################################################################

# PLAYER LEVEL ------------------------------------------------------------

# This section calculates some player level stats

# Number of matches played by each player
# Check that a player can appear once per match
lineups_NRL[, .N, by=.(PlayerId, MatchId)] %>% .[N>1]

# Check how many clubs can play in a match
tmp <- lineups_NRL[, .N, by=.(MatchId, ClubId)]

tmp2 <- tmp[, .N, by=.(MatchId)]
setorder(tmp2, -N)
tmp2
tmp2[MatchId==2630001]

# Check a match
matches_NRL[MatchId==2630001]

# There are players from more than 2 clubs playing in the match
lineups_NRL[MatchId==2630001]

lineups_NRL[MatchId==2630001 & ClubId!=500001 & ClubId!=500005]

lineups_NRL[MatchId==2630001, .N, by=ClubId]

# So in the lineups the 1st 17 rows relate to the lowest club number for the match

# Add a column to the lineups for the ClubId playing for calling the new data lups_NRL
matchlist <- sort(unique(matches_NRL[, MatchId]))

for(i in 1:length(matchlist)){
  
  m <- matches_NRL[MatchId==matchlist[i]]
  
  # Get a list of teams and sort the teams so that smaller team number is first
  teams <- sort(m[, c(TeamAId, TeamBId)])
  
  # Do some manual fudging for a couple of matches without 34 players listed
  # For matches with only 16 players in team 1
  if(matchlist[i]==3130186){
    
    tmp <- lineups_NRL[MatchId==matchlist[i]]
    tmp[1:16, ClubId2:=teams[1]]
    tmp[17:33, ClubId2:=teams[2]]
    
  } else {
    
    # For matches with only 16 players in team 2
    if(matchlist[i]==3130184){
      
      tmp <- lineups_NRL[MatchId==matchlist[i]]
      tmp[1:17, ClubId2:=teams[1]]
      tmp[18:33, ClubId2:=teams[2]]
      
    } else {
      
      # For matches where there are 17 players in each team
      tmp <- lineups_NRL[MatchId==matchlist[i]]
      tmp[1:17, ClubId2:=teams[1]]
      tmp[18:34, ClubId2:=teams[2]]
      
    }
    
  }
  
  if(i==1){
    lups_NRL <- tmp
  } else {
    lups_NRL <- rbind(lups_NRL, tmp)
  } # end if
  
} # end for 

# Merge on the match info to the lineups data
player_match_dat <- merge(lups_NRL, 
                   teamgames[, .(MatchId, MatchYear, MatchMonth, MatchDay, RoundId, 
                                 WeatherConditionId, VenueId, VenueNam, TeamID, TeamName,
                                 TeamScore, winner, win, lose, draw)], 
                     by.x=c("MatchId", "ClubId2"), by.y=c("MatchId", "TeamID"), 
                     all.x=T)

# Create a match date variable
library(lubridate)
player_match_dat[, match_date:=ymd(paste(MatchYear, MatchMonth, MatchDay))]

describe(player_match_dat$match_date)

# Now that I have added ClubId2 no more missings
# player_match_dat[is.na(match_date)]
# teamgames[MatchId==3130186]
# lups_NRL[MatchId==3130186]

################################################################################

# How many matches has a player played in our data to date
playerlist <- unique(player_match_dat$PlayerId) # 637

for(i in 1:length(playerlist)){

  tmp <- player_match_dat[PlayerId==playerlist[i]]
  # Order by date
  setorder(tmp, match_date)
  tmp[, NRL_player_match_count:=1:nrow(tmp)]
  
  if(i==1){
    p <- tmp
  } else {
    p <- rbind(p, tmp)
  }
} # end for

# Now overwrite back over player_match_dat
# Check same number of rows as before
player_match_dat <- p
rm(p, tmp)

################################################################################
# When did the player first appear in our data
player_match_dat[, min(MatchYear), by=PlayerId]

################################################################################
# How long has the player been playing in that position since 2014
jump <- player_match_dat[, .(jump_start_year=min(MatchYear)), by=.(PlayerId, Jumper)]
# Players can play multiple jumoer numbers
jump[, .N, by=PlayerId] %>% .[N>1]

################################################################################
# Number of times in that position
for(i in 1:length(playerlist)){
  
  # Get a list of all of the matches played by the player
  tmp <- player_match_dat[PlayerId==playerlist[i]]
  
  # Order by date
  setorder(tmp, match_date)
  
  # Get a list of unique positions played
  list <- unique(tmp$PositionKey)
  
  for(j in 1:length(list)){
    
    tmp2 <- tmp[PositionKey==list[j]]
    tmp2[, position_count:=1:nrow(tmp2)]
    
    if(j==1){
      p <- tmp2
    } else {
      p <- rbind(p, tmp2)
    }
    
  } # end j for
  
  if(i==1){
    p2 <- p
  } else {
    p2 <- rbind(p2, p)
  }

} # end for

player_match_dat <- p2
rm(p, p2, tmp, tmp2)

################################################################################
# Time in that position since 2014
pos <- player_match_dat[, .(pos_start_date=min(match_date)), by=.(PlayerId, PositionKey, PositionName)]
pos[, .N, by=PlayerId] %>% .[N>1]


# Merge on to player_match_dat
player_match_dat <- merge(player_match_dat, pos[, .(PlayerId, PositionName, pos_start_date)], 
                          by=c("PlayerId", "PositionName"), all.x=T)

player_match_dat[, days_in_pos:=match_date-pos_start_date+1]

player_match_dat[, player_pos:=paste0(PlayerId, "-", PositionKey)]

################################################################################
# Could use the jumper number to see if players are playing out of position?
View(lineups_NRL[, .N, by=.(Jumper, PositionKey, PositionName)] %>% setorder(Jumper))
positions_NRL
unique(player_match_dat$Jumper)
# Not sure what the 14-26 and 0 jumper numbers are for going to keave this for now

# Out of position flag
# player_match_dat[, outpos:=ifelse()]


################################################################################
# Number of times the spine has played together before (since 2014)

#Loop through the matches
for(i in 1:length(matchlist)){
  
  # Get the match
  m <- player_match_dat[MatchId==matchlist[i]]
  
  # Get the spine players position keys
  spine <- c(53, 58, 59, 61)
  
  # Get the match date
  # play_date <- unique(m[ClubId2==clubs[j] & PositionKey %in% spine, match_date])
  play_date <- unique(m[, match_date])
    
  # For each club in the match
  clubs <- unique(m$ClubId2)
  
  for(j in 1:length(clubs)){
    
    # Get the spine team and their position
    spine_players <- m[ClubId2==clubs[j] & PositionKey %in% spine, .(PlayerId, PositionKey, player_pos)]
    
    # Now get the previous matches where the players have played together in those positions
    prev1 <- player_match_dat[match_date < play_date & 
                                 player_pos == spine_players[1, player_pos], .(MatchId, ClubId2, player_pos)]
    
    prev2 <- player_match_dat[match_date < play_date & 
                                 player_pos == spine_players[2, player_pos], .(MatchId, ClubId2, player_pos)]
    
    prev3 <- player_match_dat[match_date < play_date & 
                                 player_pos == spine_players[3, player_pos], .(MatchId, ClubId2, player_pos)]
    
    prev4 <- player_match_dat[match_date < play_date & 
                                 player_pos == spine_players[4, player_pos], .(MatchId, ClubId2, player_pos)]
    
    
    all <- merge(prev1, prev2, by="MatchId")
    
    all <- merge(all, prev3, by="MatchId")
    
    all <- merge(all, prev4, by="MatchId")
    
    # Get the list of match ids where they have all played together
    all <- length(all[, MatchId])
    
    if(j==1){
      
      d <- data.table(MatchId=matchlist[i], ClubId2=clubs[j], prev_spine_matches=all)
      
    } else {
      d <- rbind(d, data.table(MatchId=matchlist[i], ClubId2=clubs[j], prev_spine_matches=all))
    }
    
  } # end j for 
  
  if(i==1){
    d2 <- d
  } else {
    d2 <- rbind(d2, d)
  }
  
} # end i for 

# d2 is at the match-club level so can merge this on to team games data

teamgames <- merge(teamgames, d2, by.x=c("MatchId", "TeamID"), 
                   by.y=c("MatchId", "ClubId2"), all.x=T)

summary(teamgames$prev_spine_matches)

# Previous spine matches rate out of prev matches played as a club
teamgames[, prev_spine_match_rate:=prev_spine_matches/(NRL_club_match_count-1)]
























################################################################################
# Are MatchYear and SeasonId always the same? Yes for NRL
# matches_NRL[MatchYear!=SeasonId]
# matches_NYC[MatchYear!=SeasonId]
# matches_NSWCup[MatchYear!=SeasonId]
# matches_QLDCup[MatchYear!=SeasonId]



# Number of clubs player has played for in this level (should merge to other data as well)!!!!!
player_clubmatches_NRL <- lineups_NRL[, .N, by=.(PlayerId, ClubId)]

# No player has player for more than 1 club in NRL level
player_clubmatches[, .N, by=PlayerId] %>% .[N>1]
# This means that player_matches and player_clubmatches would be the same

######

# Merge on the matchid to get the match date then we can calc per season !!!!!!!!!!!!

# Loop through the datasets
for(i in dattypes){
  
  # Get the lineups data
  dat <- get(paste0("lineups_", i))
  
  # Get the matches data
  mdat <- get(paste0("matches_", i))
  mdat <- mdat[, .(MatchId, MatchYear, MatchMonth, MatchDay, MatchHour, MatchMinute, 
                    SeasonId, SeasonName, SeriesId, SeriesName, RoundId, RoundName, 
                    MatchNumber, VenueId, VenueNam, WeatherConditionId, WeatherConditionName)]
  
  # Merge on the match info to the lineups data
  dat <- merge(dat, mdat, by="MatchId", all.x=T)
  
  print("Check that a player can appear once per match")
  if(nrow(dat[, .N, by=.(PlayerId, MatchId)] %>% .[N>1])==0){
    print(paste(i, "Players only appear once per matchin the lineups data"))
  } else {
    print(paste(i, "Players appear more than once per match in the lineups data"))
  } # end if 
  
  # Number of matches played by the player
  play_match <- dat[, .N, by=.(PlayerId, PlayerName, PlayerFirstName, PlayerLastName, SeasonId)]
  
  # Reshape the data
  play_match <- reshape(play_match, 
          idvar=c("PlayerId", "PlayerName", "PlayerFirstName", "PlayerLastName"), 
          timevar = "SeasonId",
          direction="wide")
  
  # Rename the columns
  cols <- names(play_match)[grepl("[0-9]", names(play_match))]
  cols2 <- sub("N.", "", cols)
  setnames(play_match, cols, paste("num_match", i, cols2, sep="_"))
  
  # Maybe add in the total over the seasons!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  # Number of seasons played!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  assign(paste0("player_matches_", i), play_match, envir = .GlobalEnv)
  
  # Number of clubs player has played for in this level (should merge to other data as well)!!!!!
  play_clubmatch <- dat[, .N, by=.(PlayerId, ClubId)]
  
  print(paste("Check that no player has played for more that 1 club in the", i, "league"))
  
  if(nrow(player_clubmatches[, .N, by=PlayerId] %>% .[N>1]) == 0){
    print(paste(i, "players do not play for more than 1 club"))
  } else {
    print(paste(i, "players play for more than 1 club"))
  } # end if
  
  print("########################")
  
  rm(dat, play_match, play_clubmatch)
} # end i for

# Join the datasets
player_matches <- merge(player_matches_NRL, player_matches_NSWCup, 
                        by=c("PlayerId", "PlayerName", "PlayerFirstName", "PlayerLastName"), all=T)
              
player_matches <- merge(player_matches, player_matches_QLDCup, 
                        by=c("PlayerId", "PlayerName", "PlayerFirstName", "PlayerLastName"), all=T)   

player_matches <- merge(player_matches, player_matches_NYC, 
                        by=c("PlayerId", "PlayerName", "PlayerFirstName", "PlayerLastName"), all=T)   

player_matches[, num_match_tot:=rowSums(.SD, na.rm=T), 
               .SDcols=c("num_match_NRL", "num_match_NSWCup", "num_match_QLDCup", "num_match_NYC")]          

# Doesn't seem like the players play across leagues.  Is it because they get a different player id?!!!!

################################################################################
# Number of times played in the same position

lineups_NRL