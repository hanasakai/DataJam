################################################################################
# DATA JAM
# djam_dataprep.r
# Hana Sakai
# April 2017
################################################################################

source("E:\\Career\\PersonalProjects\\DataJam\\code\\djam_functions.r")

load("djam_data_new.Rd")
load("djam_data_nrl.Rd")

# dattypes <- c("NRL", "NYC", "NSWCup", "QLDCup")

################################################################################
# TEAM-MATCH --------------------------------------------------------------

## Create some game outcomes for each team

# Create a dataset with one row for each team-match

# Create a new variable for the winner
matches_NRL[, winner:=ifelse(TeamAScore > TeamBScore, "A", 
                             ifelse(TeamBScore > TeamAScore, "B", 
                                    ifelse(TeamAScore == TeamBScore, "D", NA)))]

# Calculate points difference as well
matches_NRL[, points_diff:=abs(TeamAScore-TeamBScore)]

describe(matches_NRL[, .(winner, points_diff)])

# Team A matches
# colnames <- names(matches_NRL)[!names(matches_NRL) %in% c("TeamBId", "TeamBName", "TeamBScore")]
colnames <- names(matches_NRL)[!names(matches_NRL) %in% c("TeamBId", "TeamBName")]

teama <- matches_NRL[, colnames, with=F]

# Update the cloumn names
setnames(teama, c("TeamAId", "TeamAName", "TeamAScore", "TeamBScore"), 
         c("TeamID", "TeamName", "TeamScore", "PointsAgainst"))

# Update the winner column
teama[, win:=ifelse(winner=="A", 1, 0)]
teama[, lose:=ifelse(winner=="B", 0, 1)]
teama[, draw:=ifelse(winner=="D", 1, 0)]

# Create an indicator for home game or not
# teama[, home:=1]

################################################################################
# Team B matches
# colnames <- names(matches_NRL)[!names(matches_NRL) %in% c("TeamAId", "TeamAName", "TeamAScore")]
colnames <- names(matches_NRL)[!names(matches_NRL) %in% c("TeamAId", "TeamAName")]

teamb <- matches_NRL[, colnames, with=F]

# Update the cloumn names
setnames(teamb, c("TeamBId", "TeamBName", "TeamBScore", "TeamAScore"), 
         c("TeamID", "TeamName", "TeamScore", "PointsAgainst"))

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

# Create a match_date var
teamgames[, match_date:=ymd(paste(MatchYear, MatchMonth, MatchDay))]

# describe(teamgames$match_date)
teamgames[, points_diff2:=ifelse(lose==1, -points_diff, points_diff)]

################################################################################
# How many matches has the club played since 2014
teamlist <- sort(unique(teamgames[, TeamID]))

for(i in 1:length(teamlist)){
  
  tmp <- teamgames[TeamID==teamlist[i]]
  
  # Order by date
  setorder(tmp, match_date)
  tmp[, NRL_club_match_count_prev:=0:(nrow(tmp)-1)]
  
  if(i==1){
    p <- tmp
  } else {
    p <- rbind(p, tmp)
  }
  
}

teamgames <- p
rm(p)

################################################################################

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Merge on geo coordinates for venue and team home stadium
# club_ prefix denotes the home venue location
teamgames <- merge(teamgames, 
                   clubgeo[league_name=="National Rugby League", 
                           .(club_name, club_venue_name, club_venue_address, 
                             club_venue_lat, club_venue_lon)], 
                   by.x="TeamName", by.y="club_name", all.x=T)

# Now have venues_NRL geocoded
teamgames <- merge(teamgames, venues_NRL[, .(VenueId, venue_lat, venue_lon)], by="VenueId")

teamgames[, home:=ifelse(club_venue_lat==venue_lat & club_venue_lon==venue_lon, 1, 0)]

summary(teamgames[, home])
teamgames[is.na(home)]

# 27 missings

################################################################################
# PLAYER-MATCH LEVEL ------------------------------------------------------------

# Number of matches played by each player
# Check that a player can appear once per match
# lineups_NRL[, .N, by=.(PlayerId, MatchId)] %>% .[N>1]

# Check how many clubs can play in a match
# More than one because the lineups data has been merged badly
# tmp <- lineups_NRL[, .N, by=.(MatchId, ClubId)]
# tmp2 <- tmp[, .N, by=.(MatchId)]
# setorder(tmp2, -N)
# tmp2
# tmp2[MatchId==2630001]

# Check a match
# matches_NRL[MatchId==2630001]
# 
# # There are players from more than 2 clubs playing in the match
# lineups_NRL[MatchId==2630001]
# 
# lineups_NRL[MatchId==2630001 & ClubId!=500001 & ClubId!=500005]
# 
# lineups_NRL[MatchId==2630001, .N, by=ClubId]

# FIX THE DATA
# So in the lineups the 1st 17 rows relate to the lowest club number for the match
# Add a column to the lineups for the ClubId playing for calling the new data lups_NRL
matchlist <- sort(unique(lineups_NRL[, MatchId]))

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
                                        TeamScore, winner, win, lose, draw, points_diff, home, match_date)], 
                          by.x=c("MatchId", "ClubId2"), by.y=c("MatchId", "TeamID"), 
                          all.x=T)

# Now that I have added ClubId2 no more missings
# player_match_dat[is.na(match_date)]
# teamgames[MatchId==3130186]
# lups_NRL[MatchId==3130186]

################################################################################
# Now I have a player - match level dataset calculate some stats
# How many matches has a player played in our data to date
playerlist <- unique(player_match_dat$PlayerId) # 637
length(playerlist)

for(i in 1:length(playerlist)){
  
  tmp <- player_match_dat[PlayerId==playerlist[i]]
  # Order by date
  setorder(tmp, match_date)
  tmp[, NRL_player_match_count_prev:=0:(nrow(tmp)-1)]
  
  if(i==1){
    p <- tmp
  } else {
    p <- rbind(p, tmp)
  }
} # end for

# Now overwrite back over player_match_dat
# Check same number of rows as before
dim(player_match_dat)
dim(p)
player_match_dat <- p
rm(p, tmp)

################################################################################
# When did the player first appear in our data
tmp <- player_match_dat[, .(player_start_date=min(match_date)), by=PlayerId]

player_match_dat <- merge(player_match_dat, tmp, by="PlayerId", all.x=T)

################################################################################
# jump <- player_match_dat[, .(jump_start_year=min(MatchYear)), by=.(PlayerId, Jumper)]
# # Players can play multiple jumoer numbers
# jump[, .N, by=PlayerId] %>% .[N>1]

# Number of times in that jumper since 2014
for(i in 1:length(playerlist)){
  
  # Get a list of all of the matches played by the player
  tmp <- player_match_dat[PlayerId==playerlist[i]]
  
  # Order by date
  setorder(tmp, match_date)
  
  # Get a list of unique positions played
  list <- unique(tmp$Jumper)
  
  for(j in 1:length(list)){
    
    tmp2 <- tmp[Jumper==list[j]]
    tmp2[, jumper_count_prev:=0:(nrow(tmp2)-1)]
    
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
    tmp2[, position_count_prev:=0:(nrow(tmp2)-1)]
    
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

# Number of days player has played in that position since 2014
player_match_dat[, days_in_pos:=match_date-pos_start_date]

# Days since 2014 season start
player_match_dat[, days_since_data_start:=match_date-min(player_match_dat$match_date)]

player_match_dat[, days_in_pos_prop:=as.numeric(days_in_pos)/as.numeric(days_since_data_start)]

summary(player_match_dat$days_in_pos_prop)

# Create a unique id for the player dat data
player_match_dat[, player_pos_id:=paste0(PlayerId, "-", PositionKey)]

################################################################################
# Could use the jumper number to see if players are playing out of position?
# View(lineups_NRL[, .N, by=.(Jumper, PositionKey, PositionName)] %>% setorder(Jumper))
# positions_NRL
# unique(player_match_dat$Jumper)
# Not sure what the 14-26 and 0 jumper numbers are for going to keave this for now

# Out of position flag
# player_match_dat[, outpos:=ifelse()]




################################################################################
# AGGREGATE PLAYER-MATCH to TEAM-MATCH ------------------------------------

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
    spine_players <- m[ClubId2==clubs[j] & PositionKey %in% spine, .(PlayerId, PositionKey, player_pos_id)]
    
    # Now get the previous matches where the players have played together in those positions
    prev1 <- player_match_dat[match_date < play_date & 
                                player_pos_id == spine_players[1, player_pos_id], .(MatchId, ClubId2, player_pos_id)]
    
    prev2 <- player_match_dat[match_date < play_date & 
                                player_pos_id == spine_players[2, player_pos_id], .(MatchId, ClubId2, player_pos_id)]
    
    prev3 <- player_match_dat[match_date < play_date & 
                                player_pos_id == spine_players[3, player_pos_id], .(MatchId, ClubId2, player_pos_id)]
    
    prev4 <- player_match_dat[match_date < play_date & 
                                player_pos_id == spine_players[4, player_pos_id], .(MatchId, ClubId2, player_pos_id)]
    
    
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
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000   2.000   4.672   6.000  36.000 

# Previous spine matches rate out of prev matches played as a club
teamgames[, prev_spine_match_rate:=prev_spine_matches/(NRL_club_match_count_prev)]

################################################################################
# Number of times the FORWARDS has played together before (since 2014)

#Loop through the matches
for(i in 1:length(matchlist)){
  
  # Get the match
  m <- player_match_dat[MatchId==matchlist[i]]
  
  # Get the forwards players position keys
  forwards <- c(60, 61, 62)
  
  # Get the match date
  play_date <- unique(m[, match_date])
  
  # For each club in the match
  clubs <- unique(m$ClubId2)
  
  for(j in 1:length(clubs)){
    
    # Get the forwards team and their position
    for_players <- m[ClubId2==clubs[j] & PositionKey %in% forwards, .(PlayerId, PositionKey, player_pos_id)]
    
    # Now get the previous matches where the players have played together in those positions
    prev1 <- player_match_dat[match_date < play_date & 
                                player_pos_id == for_players[1, player_pos_id], .(MatchId, ClubId2, player_pos_id)]
    
    prev2 <- player_match_dat[match_date < play_date & 
                                player_pos_id == for_players[2, player_pos_id], .(MatchId, ClubId2, player_pos_id)]
    
    prev3 <- player_match_dat[match_date < play_date & 
                                player_pos_id == for_players[3, player_pos_id], .(MatchId, ClubId2, player_pos_id)]
    
    
    all <- merge(prev1, prev2, by="MatchId")
    
    all <- merge(all, prev3, by="MatchId")
    
    # Get the list of match ids where they have all played together
    all <- length(all[, MatchId])
    
    if(j==1){
      
      d <- data.table(MatchId=matchlist[i], ClubId2=clubs[j], prev_forward_matches=all)
      
    } else {
      d <- rbind(d, data.table(MatchId=matchlist[i], ClubId2=clubs[j], prev_forward_matches=all))
    }
    
  } # end j for 
  
  if(i==1){
    d2 <- d
  } else {
    d2 <- rbind(d2, d)
  }
  
} # end i for 

summary(d2$prev_forward_matches)

# d2 is at the match-club level so can merge this on to team games data

teamgames <- merge(teamgames, d2, by.x=c("MatchId", "TeamID"), 
                   by.y=c("MatchId", "ClubId2"), all.x=T)

# Previous spine matches rate out of prev matches played as a club
teamgames[, prev_forward_match_rate:=prev_forward_matches/(NRL_club_match_count_prev)]

summary(teamgames[, .(prev_forward_matches, prev_forward_match_rate)])


################################################################################
# Number of times the middles have played together before (since 2014)

#Loop through the matches
for(i in 1:length(matchlist)){
  
  # Get the match
  m <- player_match_dat[MatchId==matchlist[i]]
  
  # Get the middles players position keys
  middles <- c(55, 59, 58, 56)
  
  # Get the match date
  play_date <- unique(m[, match_date])
  
  # For each club in the match
  clubs <- unique(m$ClubId2)
  
  for(j in 1:length(clubs)){
    
    # Get the spine team and their position
    mid_players <- m[ClubId2==clubs[j] & PositionKey %in% middles, .(PlayerId, PositionKey, player_pos_id)]
    
    # Now get the previous matches where the players have played together in those positions
    prev1 <- player_match_dat[match_date < play_date & 
                                player_pos_id == mid_players[1, player_pos_id], .(MatchId, ClubId2, player_pos_id)]
    
    prev2 <- player_match_dat[match_date < play_date & 
                                player_pos_id == mid_players[2, player_pos_id], .(MatchId, ClubId2, player_pos_id)]
    
    prev3 <- player_match_dat[match_date < play_date & 
                                player_pos_id == mid_players[3, player_pos_id], .(MatchId, ClubId2, player_pos_id)]
    
    prev4 <- player_match_dat[match_date < play_date & 
                                player_pos_id == mid_players[4, player_pos_id], .(MatchId, ClubId2, player_pos_id)]
    
    
    all <- merge(prev1, prev2, by="MatchId")
    
    all <- merge(all, prev3, by="MatchId")
    
    all <- merge(all, prev4, by="MatchId")
    
    # Get the list of match ids where they have all played together
    all <- length(all[, MatchId])
    
    if(j==1){
      
      d <- data.table(MatchId=matchlist[i], ClubId2=clubs[j], prev_middle_matches=all)
      
    } else {
      d <- rbind(d, data.table(MatchId=matchlist[i], ClubId2=clubs[j], prev_middle_matches=all))
    }
    
  } # end j for 
  
  if(i==1){
    d2 <- d
  } else {
    d2 <- rbind(d2, d)
  }
  
} # end i for 

summary(d2$prev_middle_matches)
# d2 is at the match-club level so can merge this on to team games data

teamgames <- merge(teamgames, d2, by.x=c("MatchId", "TeamID"), 
                   by.y=c("MatchId", "ClubId2"), all.x=T)

summary(teamgames$prev_middle_matches)

# Previous spine matches rate out of prev matches played as a club
teamgames[, prev_middle_match_rate:=prev_middle_matches/(NRL_club_match_count_prev)]

################################################################################
# Aggregate player days_in_pos_prop to the team - match level

tmp <- player_match_dat[, .(avg_days_in_pos_prop=mean(days_in_pos_prop, na.rm=T)), by=.(MatchId, ClubId2)]

teamgames <- merge(teamgames, tmp, by.x=c("MatchId", "TeamID"), 
                   by.y=c("MatchId", "ClubId2"), all.x=T)

summary(teamgames$avg_days_in_pos_prop)

# teamgames[, avg_days_in_pos_prop:=NULL ]
# teamgames[is.na(avg_days_in_pos_prop) ]
# Missings for the first match

###############################################################################

# TRX AGGREGATION ---------------------------------------------------------

# trx_NRL[EventName=="Tackled - missed", .N,by=.(EventCode, EventName)]

trx_agg <- ftrx_agg(trx_NRL)
# Try assist
# Linebreak assist by pass

# Merge trx_agg to the teamgames datasets
teamgames2 <- merge(teamgames, trx_agg, by.x=c("MatchId", "TeamID"), by.y=c("MatchId", "ClubId"), all.x=T)

################################################################################
# Calculate stats by team for the spine players only
trx_NRL_spine <- trx_NRL[PositionId %in% spine & RoundId<=26]

trx_agg_spine <- ftrx_agg(trx_NRL_spine)

trx_agg_spine[, ":=" ("try_ass_rank" = ifelse(try_assist>3, 4, ifelse(try_assist>2, 3, ifelse(try_assist>1, 2, 1))), 
                      "receive_rank" = ifelse(receives>204, 4, ifelse(receives>183, 3, ifelse(receives>159, 2, 1))),
                      "line_breaks_ass_rank" = ifelse(linebreak_assist>3, 4, ifelse(linebreak_assist>2, 3, ifelse(linebreak_assist>1, 2, 1))), 
                      "break_cause_rank" = ifelse(breakcause>4, -4, ifelse(breakcause>2, -3, ifelse(breakcause>1, -2, -1))),
                      "missed_tackles_rank" = ifelse(missed_tackles>19, -4, ifelse(missed_tackles>15, -3, ifelse(missed_tackles>12, -2, -1))), 
                      "line_not_engage_rank" = ifelse(line_not_engage>4, -4, ifelse(line_not_engage>3, -3, ifelse(line_not_engage>1, -2, -1))))]

# Should I put a weights in 
# trx_agg_spine[, try_ass_rank:=try_ass_rank*]


trx_agg_spine[, score:=rowSums(trx_agg_spine[, c("try_ass_rank", "receive_rank", "line_breaks_ass_rank", 
                                                 "break_cause_rank", "missed_tackles_rank", "line_not_engage_rank")], na.rm=T)]

setorder(trx_agg_spine, -score)

summary(trx_agg_spine)




# Merge on the spine score
teamgames3 <- merge(teamgames, trx_agg_spine, by.x=c("MatchId", "TeamID"), by.y=c("MatchId", "ClubId"), all.x=T)

setorder(teamgames3, -score)

top <- teamgames3[, .(TeamID, TeamName, MatchYear, prev_spine_match_rate, 
                      try_ass_rank, receive_rank, line_breaks_ass_rank, 
                      break_cause_rank, missed_tackles_rank, line_not_engage_rank, score, win)]


top2 <- top[, .(score=sum(score, na.rm=T), cohesion=mean(prev_spine_match_rate, na.rm=T)), by=.(TeamID, TeamName, MatchYear)]

View(top2[MatchYear==2014])


top2014 <- top[MatchYear %in% c(2014), .(score=mean(score, na.rm=T), cohesion=mean(prev_spine_match_rate, na.rm=T)), by=.(TeamID, TeamName)]

top2015 <- top[MatchYear %in% c(2014, 2015), .(score=mean(score, na.rm=T), cohesion=mean(prev_spine_match_rate, na.rm=T)), by=.(TeamID, TeamName)]

top2016 <- top[MatchYear %in% c(2014, 2015, 2016), .(score=mean(score, na.rm=T), cohesion=mean(prev_spine_match_rate, na.rm=T)), by=.(TeamID, TeamName)]


View(top2014)
View(top2015)
View(top2016)
View(top2[MatchYear==2016])

temgames4 <- copy(top)
setorder(teamgames4, -score)
# rank2014 <- teamgames3[MatchYear==2014]
# 
# rank2016 <- teamgames3[MatchYear==2016]





################################################################################
# SAVE --------------------------------------------------------------------

save(teamgames, player_match_dat, file="djam_data_analysis.Rd")

fwrite(teamgames, "djam_teamgames.csv")

fwrite(player_match_dat, "djam_player_match_dat.csv")
