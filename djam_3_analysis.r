################################################################################
# DATA JAM
# djam_3_analysis.r
# Hana Sakai
# April 2017
################################################################################

source("E:\\Career\\PersonalProjects\\DataJam\\code\\djam_functions.r")

load("djam_data_new.Rd")
load("djam_data_nrl.Rd")
load("djam_data_analysis.Rd")

################################################################################
# MODEL 1: ONLY SPINE RATE ------------------------------------------------


teamgames[, .(prev_spine_match_rate, win)]

m1 <- glm(win~prev_spine_match_rate+I(prev_spine_match_rate^2), family=binomial, 
    data=teamgames[, .(prev_spine_match_rate, win)])

summary(m1)
windows()
par(mfrow=c(2,2))
plot(m1)

teamgames[TeamID==500001 & SeasonId==2014]

tmp <- teamgames[, .(wins=sum(win), win_rate=sum(win)/.N, N=.N, avg_spine_rate=mean(prev_spine_match_rate)), 
          by=.(TeamID, TeamName, SeasonId)]

tmp <- tmp[SeasonId>2014]

library(ggplot2)

ggplot(tmp) + 
  geom_point(aes(x=avg_spine_rate, y=win_rate, colour=as.factor(TeamName))) + 
  theme(legend.position = "bottom") +
  ylab("Win Rate") + xlab("Avg proportion of times Spine Players have played together before")

describe(tmp)
tmp[avg_spine_rate>0.3]

player_match_dat[ClubId2==500021 & MatchYear==2016 & (PositionKey==53 | PositionKey==58 | PositionKey ==59 | PositionKey ==61), .N, by=PlayerName]


tmp[TeamID==500013]
################################################################################

# ADD MORE VARS TO MODEL --------------------------------------------------

# Tested adding in home but don't think data is right
# NRL_club_match_count_prev was not sig
m2 <- glm(win~prev_spine_match_rate+I(prev_spine_match_rate^2)+avg_days_in_pos_prop+
            prev_forward_match_rate+I(prev_forward_match_rate^2)+RoundId+SeasonId+
            prev_middle_match_rate, 
          family=binomial, 
          data=teamgames2)

summary(m2)
windows()
par(mfrow=c(2,2))
plot(m1)

################################################################################
# Try different y var - points diff
m3 <- glm(points_diff2~prev_spine_match_rate+I(prev_spine_match_rate^2)+avg_days_in_pos_prop+
            prev_forward_match_rate+I(prev_forward_match_rate^2), 
          family=gaussian, 
          data=teamgames[, .(avg_days_in_pos_prop, prev_spine_match_rate, 
                             prev_forward_match_rate, win, points_diff2)])

summary(m3)
windows()
par(mfrow=c(2,2))
plot(m1)

tmp3 <- teamgames[, .(points_diff2=mean(points_diff2), N=.N, avg_spine_rate=mean(prev_spine_match_rate)), 
                  by=.(TeamID, TeamName, SeasonId)]

tmp3 <- tmp3[SeasonId>2014]

ggplot(tmp3) + 
  geom_point(aes(x=avg_spine_rate, y=points_diff2, colour=as.factor(TeamName))) + 
  theme(legend.position = "bottom") +
  ylab("Mean Points Diff") + xlab("Avg proportion of times Spine Players have played together before")




# Rolling average, match rate over time to see if it improves?
################################################################################

m3 <- glm(win~receives + linebreak_assist + line_engage + supports + 
            line_not_engage + kick_error + hand_error + breakcause * missed_tackles, 
          family=binomial, 
          data=teamgames2)

m3 <- glm(win~receives + linebreak_assist + try_assist + line_engage + supports + 
            line_not_engage + kick_error + hand_error + breakcause * missed_tackles, 
          family=binomial, 
          data=teamgames2)

summary(m3)


################################################################################

m4 <- glm(win~receives + linebreak_assist + try_assist + line_engage + supports + 
            line_not_engage + kick_error + hand_error + breakcause * missed_tackles, 
          family=binomial, 
          data=teamgames3)

summary(m4)

teamgames3
m4 <- glm(win~prev_spine_match_rate * score, 
          family=binomial, 
          data=top)
