library(dplyr)
library(ggplot2)
library(tidyr)
data <- read.csv(file.choose(),sep=',')
matches <- read.csv(file.choose(),sep=',')
matches <- matches[,-18]
#matches
data$wickets <- as.numeric(ifelse(data$player_dismissed =="" ,"",1))

#Data Wragling
teams <- data %>% select(batting_team)%>%
  distinct()

teams <- rename(teams, team = batting_team)  

s_team <- c("KKR","RCB","CSK","KXIP","RR","DD","MI","DC","KTK","PWI","SRH","RPS","GL")
teams <- cbind(teams, s_team)

player_of_match <- matches%>% select(id,player_of_match,season) %>%
  distinct()

player_of_match <- rename(player_of_match, player=player_of_match)

Season <- data.frame(season=c(2008,2009,2010,2011,2012,2013,2014,2015,2016),T_winner=c("Rajasthan Royals","Deccan Chargers","Chennai Super Kings","Chennai Super Kings","Kolkata Knight Riders","Mumbai Indians","Kolkata Knight Riders","Mumbai Indians","Sunrisers Hyderabad"))

#Run rate is high at the end of 1st innings compared to 2nd innings
runs_o <- data%>%
  left_join(matches, by=c("match_id"="id"))%>%
  group_by(season,inning,over)%>%
  summarize(no=n(),runs =sum(total_runs))%>%
  mutate(avg=runs/no)%>%
  filter(inning!=3,inning!=4)



ggplot(runs_o,aes(x=season,y=avg,colour=over,group=over))+
  geom_line(show.legend = TRUE, size =1.25,linetype=1)+
  theme(legend.position="bottom")+
  theme(legend.direction = "horizontal") +
  facet_wrap(~inning)+
  scale_y_continuous(name="average runs per ball")+
  scale_x_discrete(name="season",limits=c(2008,2009,2010,2011,2012,2013,2014,2015,2016))+
  ggtitle("Average runs per ball by over each season and innings")

# batsmen dismissed mode
dismissal <- data%>%
  left_join(matches, by=c("match_id"="id"))%>%
  left_join(teams,by=c("batting_team"="team"))%>%
  filter(dismissal_kind!="")%>%
  group_by(season,dismissal_kind,s_team)%>%
  summarize(wickets =n())



ggplot(dismissal,aes(x=dismissal_kind,y=wickets,colour=as.factor(season),fill=as.factor(season)))+
  geom_bar(position = "stack", show.legend = TRUE, width =.6,stat="identity")+
  theme(legend.position="bottom")+
  coord_flip()+
  theme(legend.direction = "horizontal") +
  scale_y_continuous(name="wickets")+
  scale_x_discrete(name="dismissal kind")+
  ggtitle("Breakdown of dismissal type")


#Runs scored by tournament winners in each season.
batting_TW <- data%>%
  left_join(matches, by=c("match_id"="id"))%>%
  semi_join(Season, by=c("season"="season","batting_team"="T_winner"))%>%
  left_join(teams,by=c("batting_team"="team"))%>%
  group_by(season,batting_team,s_team)%>%
  summarize(runs =sum(total_runs))
## Warning: Column `batting_team`/`T_winner` joining factors with different
## levels, coercing to character vector
ggplot(batting_TW,aes(x=season,y=runs,colour=batting_team,fill=batting_team))+
  geom_bar(position = "stack",  show.legend = FALSE, width = .6,stat="identity")+
  geom_text(aes(label=s_team,hjust=-.25, colour="green"))+
  theme(legend.position="none")+
  coord_flip()+
  scale_x_discrete(name="Season", limits=c(2008,2009,2010,2011,2012,2013,2014,2015,2016))+
  ggtitle("Total Runs by Tournament winners by season")

#wickets taken by tournament winners in each season.

bowling_TW <- data%>%
  left_join(matches, by=c("match_id"="id"))%>%
  semi_join(Season, by=c("season"="season","bowling_team"="T_winner"))%>%
  left_join(teams,by=c("bowling_team"="team"))%>%
  group_by(season,bowling_team,s_team)%>%
  summarize(wicket =sum(wickets,na.rm=TRUE))
## Warning: Column `bowling_team`/`T_winner` joining factors with different
## levels, coercing to character vector
ggplot(bowling_TW,aes(x=season,y=wicket,colour=bowling_team,fill=bowling_team))+
  geom_bar(position = "stack",  show.legend = FALSE, width = .6,stat="identity")+
  geom_text(aes(label=s_team,hjust=-.25, colour="green"))+
  theme(legend.position="none")+
  coord_flip()+
  scale_x_discrete(name="Season", limits=c(2008,2009,2010,2011,2012,2013,2014,2015,2016))+
  ggtitle("Total wickets by Tournament winners by season")

#Highest run scorers in each season

bastmen<- data%>%
  left_join(matches, by=c("match_id"="id"))%>%
  group_by(season,batsman)%>%
  summarize(runs =max(sum(batsman_runs,na.rm=TRUE)))%>%
  arrange(season,desc(runs))%>%
  filter(runs==max(runs))

ggplot(bastmen,aes(x=season,y=runs,colour=batsman,fill=batsman))+
  geom_bar(position = "stack",  show.legend = FALSE, width = .6,stat="identity")+
  geom_text(aes(label=batsman,hjust=-.25, colour="green"))+
  theme(legend.position="none")+
  coord_flip()+
  scale_x_discrete(name="Season", limits=c(2008,2009,2010,2011,2012,2013,2014,2015,2016))+
  ggtitle("Highest run scorers by season")

#Highest Wicket takers in each season

bowler<- data%>%
  left_join(matches, by=c("match_id"="id"))%>%
  filter(dismissal_kind!="run out")%>%
  group_by(season,bowler)%>%
  summarize(wicket =max(sum(wickets,na.rm=TRUE)))%>%
  arrange(season,desc(wicket))%>%
  filter(wicket==max(wicket))

ggplot(bowler,aes(x=season,y=wicket,colour=bowler,fill=bowler))+
  geom_bar(position = "stack",  show.legend = FALSE, width = .6,stat="identity")+
  geom_text(aes(label=bowler,hjust=-.25, colour="green"))+
  theme(legend.position="none")+
  coord_flip()+
  scale_x_discrete(name="Season", limits=c(2008,2009,2010,2011,2012,2013,2014,2015,2016))+
  ggtitle("Highest wickter takers by season")

#Total wickets taken by each team across all seasons.

team_wickets <- data%>%
  left_join(matches, by=c("match_id"="id"))%>%
  left_join(teams,by=c("bowling_team"="team"))%>%
  group_by(season,bowling_team,s_team)%>%
  summarize(wicket =sum(wickets,na.rm=TRUE))



ggplot(team_wickets,aes(x=season,y=wicket,colour=s_team,fill=s_team))+
  geom_line(show.legend = TRUE, size =1.25,linetype=1)+
  theme(legend.position="bottom")+
  theme(legend.direction = "horizontal") +
  scale_y_continuous(limits = c(60,125))+
  scale_x_discrete(name="Season", limits=c(2008,2009,2010,2011,2012,2013,2014,2015,2016))+
  ggtitle("Total wickets by teams by season")

#Runs scored by each team in 1s, 2s, 3s, 4s,and 6s.

runs_cat <- data %>%
  left_join(matches,by=c("match_id"="id"))%>%
  left_join(teams,by=c("batting_team"="team"))%>%
  group_by(s_team,batsman_runs)%>%
  summarize(no=n(),runs=sum(total_runs))

runs_cat$batsman_runs <- as.factor(runs_cat$batsman_runs)

ggplot(runs_cat,aes(x=s_team,y=runs,colour=batsman_runs,fill=batsman_runs))+
  geom_bar(position = "stack", show.legend = TRUE, width =.6,stat="identity")+
  theme(legend.position="bottom")+
  theme(legend.direction = "horizontal") +
  scale_y_continuous(name="Runs")+
  scale_x_discrete(name="Teams")+
  ggtitle("Total runs scored in 1s to 6s")

#Strike rate of all batsman

Bat_sr<- data %>%
  left_join(matches,by=c("match_id"="id"))%>%
  left_join(teams,by=c("batting_team"="team"))%>%
  group_by(batsman)%>%
  summarize(balls=n(),runs=sum(batsman_runs))%>%
  mutate(sr=runs*100/balls)%>%
  arrange(desc(sr))%>%
  mutate(sr_grp=ifelse(sr<100,"100",ifelse(sr<150,"100-150","150+")))%>%
  mutate(player_lab=ifelse(batsman=="AD Russell","AD Russell",ifelse(batsman=="V Sehwag","V Sehwag",ifelse(batsman=="V Kohli","V Kohli",ifelse(batsman=="CH Gayle","CH Gayle","")))))



ggplot(Bat_sr,aes(x=sr,y=runs,colour=sr_grp,fill=sr_grp,size=runs))+
  geom_jitter(show.legend = TRUE,alpha=.75)+
  theme(legend.position="bottom")+
  theme(legend.direction = "horizontal") +
  geom_text(aes(label=player_lab,hjust=-.25, colour="red"))+
  scale_y_continuous(name="Runs")+
  scale_x_continuous(name="strike rate")+
  ggtitle("strike rate for batsman   ")

#Number of Toss and Match wins by each team

toss <- matches%>%
  left_join(teams,by=c("toss_winner"="team") )%>%
  select(s_team,toss_winner)%>%
  group_by(s_team)%>%
  summarize(wins=n())

toss$type <- "toss"

wins <-matches%>%
  left_join(teams,by=c("winner"="team") )%>%
  select(s_team,winner)%>%
  group_by(s_team)%>%
  summarize(wins=n())
## Warning: Column `winner`/`team` joining factors with different levels,
## coercing to character vector
wins$type <- "wins"

toss_w <- rbind(toss,wins)

toss_w <- toss_w %>%
  group_by(s_team, type)%>%
  summarize(wins=sum(wins))


ggplot(toss_w,aes(x=s_team,y=wins,colour=type,fill=type))+
  geom_bar(position = "dodge",stat = "identity")+
  theme(legend.position="right")+
  scale_y_continuous(name="Toss and Match Wins")+
  scale_x_discrete(name="Toss and Match winner")+
  ggtitle("Toss and Match wins by each Team")

#Toss decisions of toss winners

wins_1 <- matches%>%
  left_join(teams,by=c("toss_winner"="team") )%>%
  select(s_team,toss_winner,toss_decision)%>%
  group_by(s_team,toss_decision)%>%
  summarize(wins=n())


ggplot(wins_1,aes(x=s_team,y=wins,colour=toss_decision,fill=toss_decision))+
  geom_bar(position = "dodge",stat = "identity")+
  theme(legend.position="right")+
  scale_y_continuous(name="Toss decision")+
  scale_x_discrete(name="Toss winners and toss decisions")+
  ggtitle("Toss decisions by each Team")

