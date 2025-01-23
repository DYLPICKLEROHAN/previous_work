library(tidyverse)
library(dplyr)
library(stringr)
library(rmarkdown)

ashes <- read_csv("C:\\Users\\rohad\\OneDrive\\Documents\\Data science\\Data Taming, modelling and Vizalization_RStudio\\a1\\a1\\ashes.csv")
#double slashes for windows directory
ashes <- read_csv("ashes.csv")
#testing something mentioned in the discussion board, neat!
ashes
#Checking the table, currently a tibble of 27 x 13
unique(ashes$team)
#need to correct variable English to be England
unique(ashes$role)
#many duplicates under alternate variable names, eg. bat, batsman, batting

#1.1
colnames(ashes)
ashes_longform <- gather(ashes, key = "innings", value = "description", "Test 1, Innings 1" : "Test 5, Innings 2")
ashes_longform
#tibble now in long form, 270 x 5
ashes_innings_first <- ashes_longform[c(4, 1, 2, 3, 5)]
knitr::kable(head(ashes_innings_first))

#tibble now in long form with subject first

#1.2
order <- str_match(ashes_innings_first$description, "Batting at number ..")
with_order <- cbind(ashes_innings_first, order)
with_order
#Order now has its own column
runs <- str_match(with_order$description, "scored ....")
runs
with_runs <- cbind(with_order, runs)
#runs now has its own column
no._of_balls <- str_match(with_runs$description, "from ....")
no._of_balls
all_columns<- cbind(with_runs, no._of_balls)
#no.  of balls now has its own column
batting_order <- str_replace_all(all_columns$order, "[^0-9.-]", "")
runs_ <- str_replace_all(all_columns$runs, "[^0-9.-]", "")
balls_ <- str_replace_all(all_columns$no._of_balls, "[^0-9.-]", "")  
#Taking numerical values from strings
order1 <- tibble(batting_order)
runs1 <- tibble(runs_)
balls1 <- tibble(balls_)
#making data frames from those values
a1_o <- cbind(ashes_innings_first, order1)
a1_o_r <- cbind(a1_o, runs1)
a1_o_r_b<- cbind(a1_o_r, balls1)
#Order same, so binding columns
a1o_r_b <- a1_o_r_b$description <- NULL
a1_o_r_b <- a1_o_r_b %>%
  mutate_all(na_if, "")
#removing description column
a1_o_r_b
#now a tibble of 270 x 7 (removed description, but unused data is still accessible in "ashes_innings_first')
  

  #_______________________________________________________#
#WAS A KEEN BEEN, SO ALTERNATIVELY:

trial <- ashes_innings_first %>%
  mutate("runs_"=str_match(description,"from ....") , "batting_order" = str_match(description, "Batting at number .."), "balls_" = str_match(description, "scored ...."))
#description string broken into appropriate columns
trial <- trial %>%
  mutate("runs_" = str_replace_all(trial$runs_, "[^0-9.-]",""), "balls_"=str_replace_all(trial$balls_, "[^0-9.-]",""), "batting_order"=str_replace_all(trial$batting_order, "[^0-9.-]",""))
trial <- mutate_all(trial, na_if, "")
#Left the description column in here, but all is right with the world
trial
  
   # _______________________________________________________#

#1.3
ashes_tibble <- as_tibble(a1_o_r_b)
ashes_tibble
#making a data frame from a1_o_r_b to set value type
ashes_tibble$batting_order <- as.factor(ashes_tibble$batting_order)
#low level ordinal, label = factor
ashes_tibble$runs_ <- as.integer(ashes_tibble$runs_)
ashes_tibble$balls_ <- as.integer(ashes_tibble$balls_)
#countable, discrete = integer
ashes_tibble
ashes_tibble$innings <- as.factor(ashes_tibble$innings)
#innings total=10, a label/name, low ordered count = factors
ashes_tibble <- rename(ashes_tibble,"player"="batter")
ashes_tibble$player <- as.character(ashes_tibble$player)
#player makes more sense as a variable name. The teams have several people that could take the position, categorical variable = character
ashes_tibble$team <- as.factor(ashes_tibble$team)
ashes_tibble$role <- as.factor(ashes_tibble$role)
#both low value labels, so factors  
ashes_tibble

#1.4
unique(ashes_tibble$player)
summary(unique(ashes_tibble$innings))
summary(unique(ashes_tibble$team))
unique(ashes_tibble$team)
unique(ashes_tibble$role)
#English to England, unify roles
ashes_corrected_ <- ashes_tibble %>%
  mutate(team = fct_recode(team, "England" = "English"))%>%
  mutate(role = fct_recode(role, "all-rounder" = "allrounder", "all-rounder"="all rounder", "batsman"="batting", "batsman"="bat", "bowler"="bowl"))
ac <- ashes_corrected_
ac
#2.1
#Histogram default below, bin of 30
ggplot(ac)+geom_histogram(aes(x=runs_, ), fill= "black", na.rm=TRUE) + 
  ggtitle("The Runs Achieved Over An Innings in the 2017/18 Ashes Series")+
  labs(x= "Scores reached", y ="Frequency")
#ac$runs_ %>%
#  unique()
#cool find -> 70 unique values excluding NA, bin of 70 width = 1 for a bar chart as below
#ggplot(ac)+geom_histogram(mapping = aes(x=runs_), na.rm=TRUE, bins=70, binwidth = 1)+
#  ggtitle("Total runs acheieved")+labs(x= "Total runs")

#2.2
summary(ac$runs_, na.rm = TRUE)
range(ac$runs_, na.rm = TRUE, finite= TRUE)
sd(ac$runs_, na.rm = TRUE)
table(ac$runs_)

#2.3
ggplot(ac, aes(x= runs_, col=team))+geom_bar()
#^this maps every players innings, we need to combine player scores across the innings
indiv_runs <-  ac%>%
  group_by(player) %>%
  summarise(team,role,runs_in_series = sum(runs_, na.rm=TRUE))%>%
  unique()
indiv_runs
unique(ac$player)
#all players accounted for
ggplot(indiv_runs, aes(x=team, fill=team))+
  geom_bar()+ggtitle("Number of Players On Each Team in the 2017/18 Ashes Series")+
  scale_y_continuous(breaks = seq(0, 20, by = 1))+
  labs(x = "Team", y= "Number of players")
#players per team^ 


          #_________________________________________________________
#What I thought question 2.3 wanted
indiv_runs %>%
  ggplot(aes(x=player, y=runs_in_series, fill=role))+
  geom_bar(stat="identity")+
  ggtitle("Individual performance over the 2017/18 Ashes series")+
  labs(x = "", y= "")+
  theme(axis.text.x= element_text(angle =-90, hjust = 0))
#score per player
          #_________________________________________________________

#3.1
ac %>%
  ggplot(aes(x=runs_, fill=team))+
  geom_histogram(show.legend = FALSE)+
  scale_y_continuous(breaks = seq(0, 30, by = 1))+
  facet_wrap(~team)+
  ggtitle("Team Batting Performance in the 2017/18 Ashes Series")+
  labs(x = "Score", y= "Frequency")

#3.2
ac %>%
  ggplot(aes(y=runs_, fill=team))+
  geom_boxplot(show.legend = FALSE)+
  facet_grid(~team)+
  ggtitle("Boxplot of Team Batting Performance over the 2017/18 Ashes Series")+
  labs(x = "Team", y="Runs over the series")

  
#3.3
#ENGLISH INDIVIDUALS 
england_players <- ac[ac$team != "Australia", ]
summary(england_players$runs_, na.rm =TRUE)
sd(england_players$runs_, na.rm = TRUE)
england_players %>%
  arrange(runs_) %>%
  view()
#England's statistics


#AUSTRALIAN INDIVIDUALS
aus_players <- ac[ac$team != "England", ]
summary(aus_players$runs_, na.rm= TRUE)
sd(aus_players$runs_, na.rm = TRUE)
aus_players %>%
  arrange(runs_) %>%
  view()


#for outliers
ggplot(ac, aes(x = team, y = runs_, fill =team)) + 
  geom_boxplot(show.legend = FALSE) +
  stat_summary(
    aes(label = round(stat(y), 1)),
    geom = "text", 
    fun.y = function(y) { o <- boxplot.stats(y)$out; if(length(o) == 0) NA else o },
    hjust = -1)+
  ggtitle("Boxplot of Team Batting Performance \nover the 2017/18 Ashes Series")+
  labs(x = "Team", y="Runs over the series")

#4.1
ggplot(ac, aes( x = runs_, y= balls_, col=team))+
  geom_point()+
  geom_smooth()+
  ggtitle("Relationship between Balls Faced and Score Reached in the 2017/18 Ashes Series")+
  labs(x = "Score reached", y="Balls")

#4.3
scoring_rate_tibble <- ac %>%
  mutate(scoring_rates = runs_/balls_)
scoring_rate_tibble
#introduced a scoring rate column
ggplot(scoring_rate_tibble, aes( x = scoring_rates, y= balls_, col=team))+
  geom_point()+
  geom_smooth()+
  ggtitle("Relationship between Balls Faced and \nScoring Rate in the 2017/18 Ashes Series")+
  labs(x = "Scoring rates (Score/balls faced)", y="Balls faced")

#5.1
ggplot(indiv_runs, aes(x=team, fill=role))+
  geom_bar()+
  ggtitle("Players per Team in the 2017/218 Ashes Series")+
  labs(x = "Team", y= "Number of players")

#5.2
con_table <-  indiv_runs %>%
  group_by(role) %>%
  summarise(team, role, player)

#keeps 27 subjects and all variables required
con_table <- con_table %>%
  count(team, role) %>%
  spread(key  = "team", value = n)
con_table
#gives a table showing the total players in each roler per team
ct <- mutate(con_table, total = sum(Australia+England))
#adds a column for row totals
contingency_table <- ct%>%
  mutate(Aus=Australia/total, Eng= England/total)
#adds a column indicating the proportion of each
contingency_table <- contingency_table %>%
  mutate(Australia = NULL, England =NULL, total=NULL)
#removes unnecessary columns to reveal the...
contingency_table

install.packages("gmodels")
library(gmodels)
CrossTable(indiv_runs$role, indiv_runs$team)

citation()
citation("tidyverse")
citation("dplyr")
citation("stringr")
citation("gmodels")
