library(tidyverse)
ashes <- read_csv("C:\\Users\\rohad\\OneDrive\\Documents\\Data science\\Data Taming, modelling and Vizalization_RStudio\\a1\\a1\\ashes.csv")
#double slashes for windows directory
ashes <- read_csv("ashes.csv")
#testing something mentioned in the discussion board, neat!
ashes
#Checking the table, currently a tibble of 27 x 13
unique(ashes$team)
#need to correct variable English to be England
unique(ashes$role)
#many duplicates under alternate variable names, eb. bat, batsman, batting

#1.1
colnames(ashes)
ashes_longform <- gather(ashes, key = "innings", value = "description", "Test 1, Innings 1" : "Test 5, Innings 2")
ashes_longform
#tibble now in long form
ashes_innings_first <- ashes_longform[c(4, 1, 2, 3, 5)]
ashes_innings_first
#tibble now in long form with subject first

#1.2
library(dplyr)
library(stringr)
library(tidyverse)
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
a1_o_r_b$description <- NULL
#removing description column
a1_o_r_b


#1.3
ashes_tibble <- as_tibble(a1_o_r_b)
ashes_tibble
#making a data frame from a1_o_r_b to set vaue types
ashes_tibble$batting_order <- as.factor(ashes_tibble$batting_order)
#low level ordinal, label = factor
ashes_tibble$runs_ <- as.integer(ashes_tibble$runs_)
ashes_tibble$balls_ <- as.integer(ashes_tibble$balls_)
#countable, but discrete = integer
ashes_tibble
ashes_tibble$innings <- as.factor(ashes_tibble$innings)
#innings total=10, label/name, low level count so values are factors
ashes_tibble <- rename(ashes_tibble,"player"="batter")
ashes_tibble$player <- as.factor(ashes_tibble$player)
#teams have a low, finite number of players, categorical variable = factor
ashes_tibble$team <- as.factor(ashes_tibble$team)
ashes_tibble$role <- as.factor(ashes_tibble$role)
#both low value labels, so factors
ashes_tibble <- ashes_tibble %>% mutate_all(na_if,"")
#set batting order empty value cells to NA

#1.4
unique(ashes_tibble$player)
summary(unique(ashes_tibble$innings))
summary(unique(ashes_tibble$team))
unique(ashes_tibble$team)
unique(ashes_tibble$role)
#English to England, unify roles
ashes_corrected_ <- ashes_tibble %>%
  mutate(team = fct_recode(team, "England" = "English"))%>%
  mutate(ashes_corrected,role = fct_recode(role, "all-rounder" = "allrounder", "all-rounder"="all rounder", "batsman"="batting", "batsman"="bat", "bowler"="bowl"))
ac <- ashes_corrected_
ac

#2.1
ac$runs_ %>%
  unique()
#70 unique values excluding NA, bin of 70 width = 1 for a bar chart
ggplot(ac)+geom_histogram(mapping = aes(x=runs_), na.rm=TRUE, bins=70, binwidth = 1)+
  ggtitle("Total runs acheieved")+labs(x= "Total runs")
#Histogram default below
ggplot(ac)+geom_histogram(aes(x=runs_, ), fill= "black", na.rm=TRUE) + 
  ggtitle("The runs achieved over an innings")+labs(x= "Scores reached", y ="Frequency")

#2.2
summary(ac$runs_, na.rm = TRUE)
range(ac$runs_, na.rm = TRUE, finite= TRUE)
sd(ac$runs_, na.rm = TRUE)
table(ac$runs_)

#2.3
ac
ggplot(ac, aes(x= runs_, col=team))+geom_bar()
#^this maps every players innings, we need to combine player scores across the innings
indiv_runs <-  ac%>%
  group_by(player) %>%
  summarise(team,role,runs_in_series = sum(runs_, na.rm=TRUE))%>%
  unique()
indiv_runs
unique(ac$player)
#all accounted for
ggplot(indiv_runs, aes(x=team, fill=team))+
    geom_bar()+ggtitle("Players per team")+
    labs(x = "Team", y= "Number of players")
#players per team^ 

#3.1
ac %>%
  ggplot(aes(x=runs_, fill=team))+
  geom_histogram(show.legend = FALSE)+
  facet_wrap(~team)+
  ggtitle("Scores of each team for the series")+
  labs(x = "Score", y= "Times that score was reached")

#3.2
ac %>%
  ggplot(aes(y=runs_, fill=team))+
  geom_boxplot(show.legend = FALSE)+
  facet_grid(~team)+
  labs(x = "Team", y="Runs in the series")+
  theme(axis.text.x= element_text(angle =-90, hjust = 0))

#3.3
england_only <- indiv_runs %>%
  filter(team =="England")
summary(england_only$runs_in_series)
sd(england_only$runs_in_series)
england_only %>%
  arrange(runs_in_series)

aus_only <- indiv_runs %>%
  filter(team =="Australia")
summary(aus_only$runs_in_series)
sd(aus_only$runs_in_series)
aus_only %>%
  arrange(runs_in_series)

#for outliers
ac %>%
  filter(team =="Australia") %>%
  summary(ac$runs_, na.rm=TRUE)

#4.1
ggplot(ac, aes( x = runs_, y= balls_, col=team))+
  geom_point()+
  geom_smooth()+
  ggtitle("The relationship between balls faced and runs obtained")+
  labs(x = "Runs", y="Balls")+
  theme(axis.text.x= element_text(angle =-90, hjust = 0))

#4.3
scoring_rate_tibble <- ac %>%
  mutate(scoring_rates = runs_/balls_)
scoring_rate_tibble

ggplot(scoring_rate_tibble, aes( x = scoring_rates, y= balls_, col=team))+
  geom_point()+
  geom_smooth()+
  ggtitle("The relationship between balls faced and scoring rates")+
  labs(x = "scoring rates (runs/balls)", y="balls")+
  theme(axis.text.x= element_text(angle =-90, hjust = 0))

#5.1
ggplot(indiv_runs, aes(x=team, fill=role))+
  geom_bar()+ggtitle("Players per team")+
  labs(x = "Team", y= "Number of players")

#5.2
con_table <-  indiv_runs%>%
  group_by(role) %>%
  summarise(team, role,player) %>%
  unique()
#keeps 27 subjects and all variables required
con_table <- con_table %>%
  count(team, role)%>%
  spread(key  = "team", value = n)
ct <- mutate(con_table, total = sum(Australia+England))
contingency_table <- ct%>%
  mutate(Aus=Australia/total, Eng= England/total)
contingency_table <- contingency_table %>%
  mutate(Australia = NULL, England =NULL, total=NULL)
contingency_table

  
  

