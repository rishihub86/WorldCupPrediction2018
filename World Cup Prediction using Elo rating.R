#Data Preparation and Analysis for prediction using ELO
library(dplyr)
library(elo)
library(ggplot2)

all_matches <- readr::read_csv('C:\\Users\\rishihub\\Desktop\\WC\\results.csv')
all_matches %>%
  dplyr::select(date, home_team, away_team, home_score, away_score) %>%
  mutate(total_goals = home_score + away_score) %>%
  arrange(-total_goals) %>%
  head(1)

all_teams <- data.frame(team = unique(c(all_matches$home_team, all_matches$away_team)))

all_teams <- all_teams %>%
  mutate(elo = 1800)

all_matches <- all_matches %>%
  mutate(result = if_else(home_score > away_score, 1,
                          if_else(home_score == away_score, 0.5, 0)))

qplot(all_matches$country, data=all_matches, geom="density", fill=result, alpha=I(.5), 
      main="Team Winning Ratio", xlab="Teams", 
      ylab="Result per elo rating")

qplot(all_matches$result, all_matches$country, data=all_matches, geom=c("boxplot", "jitter"), 
      fill=result, main="Distribution of results based on all_matches",
      xlab="", ylab="Result per elo rating")


# Updation and Prediction of Fifa World Cup 2018 using elo calculation
for (i in seq_len(nrow(all_matches))) {
  match <- all_matches[i, ]
  
  # Fetch pre-match details
  elo_A <- subset(all_teams, team == match$home_team)$elo
  elo_B <- subset(all_teams, team == match$away_team)$elo
  
  # Updating our rating with k=30
  rev_elo <- elo.calc(wins.A = match$result, elo.A = elo_A,elo.B = elo_B, k = 30)
  
  # Updating our rating with k=50
  #rev_elo_50 <- elo.calc(wins.A = match$result, elo.A = elo_A,elo.B = elo_B, k = 50)
  
  # Updating our rating with k=50
  #rev_elo_100 <- elo.calc(wins.A = match$result, elo.A = elo_A,elo.B = elo_B, k = 100)
  
  # Updating our rating with k=50
  #rev_elo_200 <- elo.calc(wins.A = match$result, elo.A = elo_A,elo.B = elo_B, k = 200)
  
  # Result data
  eloA_new <- rev_elo[1, 1]
  eloB_new <- rev_elo[1, 2]
  
  # Updating the final rating to old data frame
  all_teams <- all_teams %>%
    mutate(elo = if_else(team == match$home_team, eloA_new,
                         if_else(team == match$away_team, eloB_new, elo)))
}

all_teams %>% arrange(-elo) %>% head

WC_teams <- all_teams %>%
  filter(team %in% c("Russia", "Germany", "Brazil", "Portugal", "Argentina", "Belgium",
                     "Poland", "France", "Spain", "Peru", "Switzerland", "England",
                     "Colombia", "Mexico", "Uruguay", "Croatia", "Denmark", "Iceland",
                     "Costa Rica", "Sweden", "Tunisia", "Egypt", "Senegal", "Iran",
                     "Serbia", "Nigeria", "Australia", "Japan", "Morocco", "Panama",
                     "Korea Republic", "Saudi Arabia")) %>%
  arrange(-elo)


print.data.frame(WC_teams)

#Predicting Knockout for WC 2018 using ELO probablities

#KnockOut 1
argKO <- subset(WC_teams, team == "Argentina")$elo
franceKO <- subset(WC_teams, team == "France")$elo
elo.prob(argKO, franceKO)
barplot(table(sample(1:2, size=100, replace=TRUE, prob=c(0.42852,1-0.42852))))
#Since probablity of argentina winning is 42% --> Winner France

#KnockOut 2
uruKO <- subset(WC_teams, team == "Uruguay")$elo
portKO <- subset(WC_teams, team == "Portugal")$elo
elo.prob(uruKO, portKO)
barplot(table(sample(1:2, size=100, replace=TRUE, prob=c(0.4309815,1-0.4309815))))
#Since probablity of uruguay winning is 43% --> Winner Portugal

#KnockOut 3
spainKO <- subset(WC_teams, team == "Spain")$elo
russiaKO <- subset(WC_teams, team == "Russia")$elo
elo.prob(spainKO, russiaKO)
barplot(table(sample(1:2, size=100, replace=TRUE, prob=c(0.7980975,1-0.7980975))))
#Since probablity of spain winning is 79.8% --> Winner Spain

#KnockOut 4
croKO <- subset(WC_teams, team == "Croatia")$elo
denKO <- subset(WC_teams, team == "Denmark")$elo
elo.prob(croKO, denKO)
barplot(table(sample(1:2, size=100, replace=TRUE, prob=c(0.5866103,1-0.5866103))))
#Since probablity of croatia winning is 58.7% --> Winner Croatia

#KnockOut 5
braKO <- subset(WC_teams, team == "Brazil")$elo
mexKO <- subset(WC_teams, team == "Mexico")$elo
elo.prob(braKO, mexKO)
barplot(table(sample(1:2, size=100, replace=TRUE, prob=c(0.7703422,1-0.7703422))))
#Since probablity of brazil winning is 77% --> Winner Brazil

#KnockOut 6
belKO <- subset(WC_teams, team == "Belgium")$elo
japKO <- subset(WC_teams, team == "Japan")$elo
elo.prob(belKO, japKO)
barplot(table(sample(1:2, size=100, replace=TRUE, prob=c(0.7564017,1-0.7564017))))
#Since probablity of belgium winning is 75.6% --> Winner Belgium

#KnockOut 7
sweKO <- subset(WC_teams, team == "Sweden")$elo
swissKO <- subset(WC_teams, team == "Switzerland")$elo
elo.prob(sweKO, swissKO)
barplot(table(sample(1:2, size=100, replace=TRUE, prob=c(0.3807853,1-0.3807853))))
#Since probablity of Sweden winning is 38% --> Winner Switzerland

#KnockOut 8
colKO <- subset(WC_teams, team == "Colombia")$elo
engKO <- subset(WC_teams, team == "England")$elo
elo.prob(colKO, engKO)
barplot(table(sample(1:2, size=100, replace=TRUE, prob=c(0.4207901,1-0.4207901))))
#Since probablity of colombia winning is 42% --> Winner England

# As per elo prob the below schedule stands for Quater Final
#France vs Portugal
#Spain vs Croatia
#Brazil vs Belgium
#Switzerland vs England

# Now lets predict Quater finals based on elo prob

#Quater Final 1
francQ <- subset(WC_teams, team == "France")$elo
porQ <- subset(WC_teams, team == "Portugal")$elo
elo.prob(francQ, porQ)
barplot(table(sample(1:2, size=100, replace=TRUE, prob=c(0.5791265,1-0.5791265))))
#Since probablity of france winning is 58% --> Winner France

#Quater Final 2
spainQ <- subset(WC_teams, team == "Spain")$elo
croQ <- subset(WC_teams, team == "Croatia")$elo
elo.prob(spainQ, croQ)
barplot(table(sample(1:2, size=100, replace=TRUE, prob=c(0.6560635,1-0.6560635))))
#Since probablity of spain winning is 65% --> Winner Spain

#Quater Final 3
braQ <- subset(WC_teams, team == "Brazil")$elo
belQ <- subset(WC_teams, team == "Belgium")$elo
elo.prob(braQ, belQ)
barplot(table(sample(1:2, size=100, replace=TRUE, prob=c(0.6537536,1-0.6537536))))
#Since probablity of brazil winning is 65% --> Winner Brazil

#Quater Final 4
swissQ <- subset(WC_teams, team == "Switzerland")$elo
engQ <- subset(WC_teams, team == "England")$elo
elo.prob(swissQ, engQ)
barplot(table(sample(1:2, size=100, replace=TRUE, prob=c(0.4108184,1-0.4108184))))
#Since probablity of swiss winning is 41% --> Winner England

# As per elo prob the below schedule stands for Semi Final
#France vs Spain
#Brazil vs England

#Semi Final 1
fraSF <- subset(WC_teams, team == "France")$elo
spainSF <- subset(WC_teams, team == "Spain")$elo
elo.prob(fraSF, spainSF)
barplot(table(sample(1:2, size=100, replace=TRUE, prob=c(0.4754294,1-0.4754294))))
#Since probablity of france winning is 47.5% --> Winner Spain

#Semi Final 2
braSF <- subset(WC_teams, team == "Brazil")$elo
engSF <- subset(WC_teams, team == "England")$elo
elo.prob(braSF, engSF)
barplot(table(sample(1:2, size=100, replace=TRUE, prob=c(0.6752963,1-0.6752963))))
#Since probablity of brazil winning is 67% --> Winner Brazil

# For third place the game will be France vs England
#Third Place
fraTP <- subset(WC_teams, team == "France")$elo
engTP <- subset(WC_teams, team == "England")$elo
elo.prob(fraTP, engTP)
barplot(table(sample(1:2, size=100, replace=TRUE, prob=c(0.5518395,1-0.5518395))))
#Since probablity of France winning is 55% --> Winner France

# For Final the game will be Brazil vs Spain
#Final
spaF <- subset(WC_teams, team == "Spain")$elo
braF <- subset(WC_teams, team == "Brazil")$elo
elo.prob(spaF, braF)
barplot(table(sample(1:2, size=100, replace=TRUE, prob=c(0.3951366,1-0.3951366))))
#Since probablity of Spain winning is 39% --> Winner Brazil

# As per ELO probality calculation
#Winner : Brazil
#Ruuner-up : Spain
#Third Place: France
#Fourth Place: England
