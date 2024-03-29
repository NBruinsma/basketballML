
###############################################################
# Machine Leraning Capstone Project                           #
# Predicting the number of points a player will get in a game #
# 129921 Observations in data set from 2013-2018              #
###############################################################

###Overview

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(knncat)) install.packages("knncat", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
## Importing packages
library(data.table) # Import big files (CSV or Text) 
library(readxl) # Import Excel files
library(stringr)
library(lubridate) 
library(rpart)

#Import data of nba players for all games between 2013 and 2018 
url <- 'https://github.com/NBruinsma/basketballML/raw/master/basic_per_game_player_stats_2013_2018.csv'
dat <- read.csv(url)
#dat <- read.csv('basic_per_game_player_stats_2013_2018.csv')






#clean the data

#Rename the home/away column
colnames(dat)[7]  <- "Home"

#Split the Age into Dates and days
dat  <- dat  %>% separate(Age, c("Age", "Days"), sep = "-")



#Split name and unique identifier
dat  <- dat  %>% separate(Player, c("Player", "Unique ID"), sep = "\\\\")


#get year from date
dat <- dat %>% mutate(Year = as.numeric(str_sub(Date,1,4)))
dat$Date <- as.Date(dat$Date)

#Remove any plyers who don't have at least 10 games worth of data
dat <- dat %>% group_by(Player) %>% filter(n() >=10)


#For when we are going beyond linear regression. We can create success or failure criteria based on the average PTS that would be 
#required to be part of a daily fantasy sports team

#With 9 players, trying to reach a score of at least 200 points we would require an average of 22 points per player

dat <- dat %>% mutate(Gr22 = case_when(PTS < 22 ~ 'No', PTS >= 22 ~ 'Yes'))

#Split into test and training sets
# test_set set will be 50% of dat data

set.seed(1)

test_index <- createDataPartition(y = dat$PTS, times = 1, p = 0.5, list = FALSE)
train_set <- dat[-test_index,]
temp <- dat[test_index,]

# Make sure all players in test set are also in train set
test_set <- temp %>% 
  semi_join(train_set, by = "Player")

# Add rows removed from test_set set back into train_set set

removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

rm( test_index, temp, removed)



#Remove the points and field goal data from the test_set
test_set_full <- test_set

#Remove points, 2 point attempts, 3 point attemts, 3 point attempts, percentages hit for each variable and field goal attempts and percentage
test_set <- test_set %>% select(-PTS, -X2P, -X3P, -FT, -X2P., -X3P., -FT., -FG., -FGA, Gr22)



### Analysis

#Determine point distribution
points_dist <- train_set%>%
  group_by(PTS) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

vec_points <- as.vector(train_set$PTS)
unique(vec_points) 

vec_points <- factor(vec_points)
qplot(vec_points) +
  ggtitle("Points' Distribution") 


# Field Goal attempts vs points
train_set %>% ggplot(aes(FGA, PTS)) + geom_point()
#Not surprisingly, more attempts correlates closely with more points - this is not as useful in predicting points - since we wont know how many attempts in the specific game we are trying to predict points for

# Home average vs away average
train_set %>% group_by(Home) %>% summarize(n = n(), avg = mean(PTS), se = sd(PTS)/sqrt(n()))  %>% mutate(Home = reorder(Home, avg)) %>% ggplot(aes(x = Home, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + geom_point() + geom_errorbar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Home enjoys a slight advantage over away

#Graph points by position
train_set %>% group_by(Pos) %>% summarize(n = n(), avg = mean(PTS), se = sd(PTS)/sqrt(n()))  %>% mutate(Pos = reorder(Pos, avg)) %>% ggplot(aes(x = Pos, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + geom_point() + geom_errorbar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Certain positions are expected to get more points than others

#Graph points by age
train_set %>% group_by(Age) %>% summarize(n = n(), avg = mean(PTS), se = sd(PTS)/sqrt(n()))  %>% mutate(Age = reorder(Age, avg)) %>% ggplot(aes(x = Age, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + geom_point() + geom_errorbar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#As players get older they tend to score more points, and be more consistant (smaller standard deviation)

#Graph points by year
train_set  %>% group_by(Year) %>% summarize(points = mean(PTS)) %>% ggplot(aes(Year, points)) + geom_point() + geom_smooth()

#It appears that average points increase each year

#Graph points by opponent 
train_set %>% group_by(Opp) %>% summarize(n = n(), avg = mean(PTS), se = sd(PTS)/sqrt(n()))  %>% mutate(Opp = reorder(Opp, avg)) %>% ggplot(aes(x = Opp, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + geom_point() + geom_errorbar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Initialize RMSE function for comparing actual points scored to predicted rating
RMSE <- function(actual_rating, predicted_rating){ sqrt(mean((actual_rating - predicted_rating)^2)) }
RMSE_results <- tibble()

#Just the mean as a baseline for RMSE
mu <- mean(train_set$PTS)
RMSE_base <- RMSE(train_set$PTS, mu)
rmse_results <- data_frame(method = "Using mean only", RMSE = RMSE_base)
options(pillar.sigfig = 9)


#With just the mu, we not that the RMSE is 7.9, which is a significant number of points off on average.


#Now we can make adjustments for year, age, position, home/away, opponent and player

#Start with a prediction based on the player themselves
player_ave <- train_set %>% 
  group_by(Player) %>% 
  summarize(b_p = mean(PTS - mu))
player_ave %>% qplot(b_p, geom ="histogram", bins = 20, data = ., color = I("black"))

predicted_player_ave <- test_set %>% 
  left_join(player_ave, by='Player') %>%
  mutate(pred = mu + b_p) 
rmse_player <- RMSE(test_set_full$PTS,predicted_player_ave$pred)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Player History Model",  
                                     RMSE = rmse_player ))

#Drops the RMSE to 5.97 - which is a large improvement, but we can do better

#Add Home/Away Effect
home_ave <- train_set %>% 
  group_by(Home) %>% 
  summarize(b_h = mean(PTS - mu))
home_ave %>% qplot(b_h, geom ="histogram", bins = 20, data = ., color = I("black"))

predicted_home_ave <- test_set %>% 
  left_join(player_ave, by='Player') %>%
  left_join(home_ave, by='Home') %>%
  mutate(pred = mu + b_p + b_h) 
rmse_home <- RMSE(test_set_full$PTS,predicted_home_ave$pred)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Add Home Model",  
                                     RMSE = rmse_home ))

#Add Position
pos_ave <- train_set %>% 
  group_by(Pos) %>% 
  summarize(b_po = mean(PTS - mu))
pos_ave %>% qplot(b_po, geom ="histogram", bins = 20, data = ., color = I("black"))

predicted_pos_ave <- test_set %>% 
  left_join(player_ave, by='Player') %>%
  left_join(home_ave, by='Home') %>%
  left_join(pos_ave, by='Pos') %>%
  mutate(pred = mu + b_p + b_h + b_po) 
rmse_pos <- RMSE(test_set_full$PTS,predicted_pos_ave$pred)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Add Position Model",  
                                     RMSE = rmse_pos ))
#Note that adding in the position average actually made the model worse - it could be because we are already
#including position by virtue of including the player, and therefore double counting

#Let's remove the player model and see if the position is a more accurate determinate
predicted_pos_ave <- test_set %>% 
  left_join(home_ave, by='Home') %>%
  left_join(pos_ave, by='Pos') %>%
  mutate(pred = mu + b_h + b_po) 
rmse_pos <- RMSE(test_set_full$PTS,predicted_pos_ave$pred)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Position without Player",  
                                     RMSE = rmse_pos ))

#Clearly using a players direct history is more useful than the position alone


age_ave <- train_set %>% 
  group_by(Age) %>% 
  summarize(b_a = mean(PTS - mu))
age_ave %>% qplot(b_a, geom ="histogram", bins = 20, data = ., color = I("black"))

predicted_age_ave <- test_set %>% 
  left_join(player_ave, by='Player') %>%
  left_join(home_ave, by='Home') %>%
  left_join(age_ave, by='Age') %>%
  mutate(pred = mu + b_p + b_h + b_a) 
rmse_age <- RMSE(test_set_full$PTS,predicted_age_ave$pred)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Add Age Model (Position removed)",  
                                     RMSE = rmse_age ))
#We see the same result with Age as with position. Clearly the "Player" factors are covered by using that player's history
#Instead we will focus on the "game" factors.

year_ave <- train_set %>% 
  group_by(Year) %>% 
  summarize(b_y = mean(PTS - mu))
year_ave %>% qplot(b_y, geom ="histogram", bins = 20, data = ., color = I("black"))

predicted_year_ave <- test_set %>% 
  left_join(player_ave, by='Player') %>%
  left_join(home_ave, by='Home') %>%
  left_join(year_ave, by='Year') %>%
  mutate(pred = mu + b_p + b_h + b_y) 
rmse_year <- RMSE(test_set_full$PTS,predicted_year_ave$pred)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Add Year Model (Age Removed)",  
                                     RMSE = rmse_year ))


#opponent included
opp_ave <- train_set %>% 
  group_by(Opp) %>% 
  summarize(b_o = mean(PTS - mu))
opp_ave %>% qplot(b_o, geom ="histogram", bins = 20, data = ., color = I("black"))

predicted_opp_ave <- test_set %>% 
  left_join(player_ave, by='Player') %>%
  left_join(home_ave, by='Home') %>%
  left_join(year_ave, by='Year') %>%
  left_join(opp_ave, by='Opp') %>%
  mutate(pred = mu + b_p + b_h + b_y + b_o) 
rmse_opp <- RMSE(test_set_full$PTS,predicted_opp_ave$pred)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Add Opponent",  
                                     RMSE = rmse_opp ))





rmse_results

# Now we will conduct a decision tree analysis of the data to determine whether or not Players are likely to score more than 22




#create a dataset that uses only the colums we found to be good predicters in the linear regression modelling
Kdat <- dat %>% select(Player, Pos, Year, Opp, Home, Gr22)

#Dependant variable is a factor
Kdat$Gr22 <- factor(Kdat$Gr22)

#Make year a factor instead of numerical
Kdat$Year <- factor(Kdat$Year)

#maintain consistency with test and training sets
set.seed(1)  


test_index <- createDataPartition(y = dat$PTS, times = 1, p = 0.5, list = FALSE)


train_set <- Kdat[-test_index,]
temp <- Kdat[test_index,]

# Make sure all players in test set are also in train set
test_set <- temp %>% 
  semi_join(train_set, by = "Player")

# Add rows removed from test_set set back into train_set set

removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

rm( test_index, temp, removed)

#create a tree using Player, year, opponent and home variables (the ones linear regression showed us a relationship to pts with)

mytree <- rpart(
  Gr22 ~., 
  data = train_set[1:6], 
  method = "class",
control =rpart.control(minsplit =1,minbucket=1, cp=0.001)
)

#Explore the tree
mytree


if(!require(rattle)) install.packages("rattle", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")

library(rattle)
library(rpart.plot)
library(RColorBrewer)


fancyRpartPlot(mytree, caption = NULL)

#generate predictions for a test set
pred <- predict(mytree, test_set, type = "class")

#change Gr22 to a factor for comparison
test_set_full$Gr22 <- factor(test_set_full$Gr22)

#generate confusion matrix
confusionMatrix(pred, test_set_full$Gr22)

#check accuracy
accuracy <- postResample(pred, test_set_full$Gr22)
accuracy


