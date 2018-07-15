#LEVEL DIFFICULTY IN CANDY CRUSH SAGA
#it's important to get level difficulty just right. Too easy and the game gets boring,
#too hard and players become frustrated and quit playing.


  install.packages("tidyverse")  #contains packages(ggplot2,tibble,tidyr,readr,dplyr)
  library("tidyverse")

#load csv file
  data <- read_csv("C:\\Users\\HOME\\Desktop\\Level difficulty in Candy Crush Saga\\datasets\\candy_crush.csv")
  head(data)

#Now that we have loaded the dataset let's count how many players we have in the sample 
#and how many days worth of data we have.

  print("Number of players:")
  length(unique(data$player_id))  #unique finds unique value, after that we calculate length to find number of unique players
  print("Period for which we have data:") 
  length(unique(data$dt))      #finding the unique dates on which game has been played.


#Computing level difficulty
#For example, let's say a level has been played 10 times and 2 of those attempts ended up in a victory.
#Then the probability of winning in a single attempt would be pwin = 2 / 10 = 20%
#so the probabilty of winning will tell the difficulty of level of game.
#p_win=sum(win)/sum(attempts)

  difficulty <- data %>%
    group_by(level)%>%
    summarise(wins = sum(num_success),attempts = sum(num_attempts))%>%
    mutate(p_win = wins/attempts)
  head(difficulty)

#We now have the difficulty for all the 15 levels in the episode.
#Keep in mind that, as we measure difficulty as the probability to pass a level in a single attempt,
#a lower value (a smaller probability of winning the level) implies a higher level difficulty.


#Plotting difficulty profile
#Use ggplot to plot a line graph with p_win on the Y-axis and level on the X-axis.
#Set the breaks of the X-axis to show a tick mark for every level.
#Set the Y-axis labels to a nicely formatted percentage using the scales package.

  ggplot(difficulty, aes(x = level, y = p_win)) + geom_line() + scale_x_continuous(breaks = 1:15) + scale_y_continuous(label = scales::percent)

#Spotting hard levels
#we could define a threshold of difficulty, say 10%, and label levels with pwin < 10% as hard. It's relatively easy to spot these hard levels on the plot, 
#but we can make the plot more friendly by explicitly highlighting the hard levels.
#In addition to the lines between the datapoints, add a point at each datapoint.
#Add a horizontal dashed line to the plot at Y-axis value 10% or any(used=1%).

  ggplot(difficulty, aes(x = level, y = p_win)) + geom_line() + scale_x_continuous(breaks = 1:15)+ scale_y_continuous(label = scales::percent) + geom_point(mapping = NULL) + geom_hline(yintercept = 1)

#As Data Scientists we should always report some measure of the uncertainty of any provided numbers. Maybe tomorrow, another sample will give us slightly different values for the difficulties? 
#Here we will simply use the Standard error as a measure of uncertainty:
#standard error=sqrt(p_win * (1 - p_win) / attempts)

  difficulty <- difficulty %>% 
    mutate(error = sqrt(p_win * (1 - p_win) / attempts))
  head(difficulty)

#Showing uncertainty
#let's use error bars to show this uncertainty in the plot.
#We will set the length of the error bars to one standard error.
#The upper limit and the lower limit of each error bar should then be pwin + σerror and pwin - σerror.

  ggplot(difficulty, aes(x = level, y = p_win)) + geom_line() + scale_x_continuous(breaks = 1:15) + scale_y_continuous(label = scales::percent) + geom_point(mapping = NULL) + geom_hline(yintercept = 1) + geom_errorbar(aes(ymax = p_win + error,ymin = p_win - error))

#A final metric
#One question a level designer might ask is:
#"How likely is it that a player will complete the episode without losing a single time?"
#probability of winning both level 1 and level 2 on the first attempt would be=
#p_win[1] * p_win[2]

# The probability of completing the episode without losing a single time

  p <- prod(difficulty$p_win)
  p
#value is 0.9 so the player wont be able to complete episode in one attempt , designer should be worried.
#Should our level designer worry?
# players will complete the episode in one attempt?
  
  should_the_designer_worry = FALSE
