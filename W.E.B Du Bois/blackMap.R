library(ggplot2)
library(dplyr)
library(tidyverse)
###########


BlackWhitePopulation <- testFrame %>% 
  filter(SexRaceHispanic == "Black alone" | SexRaceHispanic == "White alone") %>%
  mutate(State = tolower(State))

registered_voters <- testFrame$SexRaceHispanic %>% 
  filter(SexRaceHispanic == "Black alone")




#

race <- c("B", "W")

population <- c(32219, 195227)

registered_voters <- c(20844, 134889)

Voted <- c(18922, 124301)

empty <- c(100000, 100000)



df <- tibble(race, population, registered_voters, Voted, empty) %>%
  
  gather(VRace, value, 2:5) %>%
  
  mutate(order = paste(race, VRace)) %>%
  
  mutate(number_labels = str_c(value, "%"))



# Create factor to order Voter/Non Voter Population

df$order <- factor(df$order, levels = c("B Voted", "B population", "B registered_voters","B empty",
                                        
                                        "W Voted", "W population", "W registered_voters","W empty" ,ordered = TRUE))



# Create vector for colours

votePOP_fill <- c("population" = "#EE3B3B",
                     
                     "registered_voters" = "#FFD700",
                     
                     " " = "white",
                     
                     "Voted" = "#1C86EE")



# Create labels and breaks (items to appear in Key) for Voting Population.

Vote_label <- c("population" = "Total Population of Black and White Alone",
                       
                       "registered_voters" = "Registerd Voters",
                       
                       "Voted" = "Population that actually Voted")



vote_breaks <- c("population", "registered_voters", "Voted")



# Plot

ggplot(df) +
  
  
  geom_bar(aes(x = "", y = value, fill = VRace, group = order), stat = "identity") +
  
  geom_text(aes(x = "", y = 285, vjust = -18, label = "BLACKS."), family = "Rajdhani", size = 5) +
  
  geom_text(aes(x = "", y = 120, vjust = 19, label = "WHITES."), family = "Rajdhani", size = 5) +
  
  coord_polar("y", start = 5.3, direction = 1) +
  
  scale_y_reverse() +
  
  scale_fill_manual(values = votePOP_fill,
                    
                    labels = Vote_label,
                    
                    breaks = vote_breaks,
                    
                    guide = guide_legend(title = NULL, ncol = 2)) +
  
  theme_minimal() +
  
  theme(text = element_text(family = "Rajdhani", size = 16, colour = "black"),
        
        plot.title = element_text(size = 24, hjust = 0.5, face = "bold", margin = margin(0,0,10,0)),
        
        panel.background = element_blank(),
        
        panel.grid = element_blank(),
        
        axis.text = element_blank(),
        
        axis.title = element_blank(),
        
        axis.ticks = element_blank(),
        
        legend.position = "bottom") +
  
  labs(title = "2020 Election vote population OF Blacks AND WHITES IN the US")
