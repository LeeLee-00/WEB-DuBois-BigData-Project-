library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(cowplot)
library(openxlsx)

#################Pulling out Black and White Population to organize the visualization#################################

### Adapted from Ella Hollowood (https://rpubs.com/ejhollowood/du-bois) ###

#Black American and White Population only 

BlackWhitePopulation <- testFrame %>% 
  filter(SexRaceHispanic == "Black alone" | SexRaceHispanic == "White alone") %>%
  mutate(State = tolower(State))

race <- c("B","W")

population <- c(BlackWhitePopulation$Population[2],BlackWhitePopulation$Population[1])
population

Registered <- c(BlackWhitePopulation$Registered[2],BlackWhitePopulation$Registered[1])
Registered

Voted <- c(BlackWhitePopulation$Voted[2],BlackWhitePopulation$Voted[1])
Voted

#Empty is representing the blank space for the visualization

empty <- c(185000, 185000)

#Created the DataFrame 
df <- tibble(race, population, Registered, Voted, empty) %>%
  
  gather(VRace, value, 2:5) %>%
  
  mutate(order = paste(race, VRace)) %>%
  
  mutate(number_labels = str_c(value, "%"))



# Create factor to order Voter/Non Voter Population

df$order <- factor(df$order, levels = c("B Voted", "B population", "B Registered","B empty",
                                        
                                        "W Voted", "W population", "W Registered","W empty" , ordered = TRUE))



# Create vector for colors

votePOP_fill <- c("population" = "#EE3B3B",
                  
                  "Registered" = "#FFD700",
                  
                  "empty" = "#FFFFFF",
                  
                  "Voted" = "#1C86EE")


PlotGraph <-  ggplot(df) +
  
  geom_bar(aes(x = "", y = value, fill = VRace, group = order), stat = "identity") +
  
  coord_polar("y", start = 6, direction = 1) +
  
  scale_y_reverse() +
  
  scale_fill_manual(values = votePOP_fill,
                    
                    guide = guide_legend(title = NULL, ncol = 2)) +
  
  theme_minimal() +
  
  theme(text = element_text(family = windowsFont("TT Times New Roman"), size = 16, colour = "black"),
        
        plot.title = element_text(size = 24, hjust = 0.5, face = "bold", margin = margin(0,0,10,0)),
        
        panel.background = element_blank(),
        
        panel.grid = element_blank(),
        
        axis.text = element_blank(),
        
        axis.title = element_blank(),
        
        axis.ticks = element_blank(),
        
        #Used to blank out label names.
        legend.position = "none") +
  
  
  ### Adapted from Nicola Rennie (https://github.com/nrennie/dubois_challenge) ###
  
  ##Graph Title
  labs(title = "2020 REPORTED VOTING AND REGISTRATION OF BLACKS AND WHITES IN THE U.S.", color = alpha("black",0.8))  

#Colored Circles for Labels.
circle1 <- grid::circleGrob(gp = grid::gpar(fill = "#EE3B3B")) #Red
circle2 <- grid::circleGrob(gp = grid::gpar(fill = alpha("#1C86EE",0.8))) #Blue
circle3 <- grid::circleGrob(gp = grid::gpar(fill = "#FFD700")) # Yellow

##Calculation for percentages for Population, Registered and Voted Population
bpopperc <-  percent(BlackWhitePopulation$Population[2]/ (BlackWhitePopulation$Population[1] + BlackWhitePopulation$Population[2]), accuracy = 1)
wpopperc <- percent(BlackWhitePopulation$Population[1]/ (BlackWhitePopulation$Population[1] + BlackWhitePopulation$Population[2]), accuracy = 1)

bregPerc <- percent(BlackWhitePopulation$Registered[2]/BlackWhitePopulation$Population[2], accuracy = 1)
wregPerc <- percent(BlackWhitePopulation$Registered[1]/BlackWhitePopulation$Population[1], accuracy = 1)


bvotePerc <-  percent(BlackWhitePopulation$Voted[2]/(BlackWhitePopulation$Population[2]), accuracy = 1)
wvotePerc <-  percent(BlackWhitePopulation$Voted[1]/BlackWhitePopulation$Population[1], accuracy = 1)  


#Positioning for percentage, label terms and circles.    
ggdraw(PlotGraph) +
  draw_label("Black", x = 0.50, y = 1.09, hjust = 0.50, vjust = 10, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.8), size = 20) + 
  draw_label("White", x = 0.50, y = .28, hjust = 0.50, vjust = 10, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.8), size = 20) + 
  draw_label("Total Population", x= 0.38, y = 0.60, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.8), size = 14) + 
  draw_label("Total Registered", x = .62, y = 0.65, vjust = 0.38 , fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.8), size = 14) +
  draw_label("Total Voted", x = 0.37, y = 0.70, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.8), size = 14) +
  draw_label(bpopperc , x = 0.49, y = 0.95, hjust = 0.5, vjust = 10, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.8), size = 13) +
  draw_label(wpopperc , x = 0.5, y = 0.15, hjust = 0.5, vjust = 0.5, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.8), size = 13) +
  draw_label(bregPerc , x = 0.53, y = 0.95, hjust = 0.7, vjust = 10, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.8), size = 13) +
  draw_label(wregPerc , x = 0.35, y = 0.35, hjust = 0.5, vjust = 0.5, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.8), size = 13) +
  draw_label(bvotePerc , x = 0.46, y = 0.95, hjust = 0.5, vjust = 10, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.8), size = 13) +
  draw_label(wvotePerc , x = 0.66, y = 0.37, hjust = 0.5, vjust = 0.5, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.8), size = 13) +
  draw_grob(circle1 , x = -0.17, y = 0.10, scale = 0.04) +
  draw_grob(circle2 , x =-0.17, y = 0.20, scale = 0.04) +
  draw_grob(circle3 , x = 0.17, y = 0.15, scale = 0.04) 

### Code Adapted from Ella Hollowood (https://rpubs.com/ejhollowood/du-bois) ###
### COde Adapted from Nicola Rennie (https://github.com/nrennie/dubois_challenge/blob/main/2021/challenge_03.R) ###