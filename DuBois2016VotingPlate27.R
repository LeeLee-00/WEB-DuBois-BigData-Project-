library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(cowplot)
library(openxlsx)

###########Cleaning the Data#########################


#Setting work directory
setwd("E:/W.E.B Du Bois")

#Loading Census data
#censusData <- read_xlsx("E:/W.E.B Du Bois/Data/Selected Datasect 4B - 2020.xlsx")
urlToRead <- "https://www2.census.gov/programs-surveys/cps/tables/p20/580/table04b.xlsx"
testFrame <- read.xlsx(urlToRead)


str(testFrame)
head(testFrame)

#Replacing Na with Matching value to factor the states.
testFrame[6:15, 1] <- "US"
testFrame[17:26, 1] <- "ALABAMA"
testFrame[28:37, 1] <- "ALASKA"
testFrame[39:48, 1] <- "ARIZONA"
testFrame[50:59, 1] <- "ARKANSAS"
testFrame[61:70, 1] <- "CALIFORNIA"
testFrame[72:81, 1] <- "COLORADO"
testFrame[83:92, 1] <- "CONNECTICUT"
testFrame[94:103, 1] <- "DELAWARE"
testFrame[105:114, 1] <- "DISTRICT OF COLUMBIA"
testFrame[116:125, 1] <- "FLORIDA"
testFrame[127:136, 1] <- "GEORGIA"
testFrame[138:147, 1] <- "HAWAII"
testFrame[149:158, 1] <- "IDAHO"
testFrame[160:169, 1] <- "ILLINOIS"
testFrame[171:180, 1] <- "INDIANA"
testFrame[182:191, 1] <- "IOWA"
testFrame[193:202, 1] <- "KANSAS"
testFrame[204:213, 1] <- "KENTUCKY"
testFrame[215:224, 1] <- "LOUISIANA"
testFrame[226:235, 1] <- "MAINE"
testFrame[237:246, 1] <- "MARYLAND"
testFrame[248:257, 1] <- "MASSACHUSETTS"
testFrame[259:268, 1] <- "MICHIGAN"
testFrame[270:279, 1] <- "MINNESOTA"
testFrame[281:290, 1] <- "MISSISSIPPI"
testFrame[292:301, 1] <- "MISSOURI"
testFrame[303:312, 1] <- "MONTANA"
testFrame[314:323, 1] <- "NEBRASKA"
testFrame[325:334, 1] <- "NEVADA"
testFrame[336:345, 1] <- "NEW HAMPSHIRE"
testFrame[347:356, 1] <- "NEW JERSEY"
testFrame[358:367, 1] <- "NEW MEXICO"
testFrame[369:378, 1] <- "NEW YORK"
testFrame[380:389, 1] <- "NORTH CAROLINA"
testFrame[391:400, 1] <- "NORTH DAKOTA"
testFrame[402:411, 1] <- "OHIO"
testFrame[413:422, 1] <- "OKLAHOMA"
testFrame[424:433, 1] <- "OREGON"
testFrame[435:444, 1] <- "PENNSYLVANIA"
testFrame[446:455, 1] <- "RHODE ISLAND"
testFrame[457:466, 1] <- "SOUTH CAROLINA"
testFrame[468:477, 1] <- "SOUTH DAKOTA"
testFrame[479:488, 1] <- "TENNESSEE"
testFrame[490:499, 1] <- "TEXAS"
testFrame[501:510, 1] <- "UTAH"
testFrame[512:521, 1] <- "VERMONT"
testFrame[523:532, 1] <- "VIRGINIA"
testFrame[534:543, 1] <- "WASHINGTON"
testFrame[545:554, 1] <- "WEST VIRGINIA"
testFrame[556:565, 1] <- "WISCONSIN"
testFrame[567:576, 1] <- "WYOMING"

#Removing NA rows from bottom of the spreadsheet
testFrame <- testFrame[-577:-583, ]

#Removes first 5 rows
testFrame <- testFrame[-1:-4, ]

#Reorder the testFrame
rownames(testFrame) = NULL

#Remove specific columns
#Columns we want are:
#State, Sex,Race,Hispanic-Origin, Total Pop, Total Reg, Total Voted

#Selecting Specific columns that we will be using
testFrame <- testFrame[ , c(1:3, 5, 10)]

#Renaming column headers
colnames(testFrame) <- c("State", "SexRaceHispanic", "Population", "Registered", "Voted")

str(testFrame)
head(testFrame)

#Converting Columns to Numeric
testFrame$State <- as.factor(testFrame$State)
testFrame$SexRaceHispanic <- as.factor(testFrame$SexRaceHispanic)
testFrame$Population <- as.numeric(testFrame$Population)
testFrame$Registered <- as.numeric(testFrame$Registered)
testFrame$Voted <- as.numeric(testFrame$Voted)
str(testFrame)

#Black alone vote data Independent


#################Pulling out Black and White Population to organize the visualization#################################

#Black American and White Population only 

BlackWhitePopulation <- testFrame %>% 
  filter(SexRaceHispanic == "Black alone" | SexRaceHispanic == "White alone") %>%
  mutate(State = tolower(State))



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
    
    ##Graph Title
    labs(title = "2016 REPORTED VOTING AND REGISTRATION OF BLACKS AND WHITES IN THE U.S.", color = alpha("black",0.8))  
  
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
  