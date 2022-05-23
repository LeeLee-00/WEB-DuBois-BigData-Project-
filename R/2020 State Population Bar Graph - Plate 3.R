library(dplyr)
library(ggplot2)

# Previously imported and cleaned data using Cleaning Census Voting Data Script
# Filtered for "Black alone"
BlackPopulation <- testFrame %>% 
  filter(SexRaceHispanic == "Black alone") %>%
  mutate(State = toupper(State))

BlackPopulation <- BlackPopulation[-1, ]



## Bar Graph Black Population by state ----

### Adapted from Ella Hollowood (https://rpubs.com/ejhollowood/du-bois) ###

BlackPopulation <- top_n(BlackPopulation, n = 15, Population) %>%
    ggplot(., aes(x = reorder(State, Population), y = Population)) +
    geom_bar(
      stat = "identity", 
      width = 0.5, 
      fill = "red3") +
    coord_flip() +
    geom_text(data = subset(BlackPopulation, Population >= 2700), 
              aes(y = Population, ymax = Population, label = Population),
              hjust = 13,
              family = c("Rajdhani"), size = 5) +
    geom_text(data = subset(BlackPopulation, Population == 994), 
              aes(y = Population, ymax = Population, label = Population),
              hjust = 6,
              family = c("Rajdhani"), size = 5) +
    theme(text = element_text(family = "Rajdhani", colour = "black"),
          panel.background = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 16),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(size = 24, hjust = 0.5, 
                                    face = "bold", margin = margin(0,0,30,0))) +
    labs(title = "2020 THE STATES OF THE UNITED STATES ACCORDING\nTO THEIR BLACK POPULATION.")

# Call Plot
BlackPopulation
### Code above adapted from Ella Hollowood (https://rpubs.com/ejhollowood/du-bois) ###