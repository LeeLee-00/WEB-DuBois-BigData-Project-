library(dplyr)
library(ggplot2)

# Previously imported and cleaned data using Cleaning Census Voting Data Script
# Filtered for "Black alone"
BlkPop2016 <- testFrame %>% 
  filter(SexRaceHispanic == "Black alone") %>%
  mutate(State = toupper(State))

BlkPop2016 <- BlkPop2016[-1, ]



## Bar Graph Black Population by state ----

### Adapted from Ella Hollowood (https://rpubs.com/ejhollowood/du-bois) ###

BlackPopBar2016 <- top_n(BlkPop2016, n = 15, Population) %>%
  ggplot(., aes(x = reorder(State, Population), y = Population)) +
  geom_bar(
    stat = "identity", 
    width = 0.5, 
    fill = "red3") +
  coord_flip() +
  geom_text(data = subset(BlkPop2016, Population >= 2500), 
            aes(y = Population, ymax = Population, label = Population),
            hjust = 12,
            family = c("Rajdhani"), size = 5) +
  geom_text(data = subset(BlkPop2016, Population == 964), 
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
  labs(title = "2016 THE STATES OF THE UNITED STATES ACCORDING\nTO THEIR BLACK POPULATION.")


# Call Plot
BlackPopBar2016

### Code above adapted from Ella Hollowood (https://rpubs.com/ejhollowood/du-bois) ###
