#Packages
library(tidyverse)
library(ggplot2)

#Work Shortcut
setwd("E:/W.E.B Du Bois")


#cleaning the vote csv file.
myData <- read.csv("E:/DataTextMining/week5_Lab/data")






# Create tibble with data
race <- c("N", "W")
domestic_service <- c(28, 5.5)
agriculture <- c(62, 64)
manufacturing <- c(5, 13.5)
trade <- c(4.5, 13)
professions <- c(0.5, 4)
empty <- c(70, 70)

df <- tibble(race, empty, domestic_service, agriculture, manufacturing, trade, professions) %>% 
  gather(occupation, value, 2:7) %>% 
  mutate(order = paste(race, occupation)) %>% 
  mutate(number_labels = str_c(value, "%"))

# Create factor to order occupations
df$order <- factor(df$order, levels = c("N agriculture", "N domestic_service", "N manufacturing", 
                                        "N trade", "N professions", "N empty",
                                        "W agriculture", "W domestic_service", "W manufacturing", 
                                        "W trade", "W professions", "W empty", ordered = TRUE))

# Create vector for colours
occupation_fill <- c("agriculture" = "#EE3B3B",
                     "domestic_service" = "#FFD700",
                     " " = "white",
                     "manufacturing" = "#1C86EE",
                     "professions" = "#CDAA7D",
                     "trade" = "#EEEEE0")

# Create labels and breaks (items to appear in Key) for occupations
occupation_labels <- c("agriculture" = "AGRICULTURE, FISHERIES AND MINING.",
                       "manufacturing" = "MANUFACTURING AND MECHANICAL INDUSTRIES.",
                       "domestic_service" = "DOMESTIC AND PERSONAL SERVICE.",
                       "professions" = "PROFESSIONS.",
                       "trade" = "TRADE AND TRANSPORTATION.")

occupation_breaks <- c("agriculture", "manufacturing", "domestic_service", "professions", "trade")

# Plot
ggplot(df) +
  geom_bar(aes(x = "", y = value, fill = occupation, group = order), stat = "identity") +
  geom_text(aes(x = "", y = 285, vjust = -18, label = "NEGROES."), family = "Rajdhani", size = 5) +
  geom_text(aes(x = "", y = 120, vjust = 19, label = "WHITES."), family = "Rajdhani", size = 5) +
  coord_polar("y", start = 5.3, direction = 1) +
  scale_y_reverse() +
  scale_fill_manual(values = occupation_fill, 
                    labels = occupation_labels,
                    breaks = occupation_breaks,
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
  labs(title = "OCCUPATIONS OF NEGROES AND WHITES IN GEORGIA.")