
### EXCERCISE 4 ###

##########################################################
# Preparations
# https://github.com/hugwes/excercise_week4 (github-link)

##########################################################
# Load in Libraries
library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data (sf = shape file)
library(terra)        # to handle raster data
library(lubridate)    # to handle dates and times
library(zoo)          # moving window functions

##########################################################
# Task 1: Write your own function for the Euclidian Distance
euclid <- function(x,y,leadval = 1){
  sqrt((x-lead(x,leadval))^2+(y-lead(y,leadval))^2)
}

##########################################################
# Task 2: Prepare Analysis

# Import Data
wildschwein <- read_delim("wildschwein_BE_2056.csv",",")

# Subset of the data (Rosa and Sabi, Timespan 01.04.2015 - 15.04.2015)
wildschwein_filter <- wildschwein %>%
  mutate(TierName = (as.factor(TierName))) %>%
  filter(TierName == "Sabi" | TierName == "Rosa") %>%
  mutate(Jahrestag = yday(DatetimeUTC)) %>%
  filter(Jahrestag>120 & Jahrestag<135)

# Subset of the data (solutions)
wildschwein_filter <- wildschwein %>%
  filter(DatetimeUTC > "2015-04-01",
         DatetimeUTC < "2015-04-15") %>%
  filter(TierName %in% c("Rosa", "Sabi"))


##########################################################
# Task 3: Create Join Key
# Datum runden (Vorbereitung für den Join von Sabi und Rosa)
wildschwein_round <- wildschwein_filter %>%
    group_by(TierID) %>%
    mutate(DatetimeRound = lubridate::round_date(DatetimeUTC,"15 minutes"))

##########################################################
# Task 4: Measuring distance at concurrent locations
library(purrr)

# Filter (Sabi & Rosa)
sabi <- wildschwein_round %>%
  filter(TierName == "Sabi")

rosa <- wildschwein_round %>%
  filter(TierName == "Rosa")

# Join (Sabi & Rosa)
wildschwein_join <- full_join(sabi, rosa, by=c("DatetimeRound"), suffix=c("_sabi","_rosa"))

# Calculate Distance and Meet (Sabi & Rosa)
wildschwein_join <- wildschwein_join %>%
  mutate(
    distance = sqrt((E_rosa-E_sabi)^2+(N_rosa-N_sabi)^2),
    meet = distance < 100)

##########################################################
# Task 5: Visualize data

# We are just interested of the meets
wildschwein_meet <- wildschwein_join %>%
  filter(meet)

# Plot
ggplot(wildschwein_meet) +
  geom_point(data = sabi, aes(E, N, colour = "sabi"),shape = 16, alpha = 0.3) +
  geom_point(data = rosa, aes(E, N, colour = "rosa"),shape = 16, alpha = 0.3) +
  geom_point(aes(x = E_sabi,y = N_sabi, fill = "sabi"),shape = 21) +
  geom_point(aes(E_rosa, N_rosa, fill = "rosa"), shape = 21) +
  labs(color = "Regular Locations", fill = "Meets") +
  coord_equal(xlim = c(2570000,2571000), y = c(1204500,1205500))

##########################################################
# Task 6: Visualize data as timecube with plotly
meanmeetpoints <- wildschwein_join %>%
  filter(meet) %>%
  mutate(
    E.mean = (E_rosa+E_sabi)/2,
    N.mean = (N_rosa+N_sabi)/2)

library(plotly)

plot_ly(wildschwein_join, x = ~E_rosa,y = ~N_rosa, z = ~DatetimeRound,type = "scatter3d", mode = "lines") %>%
  add_trace(wildschwein_join, x = ~E_sabi,y = ~N_sabi, z = ~DatetimeRound) %>%
  add_markers(data = meanmeetpoints, x = ~E.mean,y = ~N.mean, z = ~DatetimeRound) %>%
  layout(scene = list(xaxis = list(title = 'E'),
                      yaxis = list(title = 'N'),
                      zaxis = list(title = 'Time')))

wildschwein_join %>%
  filter(DatetimeRound<"2015-04-04") %>%
  plot_ly(x = ~E_rosa,y = ~N_rosa, z = ~DatetimeRound,type = "scatter3d", mode = "lines") %>%
  add_trace(wildschwein_join, x = ~E_sabi,y = ~N_sabi, z = ~DatetimeRound) %>%
  add_markers(data = meanmeetpoints, x = ~E.mean,y = ~N.mean, z = ~DatetimeRound) %>%
  layout(scene = list(xaxis = list(title = 'E'),
                      yaxis = list(title = 'N'),
                      zaxis = list(title = 'Time')))





