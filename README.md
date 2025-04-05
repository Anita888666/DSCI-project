library(tidyverse)
library(repr)

##Load the data
players <- read_csv("data/players (1).csv")
head(players)

# Tidy and clean the data 

##select the columns that are focused on this analysis. 

players_select<-players |>
    select(gender, Age, played_hours)

# Filter the gender column to only rows with "Male" and "Female"

players_filter <- players_select |>
    filter(gender != "Non-binary",gender != "Two-Spirited", gender !=  "Prefer not to say", gender != "Agender", gender != "Other" )
players_filter

## The average video game played hours is calculated for each age
players_summarize <- players_filter |>
    group_by(Age) |>
    summarize(average_hr=mean(played_hours), na.rm=TRUE)
players_summarize
##A visualization (bar plot) that illustrate the the played hours for each age bewteen male and female
options(repr.plot.width = 15, repr.plot.height = 15)

players_plot <- players_filter |>
    ggplot(aes(x = Age, y = played_hours, fill = gender)) +
    geom_col(position = "dodge") +
    labs(x = "Age of players", y = "Average Played Hours", fill = "Gender (Male or Female)") +
    theme(text = element_text(size = 18)) +
    ggtitle("The relationship between age and average played hours for female and male")
players_plot


