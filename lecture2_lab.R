#install.packages("fredr")

library(tidyverse)
library(fredr)

# TASK 1
# Modify the function below to accept customized observation start and end dates

state_date_function <- function(st, start.date = "1976-01-01", end.date = "2020-11-01") {
  
  start.date <- as.Date(start.date)
  end.date <- as.Date(end.date)
  
  s <- fredr(
    series_id = paste0(st, "UR"),
    observation_start = start.date,
    observation_end = end.date
  )
  
  s <- s %>%
    rename(!!st := value) %>%
    select(-c("series_id", "realtime_start", "realtime_end"))
  
  return(s)
}


# TASK 2
# Create a dataframe using the above function and these states for observations beginning 2000-01-01 to 2010-11-01
states <- c("IL", "MI", "IN", "OH", "IA", "KS", "MN")


state.data <- lapply(states, function(st) {
  state_date_function (st, start.date = "2000-01-01", end.date = "2010-11-01")
})

dataframe <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), 
                    state.data)


# TASK 3
# Plot the unemployment rate series from 2000-01-01 to 2010-11-01 series with a different color for each state


long_dataframe <- dataframe %>%
  pivot_longer(cols = -date, names_to = "state", values_to = "unemployment.rate")

ggplot(long_data, aes(x = date, y = unemployment_rate, color = state)) +
  geom_line() +
  labs(title = "Unemployment Rate by State from 2000 to 2010",
       x = "Year",
       y = "Unemployment Rate Percentage") +
  scale_color_discrete(name = "state")



