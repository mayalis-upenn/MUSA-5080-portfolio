library(tidyverse)
library(tidycensus)

#I'm going to load some data now!
pa_income <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "PA",
  year = 2023,
  survey = "acs5"
)

dim(pa_income)
glimpse(pa_income)
head(pa_income)

#Yes it looks it does

as.numeric("01001")

filter(pa_income, estimate > 60000)
