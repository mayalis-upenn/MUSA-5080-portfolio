

#Testing the claim: Holmesburg recorded the highest increase, from 2 percent to 19 percent between the 2006-10 survey and the 2013-17 survey

#My two variables include the following; B17001: Poverty Status in the Past 12 Months by Sex by Age and and B17001_002 refers to the total number of people in poverty. I used the acs5 since the claim from the article uses data from the acs5 to compare two years. I used the surveys from 2010 and 2017.The survey periods the articles bases its claims on do not overlap.

library(tidycensus)
library(tidyverse)

my_state <- "PA"
my_variable <- c("B17001_001", "B17001_002")

poverty_2010 <- get_acs(
  geography = "tract",
  variables = my_variable,
  state = my_state,
  county = "Philadelphia",
  year = 2010,
  survey = "acs5"
)

head(poverty_2010)

poverty_2017 <- get_acs(
  geography = "tract",
  variables = my_variable,
  state = my_state,
  county = "Philadelphia",
  year = 2017,
  survey = "acs5"
)

head(poverty_2017)
