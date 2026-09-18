---
  title: "Verify the Claim: Holmesburg recorded the highest increase, from 2 percent to 19 percent between the 2006-10 survey and the 2013-17 survey"
author: "Maya Lis"
date: today
format:
  html:
  toc: true
code-fold: true
code-summary: "Show code"
embed-resources: true
---


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
  survey = "acs5",
  output= "wide"
)

head(poverty_2010)

poverty_2017 <- get_acs(
  geography = "tract",
  variables = my_variable,
  state = my_state,
  county = "Philadelphia",
  year = 2017,
  survey = "acs5",
  output= "wide"
)

head(poverty_2017)


poverty_2010 <- poverty_2010 %>%
  mutate(
    poverty_rate = B17001_002E / B17001_001E * 100,
    poverty_moe = moe_prop(
      B17001_002E,
      B17001_002M,
      B17001_001E,
      B17001_001M
    ),
    cv = poverty_moe / poverty_rate * 100
  )


head(poverty_2010)

poverty_2010 %>%
  select(NAME, poverty_rate, poverty_moe, cv, reliability) %>%
  head(10)

poverty_2017 <- poverty_2017 %>%
  mutate(
    poverty_rate = B17001_002E / B17001_001E * 100,
    poverty_moe = moe_prop(
      B17001_002E,
      B17001_002M,
      B17001_001E,
      B17001_001M
    ),
    cv = poverty_moe / poverty_rate * 100
  )


head(poverty_2017)

poverty_2017 <- poverty_2017 %>%
  mutate(
    reliability = case_when(
      cv < 12 ~ "Reliable",
      cv <= 40 ~ "Somewhat reliable",
      cv > 40 ~ "Unreliable"
    )
  )

head(poverty_2017)

poverty_2010 <- poverty_2010 %>%
  mutate(
    reliability = case_when(
      cv < 12 ~ "Reliable",
      cv <= 40 ~ "Somewhat reliable",
      cv > 40 ~ "Unreliable"
    )
  )

head(poverty_2010)

poverty_change <- poverty_2010 %>%
  left_join(
    poverty_2017,
    by = "GEOID"
  )

head(poverty_change)

poverty_change <- poverty_change %>%
  rename(
    poverty_rate_2010 = poverty_rate.x,
    poverty_rate_2017 = poverty_rate.y,
    poverty_moe_2010 = poverty_moe.x,
    poverty_moe_2017 = poverty_moe.y,
    cv_2010 = cv.x,
    cv_2017 = cv.y,
    reliability_2010 = reliability.x,
    reliability_2017 = reliability.y
  )

head(poverty_change)

poverty_change <- poverty_change %>%
  mutate(
    change = poverty_rate_2017 - poverty_rate_2010
  )

head(poverty_change)

poverty_change <- poverty_change %>%
  mutate(
    change_moe = sqrt(
      poverty_moe_2010^2 +
        poverty_moe_2017^2
    )
  )

head(poverty_change)
poverty_change <- poverty_2010 %>%
  left_join(
    poverty_2017,
    by = "GEOID"
  )

head(poverty_change)

poverty_change <- poverty_change %>%
  mutate(
    change = poverty_rate.y - poverty_rate.x
  )

head(poverty_change)

poverty_change %>%
  select(
    NAME.x,
    poverty_rate.x,
    poverty_rate.y,
    change
  ) %>%
  head(10)

poverty_change %>%
  arrange(desc(change)) %>%
  select(
    NAME.x,
    poverty_rate.x,
    poverty_rate.y,
    change
  ) %>%
  head(10)

poverty_change <- poverty_change %>%
  mutate(
    change_moe = sqrt(
      poverty_moe.x^2 + poverty_moe.y^2
    )
  )

head(poverty_change)

poverty_change %>%
  arrange(desc(change)) %>%
  select(
    NAME.x,
    poverty_rate.x,
    poverty_rate.y,
    change,
    change_moe
  ) %>%
  head(10)

poverty_change <- poverty_change %>%
  mutate(
    lower = change - change_moe,
    upper = change + change_moe
  )
head(poverty_change)

poverty_change %>%
  arrange(desc(change)) %>%
  select(
    NAME.x,
    poverty_rate.x,
    poverty_rate.y,
    change,
    change_moe,
    lower,
    upper
  ) %>%
  head(10)

head(poverty_change)

poverty_change %>%
  arrange(desc(change)) %>%
  select(
    NAME.x,
    poverty_rate.x,
    poverty_rate.y,
    change,
    change_moe,
    lower,
    upper
  ) %>%
  head(10)

poverty_change <- poverty_change %>%
  mutate(
    significance = case_when(
      lower > 0 ~ "Increase",
      upper < 0 ~ "Decrease",
      TRUE ~ "Not distinguishable from zero"
    )
  )

poverty_change %>%
  arrange(desc(change)) %>%
  select(
    NAME.x,
    poverty_rate.x,
    poverty_rate.y,
    change,
    change_moe,
    lower,
    upper,
    significance
  ) %>%
  head(10)

poverty_change %>%
  count(significance)

poverty_change %>%
  filter(significance == "Increase") %>%
  arrange(desc(change)) %>%
  select(
    NAME.x,
    poverty_rate.x,
    poverty_rate.y,
    change,
    change_moe,
    lower,
    upper,
    significance
  ) %>%
  head(10)

poverty_change %>%
  filter(significance == "Increase") %>%
  arrange(desc(change)) %>%
  select(
    NAME.x,
    poverty_rate.x,
    poverty_rate.y,
    change,
    cv.x,
    reliability.x,
    cv.y,
    reliability.y
  ) %>%
  head(10)

poverty_change %>%
  summarise(
    missing_2010 = sum(is.na(poverty_rate.x)),
    missing_2017 = sum(is.na(poverty_rate.y)),
    missing_change = sum(is.na(change))
  )

poverty_change %>%
  filter(is.na(change)) %>%
  select(
    GEOID,
    NAME.x,
    B17001_001E.x,
    B17001_002E.x,
    B17001_001E.y,
    B17001_002E.y
  )
poverty_change_analysis <- poverty_change %>%
  filter(
    !is.na(change)
  )

poverty_change_analysis %>%
  summarise(
    number_of_tracts = n()
  )

poverty_change_analysis %>%
  arrange(desc(change)) %>%
  select(
    NAME.x,
    poverty_rate.x,
    poverty_rate.y,
    change,
    change_moe,
    lower,
    upper,
    reliability.x,
    reliability.y,
    significance
  ) %>%
  head(15)

poverty_change_analysis %>%
  arrange(desc(change)) %>%
  select(
    NAME.x,
    change,
    lower,
    upper,
    reliability.x,
    reliability.y,
    significance
  ) %>%
  head(15)

poverty_change_analysis %>%
  count(significance)

poverty_change_analysis %>%
  filter(significance == "Increase") %>%
  arrange(desc(change)) %>%
  select(
    NAME.x,
    poverty_rate.x,
    poverty_rate.y,
    change,
    change_moe,
    lower,
    upper,
    reliability.x,
    reliability.y
  ) %>%
  head(10)
