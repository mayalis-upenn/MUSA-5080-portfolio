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

# countries where the margin of error is bigger than 3000

# countries where the estimate is under 50000

select(pa_income, NAME, estimate, moe)

pa_income <- mutate(pa_income, moe_pct = moe / estimate * 100)

pa_income

#moe_pct tells me the what percent of the estimate could the margin of error represent, lower moe_pct means you have a more percise estimate and vice vers. if a high moe_pct then have a high margin of error

arrange(pa_income, moe_pct)

arrange(pa_income, desc(moe_pct))

#no change in the number of rows between these two

step1 <- filter(pa_income, moe_pct > 5)
step2 <- arrange(step1, desc(moe_pct))
step3 <- select(step2, NAME, estimate, moe, moe_pct)
step3

pa_income %>%
  filter(moe_pct > 5) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, estimate, moe, moe_pct)

#this means  take pa_income, and then keep the unreliable ones, and then sort worst first, and then show me these four columns.

# Keep counties with moe_pct over 8, sort by estimate, show NAME and moe_pct

pa_income %>%
  filter(moe_pct > 8) %>%
  arrange(estimate) %>%
  select(NAME, moe_pct)

pa_income <- mutate(pa_income, reliable = moe_pct < 5)

pa_income %>%
  group_by(reliable) %>%
  summarize(n = n(),
            avg_income = mean(estimate))

pa_income <- pa_income %>%
  mutate(reliability = case_when(
    moe_pct < 3 ~ "High confidence",
    moe_pct < 6 ~ "Moderate",
    TRUE        ~ "Low confidence"
  ))

count(pa_income, reliability)

# 26 in high confidence, 7 in low confidence, 34 in moderate for a total of 67

pa_wide <- get_acs(
  geography = "county",
  variables = c(income = "B19013_001",
                pop    = "B01003_001"),
  state = "PA", year = 2023, survey = "acs5",
  output = "wide"
)

pa_wide

pa_wide %>%
  mutate(moe_pct = incomeM / incomeE * 100) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, popE, incomeE, moe_pct) %>%
  head(10)
