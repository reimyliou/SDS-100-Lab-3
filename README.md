# SDS-100-Lab-3

library(tidyverse)

install.packages("openintro")

acs_df <- openintro::acs12

glimpse(acs_df)

subset_df <- acs_df |>
  select(income, age, gender, hrs_work, time_to_work) |>
  filter(gender == "female")

acs_df |> 
  select(income, employment, age)

# CODE TEMPLATE
acs_df |> 
  select(married, race, edu)

acs_df %>% 
  select(citizen, time_to_work, lang)

acs_df |>
  filter(gender == "female")

acs_df |>
  filter(age <= 30)

acs_df |>
  filter(age <= 30 & gender == "female")

acs_df |>
  filter(age <= 30 | gender == "female")

# CODE TEMPLATE
Q2<- acs_df |>
  filter(time_to_work >= 35 & income > 50000)

# Mutate data by dividing income by 1000
subset_df <- subset_df |>
  mutate(income_1000 = income / 1000)

subset_df

# Mutate data of age from years to months
subset_df %>% 
  mutate(age_months = age * 12)

# Compare to age column
acs_df %>% 
  select(age)

subset_df |>
  arrange(income_1000)

subset_df |>
  arrange(desc(income_1000))

# Arrange the subset_df data frame by the hrs_work variable
subset_df %>%
  arrange(hrs_work)

# Generate code that will arrange the subset_df data frame by time_to_work in descending order
subset_df %>%
  arrange(desc(time_to_work))

# Bringing it together
acs_df |>
  select(age, gender, race) |>
  filter(age >= 75)

# Select the variables hrs_work, income, and married, and keep observations where income is less than $ 30,000
acs_df |>
  select(hrs_work, income, married) |>
  filter(income < 30000)

# Use the pipe operator to generate a variable that measures income in increments of $10,000 and then arranges the data by hrs_work
Q3 <- acs_df %>% 
  mutate(income_10000 = income/10000) %>% 
  arrange(hrs_work)

acs_df

view(acs_df$age)
library(age)

# Question 3
?max()
max(Q3$income_10000, na.rm = TRUE)

save(data, file = "SDS 100 Lab 3.R")
