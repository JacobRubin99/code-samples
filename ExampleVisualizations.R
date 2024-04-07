# filename: AnalysisVisualizations.R
# author: Jacob Rubin
# description: Create Summary Table and Line Graphs

####################################################

# If not already installed,
# install.packages("ggplot2)
# install.packages("gt")
# install.packages("tidyverse")
library(ggplot2)
library(gt)
library(tidyverse)

# modify location of your directory as needed
setwd("/Users/jacobrubin/Desktop/Spring 2024 MIT Blueprint Labs Data Task/") 

# Read data
data <- read.csv("clean.csv")


# 1. Create a table of summary statistics of the data, broken down by school year
summary_table <- data %>%
  group_by(year) %>%
  summarise(
    "Bachelor's Degree-Granting Institutions" = sum(degree_bach == 1),
    "Public Institutions" = sum(public == 1),
    "Mean FIFT Undergrads" = round(mean(enroll_ftug), digits = 0),
    "Mean State/Local Aid" = round(mean(grant_state), digits=0),
    "Mean Federal Aid" = round(mean(grant_federal), digits=0),
  ) %>%
  ungroup()

# Create a table using gt
gt_table <- summary_table %>%
  gt() %>%
  opt_stylize(style = 4) 

# Print the gt table
print(gt_table)


# 2. Two graphs

# a. Compare avg state/local grant aid across school type during the sample period
state_aid_avg_by_school <- data %>%
  mutate(school_type = case_when(
    degree_bach == 1 & public == 1 ~ "Public, Four-Year",
    degree_bach == 1 & public == 0 ~ "Private, Four-Year",
    degree_bach == 0 & public == 1 ~ "Public, Two-Year",
    degree_bach == 0 & public == 0 ~ "Private, Two-Year",
  ))  %>%
  group_by(year, school_type) %>%
  summarise(average_grant_state = mean(grant_state), .groups = 'drop')

ggplot(state_aid_avg_by_school, aes(x = year, y = average_grant_state, color = school_type)) +
  geom_line() +  # Add lines
  geom_point() + # Optionally add points
  scale_y_continuous(labels = label_comma()) + 
  labs(x = "Year", y = "Avg State/Local Grant Aid", title = "Average State/Local Grant Aid from 2010-2015 by School Type") +
  theme_minimal()  # Use a minimal theme


# b. x-axis is time; y-axis is avg. school-level enrollment of FTFT undergrads 
avg_enrollment_by_school <- data %>%
  mutate(school_type = case_when(
    degree_bach == 1 & public == 1 ~ "Public, Four-Year",
    degree_bach == 1 & public == 0 ~ "Private, Four-Year",
    degree_bach == 0 & public == 1 ~ "Public, Two-Year",
    degree_bach == 0 & public == 0 ~ "Private, Two-Year",
  ))  %>%
  group_by(year, school_type) %>%
  summarise(avg_enrollment = mean(enroll_ftug), .groups = 'drop')

ggplot(avg_enrollment_by_school, aes(x = year, y = avg_enrollment, color = school_type)) +
  geom_line() +  # Add lines
  geom_point() + # Optionally add points
  scale_y_continuous(labels = label_comma()) + 
  labs(x = "Year", y = "Avg FTFT Enrollment", title = "Average FTFT Enrollment from 2010-2015 by School Type") +
  theme_minimal()  # Use a minimal theme
