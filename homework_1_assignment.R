# HOMEWORK 1 ----

# Libraries ----
library(tidyverse)
library(tidyquant)
library(readxl)
library(forcats)
library(stringr)

# Source Scripts ----
source("00_Scripts/assess_attrition.R")

# Data ----
path_train     <- "00_Data/telco_train.xlsx"
train_raw_tbl  <- read_excel(path_train, sheet = 1)

dept_jobrole_tbl <- train_raw_tbl %>%
  select(EmployeeNumber, Department, JobRole, PerformanceRating, Attrition)

kpi_industry_turnover_pct <- 0.088

# Productivity Cost by Role ----

productivity_cost_by_role_tbl <- read_excel("00_Data/productivity_cost_by_role.xlsx")
productivity_cost_by_role_tbl


# Q1: Which Job Role has the highest total cost of attrition? ----

dept_jobrole_productivity_tbl <- dept_jobrole_tbl %>% 
  
  count(Department, JobRole, Attrition) %>%
  count_to_pct(Department, JobRole) %>%
  assess_attrition(Attrition, attrition_value = "Yes", 
                   baseline_pct = 0.088) %>% 
  
  left_join(productivity_cost_by_role_tbl, 
            by = c("Department", "JobRole")) %>% 
  
  mutate(cost_of_attrition = calculate_attrition_cost(n = n, 
                                                      salary = Salary_Average,
                                                      net_revenue_per_employee = Revenue_Average)) %>% 
  arrange(-cost_of_attrition)

dept_jobrole_productivity_tbl %>% 
  select(Department, JobRole, cost_of_attrition) %>% 
  slice(1)

# Sales executive

# Q2: What is the total cost of attrition for the Research & Development: Research Scientist job role? ----

dept_jobrole_productivity_tbl %>% 
  filter(JobRole == "Research Scientist") %>% 
  select(Department, JobRole, cost_of_attrition)

# $2.28M

# Q3: What percentage do the top four Job Roles account for in terms of the total cost of attrition? ----

dept_jobrole_productivity_tbl %>% 
  mutate(cumulative_attrition = cumsum(cost_of_attrition),
         pct = cumulative_attrition/sum(cost_of_attrition)) %>% 
  slice(1:4) %>% 
  select(Department, JobRole, cost_of_attrition, cumulative_attrition, pct)

#86.1%

# Q4. Which Department has the highest total cost of attrition? ----

dept_jobrole_productivity_tbl %>% 
  group_by(Department) %>%
  summarise(dept_cost_of_attrition = sum(cost_of_attrition)) %>% 
  arrange(-dept_cost_of_attrition)

# Sales

# Q5: What percentage does the top Department account for in terms of the total cost of attrition? ----

dept_jobrole_productivity_tbl %>% 
  group_by(Department) %>%
  summarise(dept_cost_of_attrition = sum(cost_of_attrition)) %>%
  arrange(-dept_cost_of_attrition) %>% 
  mutate(pct = dept_cost_of_attrition / sum(dept_cost_of_attrition))

# 50.1%
