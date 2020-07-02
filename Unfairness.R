# Based on: http://aaronsadventures.blogspot.com/2019/01/discussion-of-unfairness-in-machine.html

# Setup
set.seed(1896)
n <- 1000000
library(tidyverse)
library(broom)

# Simulation of data:
# talent ~ N(100, 15)
# grade ~ N(t, 15)
# sat1 ~ N(t, 15)
# sat2 ~ N(t, 15)
# rich ~ B(0.01)
# In case rich == 1: sat = max(sat1, sat2), else sat = sat1

Data <- tibble(talent = rnorm(n, mean = 100, sd = 15)) %>%
  mutate(grade = rnorm(n, mean = talent, sd = 15)) %>%
  mutate(sat1 = rnorm(n, mean = talent, sd = 15)) %>%
  mutate(sat2 = rnorm(n, mean = talent, sd = 15)) %>%
  mutate(rich = rbinom(n, 1, prob = 0.01)) %>%
  rowwise() %>%
  mutate(sat = ifelse(rich == 1, max(sat1,sat2), sat1)) %>%
  select(-sat1, -sat2)

# Accepted if estimated talent > 115

##################
# Groupwise models
Model_Grouped <- Data %>%
  group_by(rich) %>%
  do(model = lm(talent ~ grade + sat, data = .)) %>%
  augment(model) %>%
  mutate(accepted = ifelse(.fitted > 115, "yes", "no")) %>%
  mutate(suitable = ifelse(talent > 115, "yes", "no")) %>%
  mutate(decision = case_when( (.fitted > 115) & (talent > 115) ~ "TP",
                               (.fitted > 115) & (talent <= 115) ~ "FP",
                               (.fitted <= 115) & (talent > 115) ~ "FN",
                               (.fitted <= 115) & (talent <= 115) ~ "TN"))

# Comparison of error and false negative rate
Model_Grouped %>%
  group_by(rich, decision) %>%
  summarise(n = n()) %>%
  pivot_wider(id_cols = rich, names_from = decision, values_from = n) %>%
  mutate(FNR = FN/(FN+TP)) %>%
  mutate(ER = (FN + FP) / (FN + FP + TN + TP)) %>%
  select(rich, ER, FNR)

# Remark: Result other way round to ref. (?)


#####################
# No groupwise models
Model_NoGrouped <- lm(talent ~ grade + sat, data = Data)

Model_NoGrouped <- Model_NoGrouped %>%
  augment() %>%
  mutate( rich = Data$rich) %>%
  mutate(accepted = ifelse(.fitted > 115, "yes", "no")) %>%
  mutate(suitable = ifelse(talent > 115, "yes", "no")) %>%
  mutate(decision = case_when( (.fitted > 115) & (talent > 115) ~ "TP",
                               (.fitted > 115) & (talent <= 115) ~ "FP",
                               (.fitted <= 115) & (talent > 115) ~ "FN",
                               (.fitted <= 115) & (talent <= 115) ~ "TN"))

# Comparison of error and false negative rate
Model_NoGrouped %>%
  group_by(rich, decision) %>%
  summarise(n = n()) %>%
  pivot_wider(id_cols = rich, names_from = decision, values_from = n) %>%
  mutate(FNR = FN/(FN+TP)) %>%
  mutate(ER = (FN + FP) / (FN + FP + TN + TP)) %>%
  select(rich, ER, FNR)

##########################################
# No groupwise models - without unfair sat
Model_NoGroupedFair <- lm(talent ~ grade, data = Data)

Model_NoGroupedFair <- Model_NoGroupedFair %>%
  augment() %>%
  mutate( rich = Data$rich) %>%
  mutate(accepted = ifelse(.fitted > 115, "yes", "no")) %>%
  mutate(suitable = ifelse(talent > 115, "yes", "no")) %>%
  mutate(decision = case_when( (.fitted > 115) & (talent > 115) ~ "TP",
                               (.fitted > 115) & (talent <= 115) ~ "FP",
                               (.fitted <= 115) & (talent > 115) ~ "FN",
                               (.fitted <= 115) & (talent <= 115) ~ "TN"))

# Comparison of error and false negative rate
Model_NoGroupedFair %>%
  group_by(rich, decision) %>%
  summarise(n = n()) %>%
  pivot_wider(id_cols = rich, names_from = decision, values_from = n) %>%
  mutate(FNR = FN/(FN+TP)) %>%
  mutate(ER = (FN + FP) / (FN + FP + TN + TP)) %>%
  select(rich, ER, FNR)

