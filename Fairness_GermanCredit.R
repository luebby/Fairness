# Setup
library(mosaic)
data("GermanCredit", package = "klaR")

# Data preprocessing
GermanCredit <- GermanCredit %>%
  mutate(sex = factor(substr(personal_status_sex,1,1))) %>%
  mutate(employment = case_when(employment_duration %in% c("4 <= ... < 7 years", 
                                                           "... >= 7 years") ~ "Long",
                            TRUE ~ "Short")) %>%
  mutate(employment = factor(employment)) %>%
  select(sex, employment, credit_risk)

# EDA: Barplots
gf_bar( ~ sex, fill = ~ credit_risk, 
        position = position_fill(), data = GermanCredit) %>%
  gf_labs(title = "Distribution of credit risk by sex",
          y = "Proportion",
          subtitle ="Higher proportion of bad risk for females")

gf_bar( ~ employment, fill = ~ credit_risk, 
        position = position_fill(), data = GermanCredit) %>%
  gf_labs(title = "Distribution of credit risk by employment",
          y = "Proportion",
          subtitle ="Higher proportion of bad risk for short employment")

gf_bar( ~ sex, fill = ~ employment, 
        position = position_fill(), data = GermanCredit) %>%
  gf_labs(title = "Distribution of employment by sex",
          y = "Proportion",
          subtitle ="Higher proportion of  short employment for femals")

# Modeling
# Protected attribute: sex, i.e. our model should be fair with respect to sex

# Full model, including protected attribute
scoring_full <- glm(credit_risk ~ sex + employment, family = binomial(), data = GermanCredit)

# Unaware model, excluding protected attribute
scoring_unaware <- glm(credit_risk ~ employment, family = binomial(), data = GermanCredit)

# Fair model, excluding the information of sex in employment
# Model employment
scoring_employment <- glm(employment ~ sex, family = binomial(), data = GermanCredit)

# Use residual of employment as predictor for credit_risk
GermanCredit <- GermanCredit %>%
  mutate(employment_r = (scoring_employment$y - fitted(scoring_employment)))
scoring_fair <- glm(credit_risk ~ employment_r, family = binomial(), data = GermanCredit)

# Add fitted values to data set
GermanCredit <- GermanCredit %>%
  mutate(fit_full = fitted(scoring_full),
         fit_unaware = fitted(scoring_unaware),
         fit_fair = fitted(scoring_fair))

# Compare mean scores
mean(fit_full ~ sex, data = GermanCredit)
mean(fit_unaware ~ sex, data = GermanCredit)
mean(fit_fair ~ sex, data = GermanCredit)

# Summary: what changed (?)
library(tidyr)

CreditScoring <- GermanCredit %>%
  select(-credit_risk, -employment_r) %>%
  unique() %>%
  arrange(sex, employment) %>%
  pivot_longer(cols =starts_with("fit"))

#gf_point(value ~ name | employment, color = ~ sex , data = CreditScoring)
