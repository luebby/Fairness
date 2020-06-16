# Data
data("GermanCredit", package = "klaR")

# DAG
library(ggdag)
co <- data.frame(x=c(0,0,1), y=c(1,0,0), name=c("A", "X", "Y")) 

Credit <- dagify(X ~ A,
                 Y ~ A + X ,  coords = co) 
ggdag(Credit)  +
  theme_dag()



library(mosaic)
library(forcats)
# Data preprocessing
GermanCredit <- GermanCredit %>%
  mutate(sex = factor(substr(personal_status_sex,1,1))) %>%
  mutate(employment = case_when(employment_duration %in% c("4 <= ... < 7 years", 
                                                           "... >= 7 years") ~ "Long",
                            TRUE ~ "Short")) %>%
  mutate(employment = factor(employment)) %>%
  select(sex, employment, credit_risk) %>%
  mutate(credit_risk = fct_relevel(credit_risk, "bad"))

# EDA: Barplots
gp1 <- gf_bar( ~ sex, fill = ~ credit_risk, 
               position = position_fill(), data = GermanCredit) %>%
  gf_labs(title = "Distribution of credit risk by sex",
          y = "Proportion",
          subtitle ="Higher proportion of bad risk for females")

gp2 <- gf_bar( ~ employment, fill = ~ credit_risk, 
               position = position_fill(), data = GermanCredit) %>%
  gf_labs(title = "Distribution of credit risk by employment",
          y = "Proportion",
          subtitle ="Higher proportion of bad risk for short employment")

gp3 <- gf_bar( ~ sex, fill = ~ employment, 
               position = position_fill(), data = GermanCredit) %>%
  gf_labs(title = "Distribution of employment by sex",
          y = "Proportion",
          subtitle ="Higher proportion of  short employment for femals")

gp1
gp2
gp3

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

mean(fit_fair ~ employment, data = GermanCredit)

# Summary: what changed (?)
library(tidyr)

CreditScoring <- GermanCredit %>%
  select(-credit_risk, -employment_r) %>%
  arrange(sex, employment) %>%
  pivot_longer(cols =starts_with("fit"), names_to = "modeling", values_to = "score") %>%
  mutate(modeling = fct_relevel(modeling, "fit_full", "fit_unaware", "fit_fair"))

CreditScoring %>%
  filter(modeling == "fit_full") %>%
  gf_col(score ~ sex, data = .,
         position = position_dodge(),
         stat = "summary", fun.y = "mean") %>%
  gf_labs(title = "Scoring of full model",
          subtitle = "Including sex and employment",
          y = "Average Score")

CreditScoring %>%
  filter(modeling == "fit_unaware") %>%
  gf_col(score ~ sex, data = .,
         position = position_dodge(),
         stat = "summary", fun.y = "mean") %>%
  gf_labs(title = "Scoring of unaware model",
          subtitle = "Including only employment (not sex)",
          y = "Average Score")

CreditScoring %>%
  filter(modeling == "fit_fair") %>%
  gf_col(score ~ sex, data = .,
         position = position_dodge(),
         stat = "summary", fun.y = "mean") %>%
  gf_labs(title = "Scoring of fair model",
          subtitle = "Including only residuals of employment",
          y = "Average Score")

CreditScoring %>%
  filter(modeling == "fit_fair") %>%
  gf_col(score ~ employment, data = .,
         position = position_dodge(),
         stat = "summary", fun.y = "mean") %>%
  gf_labs(title = "Scoring of fair model",
          subtitle = "Including only residuals of employment",
          y = "Average Score")
