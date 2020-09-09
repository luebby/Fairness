# Data
# See http://www1.beuth-hochschule.de/FB_II/reports/Report-2019-004.pdf

# DAG
library(ggdag)
co <- data.frame(x=c(0,0,1), y=c(1,0,0), name=c("A", "X", "Y")) 

dagify(X ~ A,
       Y ~ A + X ,  coords = co)  %>%
  ggdag(node_size = 20, text_size = 8, text = TRUE, text_col = "lightgray") + theme_dag_blank() +
  geom_dag_edges(arrow_directed = grid::arrow(length = grid::unit(15, "pt"), type = "closed"))  + 
  geom_text(label = "A - Gender \nX - Employment duration \nY - Credit risk", 
            hjust = 1, vjust = 1,
            x = 1, y = 1, size = 7, color = "darkgrey")



library(mosaic)
library(forcats)
# Data preprocessing
GermanCredit <- dat %>%
  mutate(gender = factor(substr(personal_status_sex,1,1))) %>%
  mutate(employment = case_when(employment_duration %in% c("4 <= ... < 7 yrs", 
                                                           ">= 7 yrs") ~ "long",
                            TRUE ~ "short")) %>%
  mutate(employment = factor(employment)) %>%
  select(gender, employment, credit_risk) %>%
  mutate(credit_risk = fct_relevel(credit_risk, "bad")) %>%
  mutate(employment = fct_relevel(employment, "short")) %>%
  rename(credit = credit_risk)

# EDA: Barplots
gf_bar( ~ gender, fill = ~ credit, 
               position = position_fill(), data = GermanCredit) %>%
  gf_labs(title = "Distribution of credit risk by gender",
          y = "Proportion",
          subtitle ="Higher proportion of bad risk for females") + theme_light()

gf_bar( ~ employment, fill = ~ credit, 
               position = position_fill(), data = GermanCredit) %>%
  gf_labs(title = "Distribution of credit risk by employment",
          y = "Proportion",
          subtitle ="Higher proportion of bad risk for short employment") + theme_light()

gf_bar( ~ gender, fill = ~ employment, 
               position = position_fill(), data = GermanCredit) %>%
  gf_labs(title = "Distribution of employment by gender",
          y = "Proportion",
          subtitle ="Higher proportion of  short employment for females") + theme_light()


# Modeling
# Protected attribute: gender, i.e. our model should be fair with respect to gender

# Full model, including protected attribute
scoring_full <- glm(credit ~ gender + employment, family = binomial(), data = GermanCredit)

# Unaware model, excluding protected attribute
scoring_unaware <- glm(credit ~ employment, family = binomial(), data = GermanCredit)

# Fair model, excluding the information of gender in employment
# Fair with respect to Demographic Parity
# Model employment
scoring_employment <- glm(employment ~ gender, family = binomial(), data = GermanCredit)

# Use residual of employment as predictor for credit
GermanCredit <- GermanCredit %>%
  mutate(employment_r = (scoring_employment$y - fitted(scoring_employment)))
scoring_fair <- glm(credit ~ employment_r, family = binomial(), data = GermanCredit)

# Add fitted values to data set
GermanCredit <- GermanCredit %>%
  mutate(fit_full = fitted(scoring_full),
         fit_unaware = fitted(scoring_unaware),
         fit_fair = fitted(scoring_fair))

# Compare mean scores
mean(fit_full ~ gender, data = GermanCredit)
mean(fit_unaware ~ gender, data = GermanCredit)
mean(fit_fair ~ gender, data = GermanCredit)

mean(fit_fair ~ employment, data = GermanCredit)

# Summary: what changed (?)
library(tidyr)

CreditScoring <- GermanCredit %>%
  select(-credit, -employment_r) %>%
  arrange(gender, employment) %>%
  pivot_longer(cols =starts_with("fit"), names_to = "modeling", values_to = "score") %>%
  mutate(modeling = fct_relevel(modeling, "fit_full", "fit_unaware", "fit_fair"))

CreditScoring %>%
  filter(modeling == "fit_full") %>%
  gf_col(score ~ gender, data = .,
         position = position_dodge(),
         stat = "summary", fun.y = "mean") %>%
  gf_labs(title = "Scoring of full model",
          subtitle = "Including gender and employment",
          y = "Average Score") + theme_light()

CreditScoring %>%
  filter(modeling == "fit_unaware") %>%
  gf_col(score ~ gender, data = .,
         position = position_dodge(),
         stat = "summary", fun.y = "mean") %>%
  gf_labs(title = "Scoring of unaware model",
          subtitle = "Including only employment (not gender)",
          y = "Average Score") + theme_light()

CreditScoring %>%
  filter(modeling == "fit_fair") %>%
  gf_col(score ~ gender, data = .,
         position = position_dodge(),
         stat = "summary", fun.y = "mean") %>%
  gf_labs(title = "Scoring of fair model",
          subtitle = "Including only residuals of employment",
          y = "Average Score") + theme_light()

# But: 
CreditScoring %>%
filter(modeling == "fit_fair") %>%
  gf_col(score ~ gender | employment, data = .,
         position = position_dodge(),
         stat = "summary", fun.y = "mean") %>%
  gf_labs(title = "Scoring of fair model",
          subtitle = "Including only residuals of employment",
          y = "Average Score") + theme_light()

CreditScoring %>%
  filter(modeling == "fit_fair") %>%
  gf_col(score ~ employment, data = .,
         position = position_dodge(),
         stat = "summary", fun.y = "mean") %>%
  gf_labs(title = "Scoring of fair model",
          subtitle = "Including only residuals of employment",
          y = "Average Score") + theme_light()



CreditScoring %>%
  unique() %>%
  arrange(gender, employment, modeling)

