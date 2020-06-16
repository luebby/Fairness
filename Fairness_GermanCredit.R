# Setup
library(mosaic)
data("GermanCredit", package = "klaR")

GermanCredit <- GermanCredit %>%
  mutate(sex = factor(substr(personal_status_sex,1,1))) %>%
  mutate(employment = case_when(employment_duration %in% c("4 <= ... < 7 years", "... >= 7 years") ~ "Long",
                            TRUE ~ "Short")) %>%
  mutate(employment = factor(employment)) %>%
  select(sex, employment, credit_risk)

# Higher proportion of bad risk for females
gf_bar( ~ sex, fill = ~ credit_risk, 
        position = position_fill(), data = GermanCredit)
# Higher proportion of bad risk for short employment
gf_bar( ~ employment, fill = ~ credit_risk, 
        position = position_fill(), data = GermanCredit)
# Higher proportion of short employment for femals
gf_bar( ~ sex, fill = ~ employment, 
        position = position_fill(), data = GermanCredit)

scoring_full <- glm(credit_risk ~ sex + employment, family = binomial(), data = GermanCredit)
scoring_unaware <- glm(credit_risk ~ employment, family = binomial(), data = GermanCredit)

scoring_employment <- glm(employment ~ sex, family = binomial(), data = GermanCredit)
GermanCredit <- GermanCredit %>%
  mutate(employment_r = residuals(scoring_employment))
scoring_fair <- glm(credit_risk ~ employment_r, family = binomial(), data = GermanCredit)


GermanCredit <- GermanCredit %>%
  mutate(fit_full = fitted(scoring_full),
         fit_unaware = fitted(scoring_unaware),
         fit_fair = fitted(scoring_fair))

mean(fit_full ~ sex, data = GermanCredit)
mean(fit_unaware ~ sex, data = GermanCredit)
mean(fit_fair ~ sex, data = GermanCredit)

 