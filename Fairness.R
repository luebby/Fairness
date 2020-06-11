#######################
# Literature (e.g.): 
# https://www.nature.com/articles/d41586-020-00274-3
# https://arxiv.org/pdf/1703.06856.pdf



# Setup
set.seed(1896)
n <- 10000
library(ggdag)
library(mosaic)
library(tidyr)


# DAG
co <- data.frame(x=c(0,1,1,2), y=c(0,0,1,0), name=c("A", "X", "U","Y")) 

Fairness <- dagify(X ~ A + U,
                   Y ~ X,  coords = co) 
ggdag(Fairness)  +
  theme_dag()


# Structual Causal Model

# Protected Attribute
# i.e. an attribute for which we do not want to discriminate
# i.e. the model should be fair with respect to this
A <- rbinom(n, 1, 0.5)

# Predictor U (Can not be observed)
U <- rnorm(n)
# Note: No child of protected attribute A

# Predictor X: Child of protected attribute A and unobserved U
X <- A + U + rnorm(n, sd = 0.1)
# Note: Biased due to A 

# Response: Child of X & U
Y <- X + U + rnorm(n, sd = 0.1)


# Data set
FairnessData <- data.frame(Y, A, U, X)


# Model with all vars - including protected
model.full <- lm(Y ~ A + X, data = FairnessData)

# Model with all vars excluding protected
model.excluded <- lm(Y ~ X, data = FairnessData)

# Model without any descendants of protected
model.strictfair <- lm(Y ~ 1, data = FairnessData)

# Model "Level 3" (https://arxiv.org/pdf/1703.06856.pdf)
# Residuals independent of predictor
model.residual <- lm(X ~ A, data = FairnessData)
FairnessData <- FairnessData %>%
  mutate(X.R = residuals(model.residual))
# X.R: Residuals of X after Regression on A
model.fair <- lm(Y ~ X.R, data = FairnessData)


# Comparison
FairnessData <- FairnessData %>%
  mutate(fitted.full = fitted(model.full)) %>%
  mutate(fitted.excluded = fitted(model.excluded)) %>%
  mutate(fitted.strictfair = fitted(model.strictfair)) %>%
  mutate(fitted.fair = fitted(model.fair)) %>%
  mutate(A = as.factor(A))

# Fairness Comparison
gf_density( ~ fitted.full, fill = ~ A, data = FairnessData) %>%
  gf_labs(title = "Volles Modell",
          subtitle ="mit A und X",
          x = "Vorhergesagte Werte",
          y = "Dichte")
# Unfair!

gf_density( ~ fitted.excluded, fill = ~ A, data = FairnessData) %>%
  gf_labs(title = "Unwissendes Modell",
          subtitle ="ohne A, aber mit X",
          x = "Vorhergesagte Werte",
          y = "Dichte")
# Still unfair.

gf_density( ~ fitted.fair, fill = ~ factor(A), data = FairnessData)%>%
  gf_labs(title = "Faires Modell",
          subtitle ="ohne A, mit Residuen von X",
          x = "Vorhergesagte Werte",
          y = "Dichte")
# Fair!

# Comparison of model performance
rsquared(model.full) 
rsquared(model.excluded)
rsquared(model.strictfair)
rsquared(model.fair)


# Counterfactual Fairness
# I.e. the predctions of Y should be
# fair with respect to A

# Some test data (incl. counterfactual A)
A0 <- c(rep(c(0,1), 5))
U0 <- rep(rnorm(5), each = 2)
U0.X <- rep(rnorm(5, sd = 0.1), each = 2)
X0 <- A0 + U0 + U0.X
TestFair <- data.frame(ID = rep(1:5, each = 2), A = A0, X = X0)

# Estimate residuals for test data
model.residual0 <- lm(X ~ A, data = TestFair)
TestFair <- TestFair %>%
  mutate(X.R = residuals(model.residual0))

# Apply model
TestFair <- TestFair %>%
  mutate(Y.Pred = predict(model.fair, newdata = TestFair))

# Compare
TestFair %>%
  pivot_wider(id_cols = ID, names_from = A, values_from = Y.Pred, names_prefix = "Score_")
