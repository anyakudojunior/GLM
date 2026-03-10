# Assignment Generalized Linear Models

# Reading the Dataset
df = read.table(
  "C:/Users/Horia/Desktop/KUL/Generalized Linear Models/researchers.txt",
  header = TRUE,
  sep = "\t",
  skip = 12,
  row.names = 1)

head(df)


# 1. Understanding the Dataset
# A pair of researchers studied a psychological phenomenon called Inattentional Blindness (IB)
# IB occurs when a person fails to notice something obvious because their attention is focused elsewhere

# During the experiment, subjects watched a video of people passing basketballs.
# The task was to count passes by players wearing white shirts. During the video, a person 
# in a gorilla suit walks across the screen.
# After the video, participants are asked whether they saw the gorilla. Most participant did not.

# The researchers hypothesized that a person's performance on the Stroop Color Word test might 
# explain whether they experience inattentional blindness.

# Variables in the dataset:
# 1. seen = binary response variable (1 seen or 0 not seen)
# Explanatory variables given by Stoop Color Test:
# 2. W = score for reading color words
# 3. C = score for naming colors of X's
# 4. CW = score for naming the color of conflicting color words

# Goal of testing: Are Stroop-scores variables related to the probability of seeing the gorilla?


# 2. Setting up a Binomial GLM (since Yi ~ Bernoulli)

# Step 1: We fit Binomial GLMs with different Link functions
# Logit Link
m_logit = glm(seen ~ W + C + CW, data=df, family = binomial(link="logit"))
# Probit Link
m_probit = glm(seen ~ W + C + CW, data=df, family = binomial(link="probit"))
# Complementary loglog Link
m_cloglog = glm(seen ~ W + C + CW, data=df, family = binomial(link="cloglog"))
# Log-Log Link
logloga = function()
{
  linkfun = function(mu) log(-log(mu))
  linkinv = function(eta) exp(-exp(eta))
  mu.eta = function(eta) -exp(-exp(eta))*exp(eta)
  valideta = function(eta) TRUE
  link = "logloga"
  structure(list(linkfun = linkfun, linkinv = linkinv,
                 mu.eta = mu.eta, valideta = valideta, name = link), class = "link-glm")
}
loglogv <- logloga()
loglogv$linkfun(loglogv$linkinv(27)) # Checking Invertibility
library("numDeriv")
all.equal(grad(loglogv$linkinv,2),loglogv$mu.eta(2)) # Checking Derivative
loglog = glm(seen ~ W + C + CW, data = dataframe ,family=binomial(link=loglogv))

# Summary Output
summary(m_logit)
summary(m_probit)
summary(m_cloglog)
summary(loglog)
# Across all three link functions, the coefficients appear to be statistically insignificant.
# We can attempt to verify this by using a Likelihood-Ratio Test, comparing an intercept-only
# model with the full model
# Null Hypothesis of LR Test, H0: Bw = Bc = Bcw = 0


# Step 2: Comparing Link Functions using AIC
AIC(m_logit, m_probit, m_cloglog, loglog)
# The Complementary Log-Log (cloglog) Link seems to have the lowest AIC, meaning it provides
# the balance of goodness of fit and model complexity amongst these 4 link functions.


# Step 3: Testing the Statistical Significance of Stroop Predictors (Likelihood Ratio Test)
# For the Complementary Log-Log Link:
m0_cloglog = glm(seen ~ 1, data=df, family=binomial(link = "cloglog"))
m1_cloglog = glm(seen ~ W + C + CW, data=df, family=binomial(link = "cloglog"))
anova(m0_cloglog, m1_cloglog, test="Chisq")
# Given the enormous p-value of 0.9165, we fail to reject H0, thus the Stroop variables
# do not improve the model beyond an intercept-only model.


# Step 4: Assessing Goodness-of-Fit
# While the Likelihood-Ratio Test answered whether the predictors help explain the response,
# the Goodness-of-Fit (GOF) assesses whether the model describes the data adequately (an intercept
# only model might still fit the data well).

# a) Deviance Test for the selected model (cloglog)
dev = deviance(m_cloglog)
df_dev = df.residual(m_cloglog)
p_dev = 1 - pchisq(dev, df_dev)

cbind(dev, df_dev, p_dev)
# Given the very small `p_dev` (0.0274), we have strong evidence of a lack of fit.

# b) Pearson Chi-Square GOF Test
pearson_X2 = sum(residuals(m_cloglog, type = "pearson")^2)
df_res = df.residual(m_cloglog)
p_pearson = 1 - pchisq(pearson_X2, df_res)

cbind(pearson_X2, df_res, p_pearson)
# Contrary to the Deviance Test, the Pearson Chi-Square test suggests no lack of fit. 
# The two GOF diagnostics disagree.

# As a conclusion, amongst the four considered link functions, the complementary log-log model
# presented the lowest AIC and was thus retained as the final model. We then assessed 
# goodness-of-fit using the Deviance test and Pearson Chi-Square test, yet the two diagnostics
# presented different conclusions about the model fit. 
# Nonetheless, based on the Likelihood Ratio test, we can conclude that the data provides 
# no evidence that Stroop test performance predicts inattentional blindness.


# DHARMa Residuals
# Instead of using classical residuals (as in Pearson, Deviance), the DHARMa package:
# 1. Simulates many datasets from the fitted model
# 2. Comapres the observed data to those simulations
# 3. Produces Uniform Residuals
# If the model is correct, the Residuals should follow Uniform(0,1)
install.packages("DHARMa")
library(DHARMa)

sim_residuals = simulateResiduals(m_cloglog)
# Uniformity Test: Verifies whether residuals are ~Uni(0,1)
testUniformity(sim_residuals)
# Dispersion Test
testDispersion(sim_residuals)
# Plots:
plot(sim_residuals)

# According to the Kolmogorov-Smirnov Test for Uniformity, the DHARMa residuals do no provide
# evidence against the expected uniform distribution, suggesting the model specification is adequate.















