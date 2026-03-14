# Assignment Generalized Linear Models
# Install needed packages (run only once)
install.packages("rbibutils", type = "binary")
install.packages("Rdpack", type = "binary")
install.packages("DHARMa", type = "binary")

<<<<<<< HEAD
# Load package
library(DHARMa)

#  Create dataset
seen <- c(rep(0, 30), rep(1, 19))  

W <- c(126,118,61,69,57,78,114,81,73,93,116,156,90,120,99,
       113,103,123,86,99,102,120,128,100,95,80,98,111,101,102,
       100,112,82,72,72,89,108,88,116,100,99,93,100,110,100,106,115,120,97)

C <- c(86,76,66,48,59,64,61,85,57,50,92,70,66,73,68,
       110,78,61,65,77,77,74,100,89,61,55,92,90,85,78,
       66,78,84,63,65,71,46,70,83,69,70,63,93,76,83,71,112,87,82)

CW <- c(64,54,44,32,42,53,41,47,33,45,49,45,48,49,44,
        47,52,28,42,51,54,53,56,56,37,36,51,52,45,51,
        48,55,37,46,47,49,29,49,67,39,43,36,62,56,36,49,66,54,41)

# Combine into a data frame
df <- data.frame(seen, W, C, CW)
=======
#  Create dataset
seen = c(rep(0, 30), rep(1, 19))  
>>>>>>> 7a06a52a76947c760e65109c1bbaf683356a8d51

W = c(126,118,61,69,57,78,114,81,73,93,116,156,90,120,99,
       113,103,123,86,99,102,120,128,100,95,80,98,111,101,102,
       100,112,82,72,72,89,108,88,116,100,99,93,100,110,100,106,115,120,97)

C = c(86,76,66,48,59,64,61,85,57,50,92,70,66,73,68,
       110,78,61,65,77,77,74,100,89,61,55,92,90,85,78,
       66,78,84,63,65,71,46,70,83,69,70,63,93,76,83,71,112,87,82)

CW = c(64,54,44,32,42,53,41,47,33,45,49,45,48,49,44,
        47,52,28,42,51,54,53,56,56,37,36,51,52,45,51,
        48,55,37,46,47,49,29,49,67,39,43,36,62,56,36,49,66,54,41)

# Combine into a data frame
df = data.frame(seen, W, C, CW)




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
loglog = glm(seen ~ W + C + CW, data = df ,family=binomial(link=loglogv))

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
# All four link functions provide almost identical fit.
# The complementary log-log model has the smallest AIC but the difference is negligible.


# Step 3: Testing the Statistical Significance of Stroop Predictors (Likelihood Ratio Test)
# For the Complementary Log-Log Link:
m0_cloglog = glm(seen ~ 1, data=df, family=binomial(link = "cloglog"))
m1_cloglog = glm(seen ~ W + C + CW, data=df, family=binomial(link = "cloglog"))
anova(m0_cloglog, m1_cloglog, test="Chisq")
# Given the enormous p-value of 0.9165, we fail to reject H0, thus the Stroop variables
# do not improve the model beyond an intercept-only model.

# For the Logit Link
m0_logit = glm(seen ~ 1, data=df, family=binomial(link = "logit"))
m1_logit = glm(seen ~ W + C + CW, data=df, family=binomial(link = "logit"))
anova(m0_logit, m1_logit, test="Chisq")

# For the Probit Link
m0_probit = glm(seen ~ 1, data=df, family=binomial(link = "probit"))
m1_probit = glm(seen ~ W + C + CW, data=df, family=binomial(link = "probit"))
anova(m0_probit, m1_probit, test="Chisq")

# For the Log-Log Link
m0_loglog = glm(seen ~ 1, data=df, family=binomial(link = loglogv))
m1_loglog = glm(seen ~ W + C + CW, data=df, family=binomial(link = loglogv))
<<<<<<< HEAD
anova(m0_loglog, m1_loglog, test="Chisq")


# Step 4: Using the Hosmer-Lemeshow Test for Non-Repeated Observations

=======
anova(m0_probit, m1_probit, test="Chisq")


# Step 4: Using the Hosmer-Lemeshow Test for Non-Repeated Observations
install.packages("ResourceSelection")
>>>>>>> 7a06a52a76947c760e65109c1bbaf683356a8d51
# Complementary Log-Log Link
p_hat = fitted(m_cloglog)
library(ResourceSelection)
hoslem.test(df$seen, p_hat, g = 5)
# The large p-value of the Hosmer-Lemeshow Test suggest that there is no evidence against an adequate model fit.

# For the Logit Link
p_hat = fitted(m_logit)
hoslem.test(df$seen, p_hat, g = 5)

# For the Probit Link
p_hat = fitted(m_probit)
hoslem.test(df$seen, p_hat, g = 5)

# For the Log-Log Link
p_hat = fitted(loglog)
hoslem.test(df$seen, p_hat, g = 5)


# As a conclusion, amongst the four considered link functions, the complementary log-log model
# presented the lowest AIC and was thus retained as the final model. We then assessed 
# goodness-of-fit of all four models using the Hosmer-Lemeshow test, with none of them indicating significant 
# evidence against adequate model fit.
# Nonetheless, based on the Likelihood Ratio test, we can conclude that the data provides 
# no evidence that Stroop test performance predicts inattentional blindness.

# Residuals
# 1) DHARMa Residuals
# Instead of using classical residuals (as in Pearson, Deviance), the DHARMa package:
# 1. Simulates many datasets from the fitted model
# 2. Comapres the observed data to those simulations
# 3. Produces Uniform Residuals
# If the model is correct, the Residuals should follow Uniform(0,1)

<<<<<<< HEAD

=======
>>>>>>> 7a06a52a76947c760e65109c1bbaf683356a8d51
# Complementary Log-Log Link
sim_residuals = simulateResiduals(m_cloglog)
# Uniformity Test: Verifies whether residuals are ~Uni(0,1)
testUniformity(sim_residuals)
# Dispersion Test
testDispersion(sim_residuals)
# Plots:
plot(sim_residuals)
# According to the Kolmogorov-Smirnov Test for Uniformity, the DHARMa residuals do no provide
# evidence against the expected uniform distribution, suggesting the model specification is adequate.

# For the Logit Link
sim_residuals = simulateResiduals(m_logit)
# Uniformity Test: Verifies whether residuals are ~Uni(0,1)
testUniformity(sim_residuals)
# Dispersion Test
testDispersion(sim_residuals)
# Plots:
plot(sim_residuals)

# For the Probit Link
sim_residuals = simulateResiduals(m_probit)
# Uniformity Test: Verifies whether residuals are ~Uni(0,1)
testUniformity(sim_residuals)
# Dispersion Test
testDispersion(sim_residuals)
# Plots:
plot(sim_residuals)

# For the Log-Log Link
sim_residuals = simulateResiduals(loglog)
# Uniformity Test: Verifies whether residuals are ~Uni(0,1)
testUniformity(sim_residuals)
# Dispersion Test
testDispersion(sim_residuals)
# Plots:
plot(sim_residuals)



# 2) Pearson Residuals
# Pearson residuals measure how large the error is relative to the model's variance assumption.

# Complementary Log-Log Link
pearson_res = residuals(m_cloglog, type = "pearson")
summary(pearson_res)
plot(pearson_res)
abline(h=0)
# Logit Link
pearson_res = residuals(m_logit, type = "pearson")
summary(pearson_res)
plot(pearson_res)
abline(h=0)
# Probit Link
pearson_res = residuals(m_probit, type = "pearson")
summary(pearson_res)
plot(pearson_res)
abline(h=0)
# Log-Log Link
pearson_res = residuals(loglog, type = "pearson")
summary(pearson_res)
plot(pearson_res)
abline(h=0)


# 3) Deviance Residuals
# Deviance residuals measure how much each observation contributes to the model deviance, 
# and are thus derived from the likelihood contribution of each observation.

# Complementary Log-Log Link
deviance_res = residuals(m_cloglog, type = "deviance")
summary(deviance_res)
plot(deviance_res)
abline(h=0)
# Logit Link
deviance_res = residuals(m_logit, type = "deviance")
summary(deviance_res)
plot(deviance_res)
abline(h=0)
# Probit Link
deviance_res = residuals(m_probit, type = "deviance")
summary(deviance_res)
plot(deviance_res)
abline(h=0)
# Log-Log Link
deviance_res = residuals(loglog, type = "deviance")
summary(deviance_res)
plot(deviance_res)
abline(h=0)



# Plotting CW versus Predicted Probability of seeing the Gorilla
CW_seq = seq(min(df$CW), max(df$CW), length=100)

newdata = data.frame(
  W  = mean(df$W),
  C  = mean(df$C),
  CW = CW_seq)

p_logit   = predict(m_logit,   newdata, type="response")
p_probit  = predict(m_probit,  newdata, type="response")
p_cloglog = predict(m_cloglog, newdata, type="response")
p_loglog  = predict(loglog,  newdata, type="response")

plot(CW_seq, p_logit, type="l", col="green", lwd=2,
     xlab="CW score", ylab="Predicted Probability of seeing the gorilla", ylim=c(0,1))

# The lines represent the predicted probabillity curves (for each link function) as `CW` changes
lines(CW_seq, p_probit,  col="red",   lwd=2)
lines(CW_seq, p_cloglog, col="blue",  lwd=2)
lines(CW_seq, p_loglog,  col="black", lwd=2)
# The points represent the raw binary responses (only 0 and 1s)
points(df$CW, df$seen, pch=16, col="black")

legend("bottomright",
       legend=c("Logistic","Probit","Complementary log-log","Log-log"),
       col=c("green","red","blue","black"),
       lwd=2)
<<<<<<< HEAD
=======










>>>>>>> 7a06a52a76947c760e65109c1bbaf683356a8d51








