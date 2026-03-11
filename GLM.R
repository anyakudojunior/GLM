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
dataframe <- data.frame(seen, W, C, CW)


# 1. Logistic regression with Logit link
logit <- glm(seen ~ W + C + CW, data = dataframe, family = binomial(link = "logit"))
summary(logit)

# Predicted probabilities
yhatlogit <- fitted(logit)

# GOF Test

hosmerlem = function(y, yhat, g = 10){
  
  cutyhat = cut(yhat,
                breaks = quantile(yhat, probs = seq(0,1,1/g)),
                include.lowest = TRUE)
  
  obs = xtabs(cbind(1 - y, y) ~ cutyhat)
  expect = xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  
  chisq = sum((obs - expect)^2 / expect)
  
  P = 1 - pchisq(chisq, g - 2)
  
  return(list(chisq = chisq, p.value = P))
}

HL <- hosmerlem(y = dataframe$seen, yhat = yhatlogit, g = 10)

data.frame(Chisq = HL$chisq, pvalue = HL$p.value)

##### 2.  Logistic regression with Probit link #######
probit <- glm(seen ~ W + C + CW, data = dataframe, family = binomial(link = "probit"))
summary(probit)

yhatprobit <- fitted(probit)

hosmerlem = function(y, yhat, g = 10){
  
  cutyhat = cut(yhat,
                breaks = quantile(yhat, probs = seq(0,1,1/g)),
                include.lowest = TRUE)
  
  obs = xtabs(cbind(1 - y, y) ~ cutyhat)
  expect = xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  
  chisq = sum((obs - expect)^2 / expect)
  
  P = 1 - pchisq(chisq, g - 2)
  
  return(list(chisq = chisq, p.value = P))
}

HL <- hosmerlem(y = dataframe$seen, yhat = yhatprobit, g = 10)
data.frame(Chisq = HL$chisq, pvalue = HL$p.value)


###### 3. Logistic regression with  Complementary Log Log link #####
cloglog <- glm(seen ~ W + C + CW, data = dataframe, family = binomial(link = "cloglog"))
summary(cloglog)

yhatcloglog <- fitted(cloglog)

hosmerlem = function(y, yhat, g = 10){
  
  cutyhat = cut(yhat,
                breaks = quantile(yhat, probs = seq(0,1,1/g)),
                include.lowest = TRUE)
  
  obs = xtabs(cbind(1 - y, y) ~ cutyhat)
  expect = xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  
  chisq = sum((obs - expect)^2 / expect)
  
  P = 1 - pchisq(chisq, g - 2)
  
  return(list(chisq = chisq, p.value = P))
}

HL <- hosmerlem(y = dataframe$seen, yhat = yhatcloglog, g = 10)
data.frame(Chisq = HL$chisq, pvalue = HL$p.value)

#### 4. Logistic regression with Log-Log link ######
loglogf <- function()
{
  ## link log(-log(mu))=eta (eta is the linear predictor)
  linkfun <- function(mu) log(-log(mu))
  ## inverse link mu=exp(-exp(eta))
  linkinv <- function(eta) exp(-exp(eta))
  ## derivative of invlink wrt eta
  mu.eta <- function(eta) -exp(-exp(eta))*exp(eta)
  valideta <- function(eta) TRUE
  link <- "logloga"
  structure(list(linkfun = linkfun, linkinv = linkinv,
                 mu.eta = mu.eta, valideta = valideta, name = link),
            class = "link-glm")
}
loglogv <- loglogf()
loglogv$linkfun(loglogv$linkinv(27)) ## check invertibility
library("numDeriv")
all.equal(grad(loglogv$linkinv,2),loglogv$mu.eta(2)) ## check derivative

loglog <- glm(seen ~ W + C + CW, data = dataframe ,family=binomial(link=loglogv))
summary(loglog)
### 5. Comparing table 
HL_results <- data.frame(
  model = c("logit", "probit", "cloglog", "loglog"),
  Chisq = c(
    hosmerlem(dataframe$seen, fitted(logit))$chisq,
    hosmerlem(dataframe$seen, fitted(probit))$chisq,
    hosmerlem(dataframe$seen, fitted(cloglog))$chisq,
    hosmerlem(dataframe$seen, fitted(loglog))$chisq
  ),
  pvalue = c(
    hosmerlem(dataframe$seen, fitted(logit))$p.value,
    hosmerlem(dataframe$seen, fitted(probit))$p.value,
    hosmerlem(dataframe$seen, fitted(cloglog))$p.value,
    hosmerlem(dataframe$seen, fitted(loglog))$p.value
  )
)
HL_results

### 6. Compare AIC 
models <- list(logit = logit,
               probit = probit,
               cloglog = cloglog,
               loglog = loglog)
aic_values <- sapply(models, AIC)
delta_aic <- aic_values - min(aic_values)  # difference from best AIC
akaike_weights <- exp(-0.5 * delta_aic) / sum(exp(-0.5 * delta_aic))

# Combine into a table, AIC, delta AIC, Aikake Weights
aic_table <- data.frame(
  Model = names(models),
  AIC = round(aic_values, 2),
  Delta_AIC = round(delta_aic, 2),
  Akaike_Weight = round(akaike_weights, 3)
)
print(aic_table)

##### 7. Residual plots 
# Deviance residuals
par(mfrow = c(2, 2))  
for (name in names(models)) {
  mod <- models[[name]]
  res_dev <- residuals(mod, type = "deviance")
  plot(fitted(mod), res_dev,
       main = paste(name, "- Deviance Residuals"),
       xlab = "Fitted values", ylab = "Deviance Residuals")
  abline(h = 0, col = "red", lty = 2)
}
par(mfrow = c(1, 1))  # reset layout

# Pearson residuals
par(mfrow = c(2, 2))
for (name in names(models)) {
  mod <- models[[name]]
  res_pearson <- residuals(mod, type = "pearson")
  plot(fitted(mod), res_pearson,
       main = paste(name, "- Pearson Residuals"),
       xlab = "Fitted values", ylab = "Pearson Residuals")
  abline(h = 0, col = "blue", lty = 2)
}
par(mfrow = c(1, 1))