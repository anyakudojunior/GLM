# 1. Create dataset
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

# 2. Logistic regression
model <- glm(seen ~ W + C + CW, data = dataframe, family = binomial(link = "logit"))
summary(model)

# 3. Predicted probabilities
yhat <- fitted(model)

# 4. GOF Test

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

HL <- hosmerlem(y = dataframe$seen, yhat = yhat, g = 10)

data.frame(Chisq = HL$chisq, pvalue = HL$p.value)