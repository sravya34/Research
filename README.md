# Research

Loading the libraries
```{r}
# install.packages("MASS")
library(MASS)
# install.packages("pscl")
library(pscl)
# insall.packages("AER")
library(AER)
#install.packages("readxl")
library(readxl)
#install.packages("Metrics")
library(Metrics)
```
Importing the dataset 
```{r}
df<- read_excel("D:/Documents/RESEARCH/data/research_30124.xlsx")
attach(df)
head(df)
summary(df$age)
```
Converting categorical variables into factors
```{r}
df$state<-as.factor(df$state)
df$residence_type <- as.factor(df$residence_type)
df$education_level <- as.factor(df$education_level)
df$religion <- as.factor(df$religion)
df$ethnicity <- as.factor(df$ethnicity)
df$wealth_index <- as.factor(df$wealth_index)
df$marital_status<-as.factor(df$marital_status)
```

```{r}
# Define variables
y <- cbind(terminationcount)
x <- cbind(age,state,residence_type, education_level, religion, ethnicity,wealth_index,marital_status)
```

POISSON REGRESSION MODEL
```{r}
# Poisson model coefficients
poisson <- glm(y ~ x, family = "poisson")
summary_poisson<-summary(poisson)
summary_poisson
poisson1<-glm(y ~ age+state+residence_type+religion+wealth_index+marital_status , family = "poisson")
summary(poisson1)
# Test for overdispersion (dispersion and alpha parameters) from AER package
dispersiontest(poisson)
dispersiontest(poisson, trafo=2)
# Calculate predicted values
predicted_poisson <- predict(poisson, type = "response")

# Calculate MAPE MSPE loglikelihood AIC_poisson using the metrics package
MAPE_P <- mape(predicted_poisson, y)
MSPE_P <- mse(predicted_poisson, y)
loglikelihood_poisson<- logLik(poisson)
AIC_poisson<-summary_poisson$aic
# Print results
cat("MAPE for poisson regression model:", MAPE_P, "\n")
cat("MSPE for poisson regression model:", MSPE_P, "\n")
cat("Log-Likelihood for poisson regression model:", loglikelihood_poisson, "\n")
cat("AIC for poisson regression model:",AIC_poisson,"\n")
```

NEGATIVE BINOMIAL REGRESSION MODEL
```{r}
# Negative binomial model coefficients
negbin <- glm.nb(y ~ x)
summary_negbin<-summary(negbin)
summary_negbin
#Calculate predicted values
predicted <- predict(negbin, type = "response")
# Calculate MAPE MSPE loglikelihood AIC using the metrics package
MAPE_n <- smape(y,predicted)
MSPE_n <- mse(y,predicted)
loglikelihood_negbin<- logLik(negbin)
AIC_negbin<-summary_negbin$aic
# Print MAPE and MSPE
cat("MAPE for negbin regression model :", MAPE_n, "\n")
cat("MSPE for negbin regression model:", MSPE_n, "\n")
cat("Log-Likelihood for poisson regression model:", loglikelihood_negbin, "\n")
cat("AIC for negbin regression model:",AIC_negbin,"\n")
```


ZERO INFLATED POISSON MODEL
```{r}
# Zero-inflated Poisson model coefficients
zip<- zeroinfl(y ~ x , link = "logit", dist = "poisson")
summary(zip)
#calculating predicted values
predicted_zip <- predict(zip, type = "response")
#Calculate MAPE and MSPE using the metrics package
MAPE_zip <- smape(y,predicted_zip)
MSPE_zip <- mse(y,predicted_zip)
log_likelihood_zip <- logLik(zip)
zip_aic<- AIC(zip)
# Print MAPE and MSPE
cat("MAPE for zip regression model :", MAPE_zip, "\n")
cat("MSPE for zip regression model:", MSPE_zip, "\n")
cat("Loglikelihood for zip regression model:",log_likelihood_zip ,"\n")
cat("AIC for zip regression model:", zip_aic, "\n")
```

ZERO INFLATED NEGATIVE BINOMIAL REGRESSION MODEL
```{r}
# Zero-inflated negative binomial model coefficients
zinb <- zeroinfl(y ~ x, link = "logit", dist = "negbin")
summary(zinb)
zinb1<-zeroinfl(y ~ x|1, link = "logit", dist = "negbin")
summary(zinb1)
#calculating predicted values
predicted_zinb <- predict(zinb, type = "response")
# Calculate MAPE and MSPE using the metrics package
MAPE_zinb <- smape(y,predicted_zinb)
MSPE_zinb <- mse(y,predicted_zinb)
zinb1_aic<- AIC(zinb1)
log_likelihood_zinb <- logLik(zinb)
# Print MAPE and MSPE
cat("MAPE for zinb regression model :", MAPE_zinb, "\n")
cat("MSPE for zinb regression model:", MSPE_zinb, "\n")
cat("AIC for zinb regression model: ",zinb1_aic,"\n")
cat("loglikelihood for zinb regression model: ",log_likelihood_zinb,"\n")
```


