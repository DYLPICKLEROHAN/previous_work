

check <- mwc
drops <- c("date", "day", "year", "month", "gust_dir", "gust_time", "wind_dir_9am", "wind_dir_3pm")
check <- check[ , !(names(check) %in% drops)]
check <- tibble(check)
tibble(check)
install.packages("ggcorrplot")
library("ggcorrplot")
citation
check
corelation <-cor(check, method = "pearson",use = "complete.obs")
corelation <- round(corelation, 2)
p.mat <- cor_pmat(check)
ggcorrplot(corelation, method = 'square', hc.order = TRUE, p.mat = p.mat)


library(olsrr)
library(olsrr)
install.packages("olsrr")
# Lachlan only used four models, suggesting the first was with all terms leaving three models.
#therefore only three terms need be removed.
#anova says day right off the bat
evap_model1 <- lm(evap_mm ~ month + day + max_t_C + min_t_C + humid_9am + month:humid_9am, data = mwc)
ols_step_backward_p(evap_model1, prem= 0.05)
#rid max T then day
ols_one <-     lm(evap_mm ~ month + min_t_C + humid_9am + month:humid_9am, data = mwc)
step_one <-    lm(evap_mm ~ month + min_t_C + humid_9am + month:humid_9am, data = mwc)
evap_model4 <- lm(evap_mm ~ month + min_t_C + month:humid_9am, data = mwc)
#___________________

anova(step_one, ols_one, evap_model4)
BoxCoxTrans(mwc$max_t_C)

checklambda<- lm(evap_mm ~ (max_t_C), data = mwc)
summary(checklambda)
anova(evap1)
#remove humid (0.52015)

evap2 <- lm(evap_mm ~ month + min_t_C + day + month:humid_9am, data = mwc)
summary(evap2)
anova(evap2)
#remove maxT (0.34033)

evap3 <- lm(evap_mm ~ month + min_t_C + day + month:humid_9am, data = mwc)
summary(evap3)
anova(evap3)
#remove day (0.2769)

evap4 <- lm(evap_mm ~ month + min_t_C + month:humid_9am, data = mwc)
summary(evap4)
anova(evap4)


ggplot(mwc, aes(month, humid_9am))+geom_boxplot()(mwc$evap_mm)
library("caret")
BoxCoxTrans(mwc$humid_9am)

tibble(check)
install.packages("ggcorrplot")
library("ggcorrplot")
check
corelation <-cor(check, method = "pearson",use = "complete.obs")
corelation <- round(corelation, 2)
p.mat <- cor_pmat(check)
ggcorrplot(corelation, method = 'square', hc.order = TRUE, p.mat = p.mat)


install.packages("tseries")
library("tseries")
adf.test(mwc$evap_mm)
adf.test(mwc$min_t_C) #stationary
adf.test(vars$maxtemp) #stationary
adf.test(vars$rh_morn) #not stationary

# Prediction
#For our purposes, we don't care what specific date this occurs, we only care about the environmental factors as they embody all the relationships we need to make such a prediction.
#Therefore we can use confidence intervals because we just want to be 95% sure on the average evaporation, not 95% sur on a specific evaporative event
#February 29, 2020, if this day has a minimum temperature of 13.8 degrees and reaches a 
#maximum of 23.2 degrees, and has 74% humidity at 9am.  
predict(transmodel, newdata = tibble(month = "2", min_t_C = 13.8, rh_morn = 74^1.4), interval = "prediction")
predict(chech, newdata = tibble(month = "12", min_t_C = 16.4, rh_morn = 57^1.4), interval = "prediction")
predict(chech, newdata = tibble(month = "1", min_t_C = 26.5, rh_morn = 35^1.4), interval = "prediction")
predict(chech, newdata = tibble(month = "7", min_t_C = 6.8, rh_morn = 76^1.4), interval = "prediction")
