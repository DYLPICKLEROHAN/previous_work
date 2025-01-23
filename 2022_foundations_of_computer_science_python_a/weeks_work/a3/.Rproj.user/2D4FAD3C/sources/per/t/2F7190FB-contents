vars<- mwc[,c( "evap_mm", "month", "day", "min_t_C", "max_t_C", "humid_9am")]
vars <- vars %>%
  mutate(maxtemp = (max_t_C)^-0.3, rh_morn = (humid_9am)^1.4)
vars <- tibble(vars)
vars
ggplot(vars, aes(x = evap_mm, maxtemp)) + 
  geom_point()+
  labs( title = "Chart revealing a moderate linear relationship \nbetween Evaporation and translated Maximum Temperature",
        x = "Evaporation (mm)",
        y = "(Maximum Temperature)^-0.3")

ggplot(vars, aes(maxtemp)) + 
  geom_histogram(binwidth=0.02) +
  labs( title = "Chart revealing the Right-Skew nature of the \n translated Maximum Temperature Data",
        x = "(Maximum Temperature)^-0.3",
        y = "Frequency")

ggplot(vars, aes(maxtemp)) + 
  geom_boxplot() +
  labs( title = "Chart revealing the outliers and spread of the \ntranslated Maximum Temperature Data",
        x = "(Maximum Temperature)^-0.3")

summary(vars$maxtemp)
sd(vars$maxtemp, na.rm = TRUE)
filter(vars, maxtemp > 0.51)
cor(vars$evap_mm, vars$maxtemp, use = "complete.obs")

ggplot(vars, aes(x = evap_mm, rh_morn)) + geom_point() + 
  labs( title = "Chart revealing the Moderate Strength, Negative Linear \nRelationship between Evaporation and the \nRelative Humidity at 9am",
        x = "Evaporation (mm)",
        y = "(Relative Humidity at 9am)^1.4")
#evap (Quant) vs humid_9am (quant) (scatter plot)
# indeed, higher humidity results in less evaporation, moderate negative linear trend with a few outliers 

ggplot(vars, aes(rh_morn)) + 
  geom_boxplot() +
  labs( title = "Chart revealing the outliers and spread of the \nTranslated Relative Humidity at 9am data",
        x = "(Relative Humidity at 9am)^1.4")

ggplot(vars, aes(rh_morn)) + 
  geom_histogram() +
  labs( title = "Chart revealing the multimodal and centered \nnature of the translated Relative Humidity data",
        x = "(Relative Humidity at 9am)^1.4",
        y = "Frequency")
summary(vars$rh_morn)
sd(vars$rh_morn, na.rm = TRUE)
filter(vars, rh_morn <133.1)
cor(vars$evap_mm, vars$rh_morn, use = "complete.obs")



install.packages("tseries")
library("tseries")
adf.test(vars$evap_mm)
adf.test(vars$min_t_C) #stationary
adf.test(vars$maxtemp) #stationary
adf.test(vars$rh_morn) #not stationary

transmodel <- lm(evap_mm ~ month + day + min_t_C + maxtemp + rh_morn + month:rh_morn, data = vars )
summary(transmodel)
anova(transmodel)

transmodel <- lm(evap_mm ~ month + min_t_C + month:rh_morn, data = vars )
summary(transmodel)
anova(transmodel)
#trying to see if models are the same or different
step(evap_model1, direction = "backward")
ols_step_backward_p(evap_model1, prem= 0.05)
ols_check <-     lm(evap_mm ~ month + min_t_C + humid_9am + month:humid_9am, data = mwc)
step_check <-    lm(evap_mm ~ month + min_t_C + humid_9am + month:humid_9am, data = mwc)
evap_model4 <- lm(evap_mm ~ month + min_t_C + month:humid_9am, data = mwc)
anova(transmodel, ols_check)


install.packages("lmtest")
install.packages("zoo") 
library("lmtest")
library("zoo")
transmodel <- lm(evap_mm ~ month + min_t_C + month:rh_morn, data = vars )
ols_check <-     lm(evap_mm ~ month + min_t_C + humid_9am + month:humid_9am, data = mwc)
lrtest(chech, ols_check)
lrtest(chech, evap_model4)
citation ("dplyr")
citation ("lubridate")
citation ("stringr")
citation ("inspectdf") 
citation ("tseries")
citation ("olsrr")
citation("ggcorrplot")
citation ("caret") 
citation ("lmtest")
citation ("zoo")

skewness(vars$maxtemp, na.rm=TRUE)
chech <- lm(evap_mm~month+min_t_C+month:rh_morn, data = vars)
predict(chech, newdata = tibble(month = "2", min_t_C = 13.8, rh_morn = (74)^1.4), interval = "prediction")
plot(chech, which = 1)
plot(chech, which = 3)
plot(chech, which = 2)
