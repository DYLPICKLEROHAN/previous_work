library("tidyverse")
library("dplyr")
library("lubridate")
library("stringr")
library("inspectdf")
library("olsrr")

mwc <- read_csv("melbourne.csv")
#Cardinia Reservoir - geographical factors 
#(altitude, surrounding flora and funa, surrounding landscape, concrete and it's heat retention propertiesetc.) )
#salt content of the water, water density
#to improve water retention make the reservoir deeper so there is less surface area, add salts to raise the energy threshold for evaporation, or cover the entire surface area with something that will increase surface tension or remove direct sunlight/ wind
colnames(mwc)
#simplify column names:
mwc <- rename(mwc, 
      date = "Date",
      min_t_C = "Minimum temperature (Deg C)",
      max_t_C = "Maximum Temperature (Deg C)",
      rainfall_mm = "Rainfall (mm)",
      evap_mm = "Evaporation (mm)",
      sun_hrs = "Sunshine (hours)", 
      gust_dir = "Direction of maximum wind gust",
      gust_kmh = "Speed of maximum wind gust (km/h)",
      gust_time = "Time of maximum wind gust",
      temp_9am_C = "9am Temperature (Deg C)",
      humid_9am = "9am relative humidity (%)",
      cloud_9am = "9am cloud amount (oktas)",
      wind_dir_9am = "9am wind direction",
      wind_speed_9am = "9am wind speed (km/h)",
      pres_9am = "9am MSL pressure (hPa)",
      temp_3pm_C = "3pm Temperature (Deg C)",
      humid_3pm = "3pm relative humidity (%)",
      cloud_3pm = "3pm cloud amount (oktas)",
      wind_dir_3pm = "3pm wind direction",
      wind_speed_3pm = "3pm wind speed (km/h)",
      pres_3pm = "3pm MSL pressure (hPa)"
      )
#create day, month, year columns
mwc <- mwc  %>%
  mutate(date = as.Date(date), 
         day = day(date), month = month(date), year = year(date))
#evaporation is now subject, date information besides it.
mwc <- mwc[,c(5, 1, 24, 23, 22, 2, 3, 4,  6:21)]
#make day = day of week
mwc$day <- weekdays(mwc$date)

#Set value types
#low level categorical = factor, year could be considered a character as in the scheme of things time is infinite but we only have from 2019 so in this context it'll be a factor
mwc$year <-   factor(mwc$year)
month_levels <- c("6", "7", "8", "9", "10", "11", "12", "1", "2", "3", "4", "5")
mwc$month <-   factor(mwc$month, levels = month_levels) #setting levels from lowest average to highest
day_levels <-  c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
mwc$day <-   factor(mwc$day, levels = day_levels)
mwc$gust_dir <- factor(mwc$gust_dir)
mwc$wind_dir_9am <- factor(mwc$wind_dir_9am)
mwc$wind_dir_3pm <- factor(mwc$wind_dir_3pm)
#All measurements appear to have a set amount of significant figures suggesting all other variables are discrete =integers
mwc$evap_mm <- as.integer(mwc$evap_mm)
mwc$min_t_C <- as.integer(mwc$min_t_C)
mwc$max_t_C <- as.integer(mwc$max_t_C)
mwc$rainfall_mm <- as.integer(mwc$rainfall_mm)
mwc$sun_hrs <- as.integer(mwc$sun_hrs)
mwc$gust_kmh <- as.integer(mwc$gust_kmh)
mwc$temp_9am_C <- as.integer(mwc$temp_9am_C)
mwc$humid_9am <- as.integer(mwc$humid_9am)
mwc$cloud_9am <- as.integer(mwc$cloud_9am)
mwc$wind_speed_9am <- as.integer(mwc$wind_speed_9am)
mwc$pres_9am <- as.integer(mwc$pres_9am)
mwc$temp_3pm_C <- as.integer(mwc$temp_3pm_C)
mwc$wind_speed_9am <- as.integer(mwc$wind_speed_9am)
mwc$humid_3pm <- as.integer(mwc$humid_3pm)
mwc$cloud_3pm <- as.integer(mwc$cloud_3pm)
mwc$wind_speed_3pm <- as.integer(mwc$wind_speed_3pm)
mwc$pres_3pm <- as.integer(mwc$pres_3pm)
inspect_types(mwc)  #Looking good, time is already sorted for me.
#Tibble ready for analysis
mwc

# EDA and Bivariate summaries  

## Evaporation 
ggplot(mwc, aes(x = evap_mm)) + geom_histogram()
ggplot(mwc, aes(x = evap_mm)) + geom_boxplot()
summary(mwc$evap_mm)
sd(mwc$evap_mm, na.rm = TRUE)
filter(mwc, evap_mm >=15)
# shape = Right skewed,
# outliers (9)= 5 at 15mm, 3 at 18mm, 1 at 20mm
# Spread [0,20] = mean = 4.936mm, SD = 3.504218 mm, IQR = 5mm
# Location: median = 4mm

## month
ggplot(mwc, aes(x = month, evap_mm)) + geom_boxplot()
#evap (Quant) vs month (cat) (side by side box-plot)
ggplot(mwc, aes(evap_mm)) + geom_histogram() + facet_wrap(~month)
#each month appears to have a fairly right scewed shape many appearing to be multimodal, the month alters the domain, 
#Jan to December, clear seasonal effects. This will have an interaction term with temp variables
#January(with the summer solstice) marking the maximum and June(with the winter solctice) marking the minimum.
# We travel an elliptical path around the sun which creates a regular weather pattern that we feel as seasons resulting from the angle of incidence on the atmosphere and consequently the amount of energy we receive from the sun.
# quite a few outliers throughout the year, the least variation is in June.
#all domains are within (-1, 21)


## Day of week
# not expecting to find anything informative here
ggplot(mwc, aes(x = day, evap_mm)) + geom_boxplot()
#evap (Quant) vs day (cat) (side by side box-plot)
unique(mwc$day)
#Not seeing too much going on from day to day. Makes sense, this cycles doesn't influence weather patterns and is an arbitrary measure created for humans not nature. 

## Maximum Temp in deg C
ggplot(mwc, aes(x = evap_mm, max_t_C)) + geom_point()
#evap (Quant) vs Max T (quant) (scatterplot)
#Moderate, positive linear trend. Variation likely due to wind speed. Evaporation depends on the energy of water molecules. higher energy (hotter) molecules will break hydrogen bonds and become gaseous. Wind speed reduces the energy need to leave the surface were the hottest particles are highly likley to be.
ggplot(mwc, aes(max_t_C))+ geom_histogram(binwidth=3)
#right skewed
ggplot(mwc, aes(log(max_t_C)))+ geom_histogram(binwidth=0.1)
#normalizes well with log(x)

## Minimum Temp in deg C
# As above but less of a slope is what I'm predicting. Colder days might have larger wind speeds though... 
ggplot(mwc, aes(x = evap_mm, min_t_C)) + geom_point()
#evap (Quant) vs Min T (quant) (scatterplot)
#don't be fooled by the y-axis, the slop is much lower. Moderate , positive linear trend.
ggplot(mwc, aes(min_t_C))+ geom_histogram(binwidth=3)
#fairly normal distribution

## Relative Humidity at 9am
# The amount of water you can have in the air is limited. Higher humidity makes it more difficult for water to become gaseous as crowding molecules become heavier and sink back into a liquid.
# correlates with season, and therefore months (interaction term)
ggplot(mwc, aes(x = evap_mm, humid_9am)) + geom_point()
#evap (Quant) vs humid_9am (quant) (scatter plot)
# indeed, higher humidity results in less evaporation, moderate negative linear trend with a few outliers 

# Model selection
#Evaporation (in mm) on a given day in Melbourne (our evap_mm is a daily measure from melbourne)
evap_model1 <- lm(evap_mm ~ month + max_t_C + min_t_C + humid_9am + day + month:humid_9am, data = mwc)
summary(evap_model1)
anova(evap_model1)
#rid day (Anova shows the chance of the mean evaporation on any given day is roughly the same)

evap_model2 <- lm(evap_mm ~ month + max_t_C + min_t_C + humid_9am + month:humid_9am, data = mwc)
summary(evap_model2)
anova(evap_model2)
#Rid humid_9am (p = 0.602627)

evap_model3 <- lm(evap_mm ~ month + min_t_C + max_t_C + month:humid_9am, data = mwc)
summary(evap_model3)
anova(evap_model3)
#rid max_t_C (p = 0.492521)

evap_model4 <- lm(evap_mm ~ month + min_t_C + month:humid_9am, data = mwc)
summary(evap_model4)
anova(evap_model4)
#All checks out.

#day is an arbitrary unnatural variable, month is useful only for determining climate, but temperatures and humidities actually are the climate, and so it makes sense that only those two really matter.
#humidity in relation to time of year might be more important
#Max-t is related to humidity, the minimum t might also be more meaningful when it comes to the amount of evap as measured in mm.
#Check it:

step(evap_model1, direction = "backward")
ols_step_backward_p(evap_model1, prem= 0.05)
ols_check <-     lm(evap_mm ~ month + min_t_C + humid_9am + month:humid_9am, data = mwc)
step_check <-    lm(evap_mm ~ month + min_t_C + humid_9am + month:humid_9am, data = mwc)
#the above two functions found the same answer. so is ours statistically differnt?
evap_model4 <- lm(evap_mm ~ month + min_t_C + month:humid_9am, data = mwc)
anova(ols_check, evap_model4, df = 2)
#ANOVA finds no statistically significant difference between these models.

# Model Diagnostics
plot(evap_model4, which = 1) #linearity
plot(evap_model4, which = 3) #homoscedasticity
plot(evap_model4, which = 2) #noise is normally distributed
#Assumptions about linearity is fairly good for the bulk of the data, 
#homoscedasticity shows a slight linear trend but nothing exponential, independence of max T and humidity is also questionable
#Noise is fairly normally distributed.

# Prediction
#For our purposes, we don't care what specific date this occurs, we only care about the environmental factors as they embody all the relationships we need to make such a prediction.
#Therefore we can use confidence intervals because we just want to be 95% sure on the average evaporation, not 95% sur on a specific evaporative event
#February 29, 2020, if this day has a minimum temperature of 13.8 degrees and reaches a 
#maximum of 23.2 degrees, and has 74% humidity at 9am.  
predict(evap_model4, newdata = tibble(month = "2", min_t_C = 13.8, humid_9am = 74), interval = "prediction")
#5.356955mm
#9.801579mm at most

#December 25, 2020, if this day has a minimum temperature of 16.4 degrees and reaches a 
#maximum of 31.9 degrees, and has 57% humidity at 9am.  
predict(evap_model4, newdata = tibble(month = "12", min_t_C = 16.4, humid_9am = 57), interval = "prediction")
#8.346807 mm
#12.77355 mm at most

#January 13, 2020, if this day has a minimum temperature of 26.5 degrees and reaches a 
#maximum of 44.3 degrees, and has 35% humidity at 9am
predict(evap_model4, newdata = tibble(month = "1", min_t_C = 26.5, humid_9am = 35), interval = "prediction")
#14.50894mm 
#19.30938mm at most

#July 6, 2020, if this day has a minimum temperature of 6.8 degrees and reaches a maximum of 
#10.6 degrees, and has 76% humidity at 9am.  
predict(evap_model4, newdata = tibble(month = "7", min_t_C = 6.8, humid_9am = 76), interval = "prediction")
#2.062808 
#6.468986mm at most
 

#Confidence interval = 95% chance of finding the mean evap_mm measurement
#prediction interval = 95% chance of finding a specific evap_mm measurement
#We want particular days values so we use prediction intervals