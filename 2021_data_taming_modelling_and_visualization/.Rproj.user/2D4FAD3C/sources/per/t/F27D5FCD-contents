#week 2
library(tidyverse)
data(mpg)
mpg
ggplot(mpg, aes(x=manufacturer))+geom_bar()
#basic bar chart
ggplot(mpg, aes(x=manufacturer, fill=trans))+geom_bar()
#stacked bar chart showing manufacturer against bin total, transmission stacked
ggplot(mpg, aes(x=manufacturer, fill=trans))+
geom_bar(position="fill", col= "black")+
theme(axis.text.x= element_text(angle =-90, hjust = 0))
#stacked chart measures proportions of trnansimission types by each manufacture, labels at 90 degrees

ggplot(mpg, aes(x=displ, y=cty))+geom_point()
cor(mpg$displ, mpg$cty)
#pearson correlation coefficient^
ggplot(mpg, aes(x=displ, y= cty))+geom_point()


ggplot(mpg, aes(x=fct_reorder(manufacturer, displ), y= displ, fill=manufacturer))+geom_boxplot(show.legend = FALSE)+labs(x= "manufacturer", Y= "displacement (litres)")
mpg
#drv = character
ggplot(mpg, aes(x=drv))+geom_boxplot()
#displ=dbl
ggplot(mpg, aes(x=displ))+geom_histogram()


data("gss_cat")
gss_cat
ggplot(gss_cat, aes(x=marital, fill=relig, height=1))+geom_bar(position = "fill") + theme(axis.text.x= element_text(angle =-90, hjust = 0))

library(dplyr)
x <-  table(gss_cat$marital, gss_cat$rincome) 
x
y <- colSums(x)
y
z <- prop.table(x)*100
abc <- data.frame(z)
179/7363

data(msleep)
msleep
ggplot(msleep, aes(x = sleep_total, y = sleep_rem))+geom_point()
data(diamonds)
diamonds

mpg
ggplot(mpg, aes(x=class, y=hwy, col=drv, shape=fl))+geom_jitter()

data(starwars)
starwars
ggplot(starwars, aes(x=height, y=mass, col= gender))+geom_point()+geom_smooth()
ggplot(starwars, aes(x=height))+geom_histogram()+facet_wrap(~sex)

ggplot(diamonds, aes(x=cut, y=price))+ geom_boxplot(aes(fill=clarity), colour="azure")

?mpg
ggplot(mpg, aes(x=as_factor(year), y=hwy))+geom_boxplot()
ggplot(mpg, aes(x=displ, y=hwy, col=year))+geom_point()+geom_smooth()
summary(mpg$year)
summary(mpg$displ)

library(lubridate)
today <- "20-jun-2021"
today <- dmy(today)
today
today+months(1)
#adds a month to date
year(today)
month(today)
month(today, label=TRUE)
day(today)
end_year <- dmy("31-dec-2021")-today
#time difference
as.duration(end_year)
end_year/dweeks(1)
end_year/dhours(1)


library(stringr)
data("sentences")
first_ten <- sentences[1:10]
first_ten
str_length(first_ten)
#number of keyboard buttons pressed to generate that sentence
string_1 <- "hello "
string_2 <- "world"
str_c(string_1, string_2)
#join strings, remember to add space
str_detect(first_ten, "chicken")
#is the word present
first_ten[str_detect(first_ten, "chicken")]
#tell me the string
str_replace(first_ten, "chicken", "turkey")
#replaced chicken with turkey

read_lines("c\\data.txt")
#reads text file
str_match patient (\\d+): (\\d+) kg
#where \\d+ means numbers), () means grab those 
#details  <- str_match(messy_data_weight, "patient (\\d+): (\\d+) kg")

#df  <- tibble(
  #text = messy_data_weight, 
  #patient = details[, 2],
  #weight = details[, 3]
#)
#df#
#^example

data(starwars)
xmas <- "21-dec-2021"
xmas <- dmy("25-dec-2021")
xmas
dmy("25-dec-2021")-dmy("31-10-2021")
xma/dweeks(1)

string <- "hello my name is jono"
str_replace(string, "jono", "Dylan")

data("diamonds")
summary(diamonds$cut)
_mutate(cut=fct_lump(diamonds$cut, n=2))

library(lubridate)
y2k <- dmy("01-jan-2000")
y2k
wday(y2k, label=TRUE)
leap_day <- dmy("29-feb-2020")
difftime(y2k,leap_day, units = "hours")
(dmy("29/2/2020") - dmy("1/1/2000"))/dhours(1)

data(presidential)
a <- presidential$start
b <- presidential$end
a
b
d <- difftime(b,a, units = "hours")
mutate(presidential, length = d)
       

install.packages("gutenbergr")
library(gutenbergr)
wuthering <- gutenberg_download(768) 
wuthering$text %>%
  str_detect("horse") %>%
  sum()

wuthering$text %>%
  str_detect("Honest people") %>%
  which()

dracula <- (gutenberg_download(345))
dracula$text %>%
  str_detect("vampire") %>%
  sum()
messy_text <- c("I am 175 cm tall and I'm 24 years old.",
                "My father is 52 years old and 185 cm tall.", 
                "We're both taller than Queen Elizabeth II, who
                  stands at 163 cm, but is way older than us at 93 years.")
tibble(height= str_match(messy_text, " (\\d+) cm"), ages= str_match(messy_text, " (\\d+) years"))
data(gss_cat)
gss_cat
summary(gss_cat$partyid)
summary(unique(gss_cat$partyid))
summary(gss_cat$partyid)
summary(fct_lump(gss_cat$partyid,4))

install.packages("pacman")
pacman::p_load(inspectdf)
data(starwars)
set.seed(2019)
starwars1 <- sample_n(starwars, 50)
starwars1
#randomly selected 50 characters
starwars2 <- sample_n(starwars, 50)
starwars2 <- starwars2[, 2:12]
starwars2
inspect_types(starwars1)
show_plot(inspect_types(starwars1))
#percentage of value types
inspect_types(starwars2)
show_plot(inspect_types(starwars1, starwars2))
inspect_num(starwars1)
show_plot(inspect_num(starwars1))
inspect_cor(starwars1)
show_plot(inspect_cor(starwars1))
show_plot(inspect_cor(starwars1, starwars2))
inspect_cat(starwars1)
show_plot(inspect_cat(starwars1))
show_plot(inspect_cat(starwars1, starwars2))

inspect_na(starwars1)
show_plot(inspect_na(starwars1, starwars2))

data(diamonds)
show_plot(inspect_cat(diamonds))          
inspect_num(diamonds)


#week 3
mpg
mpg[1,]
pacman::p_load(nycflights13)
flights
library(dplyr)
filter(flights, month == 1)
#filter(dataframe,month is equal to 1, so January
filter(flights, month==1, day== 1)

#magrittr -> pipes -> %>%
flights %>% filter(carrier == "AA")
flights %>% filter(carrier == "AA", month ==1, day==1) %>% select(flight, dep_time, arr_time)

select(flights, contains("time"))
#selects anything with time columns
flights %>% select(contains("time"))
flights %>% mutate(delay = dep_time, sched_dep_time)
flights <- flights %>% mutate(delay = dep_time, sched_dep_time)

flights %>%
  select(origin, dest, distance) %>%
  arrange(distance)
#arranged from shortest to largest
flights %>%
  select(origin, dest, distance) %>%
  arrange(desc(distance))
#largest to smallest
by_month <- group_by(flights, month)
by_month
#group data into clusters for each month
library(ggplot2)
summarise(by_month, delay = mean(dep_delay, na.rm = TRUE))
by_day <- group_by(flights,year,month,day) 
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE)) %>%
  ungroup %>%
  mutate(day_num = seq_along(delay)) %>%
  ggplot(aes(day_num,delay))+
  geom_point()+
  geom_smooth()

summarise(by_day, delay= mean(dep_delay, na.rm = TRUE), num_flights = n()) %>%
  ggplot(aes(num_flights, delay))+
  geom_point()+
  geom_smooth()

airlines
flights <- left_join(flights, airlines, by = "carrier")
flights
flights %>%
  select(year, month, day, carrier, name)

data(mpg)
mpg %>% 
  filter(year == "1999")%>%
  count()

mpg %>% 
  select(year, manufacturer)

mpg %>%
  mutate(hwy = hwy*0.42)
mpg %>%
  group_by(year) %>%
  summarize(mean_cty = mean(cty))
data(diamonds)
diamonds
diamonds%>%
  filter(cut == "Ideal") %>%
  count()
diamonds %>%
  filter(cut=="Ideal", price < 400, color == "D")%>%
  count()

diamonds2 <- diamonds %>%
  mutate(cheap = price < "400")

diamonds2 %>%
  filter(cheap=="TRUE")%>%
  count()

diamondss <- diamonds %>%
  select(depth, price) %>%
  arrange(depth)
diamondss

mean_depth <- diamonds%>%
  group_by(cut)%>%
  summarise( mean_depth = mean(depth, na.rm=TRUE))
mean_depth

flights <- left_join(diamonds, mean_depth, by = "cut")
flights

hm <- group_by(diamonds, clarity)
hmm <- summarise(hm, mdepth = mean(depth), sddepth = sd(depth))
hmm
ggplot(hmm, aes(x = sddepth, y = mdepth)) + 
  geom_point() + 
  geom_smooth()
data(diamonds)
diamonds

ok <- diamonds %>%
  filter( cut == "Fair")%>%
  group_by(clarity)%>%
  summarise(mdepth = mean(depth), sddepth = sd(depth))
ok
ggplot(ok, aes(x = sddepth, y = mdepth))+
  geom_point()+
  geom_smooth()


pacman::p_load(caret)
data(mpg)
mpg_preprocess <- preProcess(mpg)
mpg_preprocess
#centred= sample mean, scale = observation/ sd
mpg <- predict(mpg_preprocess, mpg)
mpg

set.seed(2019)
df  <- tibble(
  treatment = rep(c("A", "B"), each = 10), 
  obs = rnorm(20)
)
df

arrange(df, obs)

df_preprocess  <- preProcess(df, method = "center")
df  <- predict(df_preprocess, df)
df
df %>%
  arrange(df)%>%
  filter(treatment == "A")

data("midwest")
midwest_pre <- preProcess(midwest, method="center")
midwest_pre
midwest1 <- predict(midwest_pre, midwest, method = "center")
sd(midwest1$area)

sd(midwest$area)

library(tidyverse)
data(table4a)
table4a
t_pre <- preProcess(table4a)
t_pre
tb <- predict(t_pre, table4a)
tb
diff_one <- table4a[2,2]-table4a[1,2]
diff_one
diff_two <- table4a[3,2]-table4a[2,2]
diff_two
diff_two/diff_one

x <- table4a%>%
  preProcess()%>%
  predict(table4a)
diff_one <- x[2,2]-x[1,2]
diff_one
diff_two <- x[3,2]-x[2,2]
diff_two
diff_two/diff_one

data(midwest)
ggplot(midwest, aes(x=area))+
  geom_boxplot()
ggplot(midwest1, aes(x=area))+
  geom_boxplot()

data("table1")
table1%>%
  preProcess()%>%
  predict(table1)%>%
  filter(country=="China")

library(tidyverse)
pacman::p_load(mlbench)
data(BreastCancer)
bc <- as_tibble(BreastCancer)
bc

bc <- na.omit(bc)
predictors <- bc%>%
  mutate_all(as.numeric)
predictors
PCA <- princomp(predictors, scores = TRUE) 
bc$PC1 <- PCA$scores[,1]
bc$PC2 <- PCA$scores[,2]
ggplot(bc, aes(PC1, PC2))+geom_point(aes(col=Class))

mpg
#get mean, sd, and population size
sample_mean <- mean(mpg$cty)
sample_sd <- sd(mpg$cty)
N <- length(mpg$cty)
#find cut off point student t-distribution
t <- qt(p=0.025, df=N-1, lower.tail = FALSE )
#2.5 in either end of the tail, leaving 95 in the middle
#calc lower point
lwr <- sample_mean - t*sample_sd/sqrt(N)
upr <- sample_mean + t*sample_sd/sqrt(N)
#look at interval
ci <- c(lwr = lwr, upr = upr)
ci

set.seed(2019)
midwest_sample <- sample_n(midwest, 50)
midwest_sample <- na.omit(midwest_sample)
midwest_mean <- mean(midwest_sample$poptotal)
mean(midwest$poptotal)
midwest_sd <- sd(midwest_sample$poptotal)
t <- qt( p = 0.1, df= 50-1, lower.tail = FALSE)
t
lower_bound <- midwest_mean-t*midwest_sd/sqrt(50)
lower_bound
upper_bound <- midwest_mean+t*midwest_sd/sqrt(50)
upper_bound

#week 4
data(mpg)
ggplot(mpg, aes(x=displ, y= hwy))+
  geom_point(aes(col = class)) +
  geom_smooth(se=FALSE) +
  labs(title = "Write what the observed trend is", 
       subtitle = "write exceptions", 
       caption = "write source",
       x= "engine displacement (L)",
       y = " highway fuel economy (mpg)")
#The labs setting

df <- tibble(x = runif(10), y = runif(10))
ggplot(df, aes(x, y))+
  geom_point()+
  labs(
    x = quote(sum(x[i]^2, i==1, n)),
    y = quote(alpha + beta+ frac(delta, theta)
    )
  )
#You can input mathematical functions as axis titles as above

best_in_class <-  mpg%>%
  group_by(class) %>%
  filter(row_number(desc(hwy))==1)
ggplot(mpg, aes(displ, hwy))+
  geom_point(aes(col= class))+
  geom_text(aes(label=model), data = best_in_class)
#or
ggplot(mpg, aes(displ, hwy))+
  geom_point(aes(col= class))+
  geom_label(aes(label=model), data = best_in_class, nudge_y = 2, nudge_x = 0.5)
#or
install.packages("ggrepel")
ggplot(mpg, aes(displ, hwy))+
  geom_point(aes(col = class))+
  ggrepel::geom_label_repel(aes(label=model), data = best_in_class)

class_average <- mpg%>%
  group_by(class)%>%
  summarize(displ=median(displ), hwy = median(hwy))

ggplot(mpg, aes(x = displ, y = hwy, colour = class))+
  ggrepel::geom_label_repel(aes(label = class),
  data = class_average, size = 6, label.size = 0, segment.color = NA) +
  geom_point() +
  theme(legend.position = "none")
#labelled without legend

label <- mpg %>%
  summarize( displ = max(displ), hwy = max(hwy), label = paste( "increasing engine size is \nrelated to decreasing fuel economy"))
ggplot(mpg, aes(displ, hwy))+
  geom_point()+
  geom_text(aes(label = label), data = label, vjust= "top", hjust="right")
#text in graph

label <-  tibble(displ = Inf, hwy = Inf, label = paste("increasing engine size is \nrelated to decreasing fuel economy"))
ggplot(mpg, aes(displ, hwy))+
  geom_point()+
  geom_text(aes(label = label), data = label, vjust= "top", hjust="right")
#top, center, bottom, and left, center, right


"increasing engine size is related to decreasing fuel economy" %>%
  stringr::str_wrap(width = 40) %>%
  writeLines()
# set the amount of characters before a line break

label <-  tibble(displ = Inf, hwy = Inf, label = paste("increasing engine size is \nrelated to decreasing fuel economy"))
ggplot(mpg, aes(displ, hwy))+
  geom_point()+
  geom_hline(size=2, yintercept = 5, colour= "white")+
  geom_text(aes(label = label), data = label, vjust= "top", hjust="right")
# put in horizontal line

label <-  tibble(displ = Inf, hwy = Inf, label = paste("increasing engine size is \nrelated to decreasing fuel economy"))
ggplot(mpg, aes(displ, hwy))+
  geom_rect(xmin= 2, ymin= 15, xmax = 4, ymax =20)+
  geom_point()+
  geom_text(aes(label = label), data = label, vjust= "top", hjust="right")
# places rectangle around points of interest

label <-  tibble(displ = Inf, hwy = Inf, label = paste("increasing engine size is \nrelated to decreasing fuel economy"))
ggplot(mpg, aes(displ, hwy))+
  geom_point()+
  geom_segment(x=4, y=10, xend =7, yend=40, size =0.5)+
  geom_text(aes(label = label), data = label, vjust= "top", hjust="right")
#draws a line between two coordinates, can be made to equal a tibble value

ggplot(mpg, aes(displ, hwy)) + 
  geom_point() +
  scale_y_continuous(breaks = seq(15, 40, by = 5))
#sets the y and x scales

ggplot(mpg, aes(displ, hwy))+
  geom_point()+
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL)
# removes scale numbers 

presidential %>%
  mutate(id =33+row_number())%>%
  ggplot(aes(start, id))+
  geom_point()+
  geom_segment(aes(xend = end, yend = id))+
  scale_x_date(
    NULL,
    breaks = presidential$start, 
    date_labels = "'%y")
#puts segment lines indicating duration, x axis has scales marking those specific times

ggplot(mpg, aes(displ, hwy))+
  geom_point(aes(col=class))+
  geom_smooth(se = FALSE)+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(nrow = 2, override.aes = list(size=4)))
#legend alterations

data(diamonds)
ggplot(diamonds, aes(carat, price))+
  geom_bin2d()

ggplot(diamonds, aes(log10(carat), log10(price)))+
  geom_bin2d()
#comparing normal, to log scaled

ggplot(diamonds, aes(carat, price))+
  geom_bin2d()+
  scale_x_log10()+
  scale_y_log10()
# changes where the log is applied, to the data or to the output

ggplot(mpg, aes(displ, hwy))+
  geom_point(aes(col=drv, shape =drv))+
  scale_color_brewer(palette= "Set1")
#changes colours that are optimum for colour blind viewers, shape changes also help

presidential %>%
  mutate(id= 33+row_number())%>%
  ggplot(aes(start,id, colour= party))+
  geom_point()+
  geom_segment(aes(xend = end, yend = id))+
  scale_colour_manual(values = c(Republican = "red", Democratic = "blue"))
#assigns specific colour by attribute value

df <- tibble( x= rnorm(1000), y= rnorm(1000))
ggplot(df, aes(x,y))+
  geom_hex()+
  coord_fixed()
#Vs
install.packages("viridis")
ggplot(df, aes(x,y))+
  geom_hex()+
  viridis::scale_fill_viridis()+
  coord_fixed()
#palette with good optical properties

ggplot(mpg,mapping = aes(displ,hwy))+ 
  geom_point(aes(col=class))+
  geom_smooth()+
  coord_cartesian(xlim = c(5,7), ylim = c(10,30))
#vs
mpg%>%
  filter(displ >= 5, displ<= 7, hwy >=10, hwy<=30)%>%
  ggplot(aes(displ, hwy))+
  geom_point(aes(col=class))+
  geom_smooth(se = FALSE)
#zooming

suv <- mpg %>% filter(class == "suv") 
compact <- mpg %>% filter(class == "compact")
x_scale <- scale_x_continuous(limits = range(mpg$displ)) 
y_scale <- scale_y_continuous(limits = range(mpg$hwy)) 
col_scale <- scale_color_discrete(limits = unique(mpg$drv)) 
ggplot(suv, aes(displ, hwy, color = drv)) + 
  geom_point() + 
  x_scale + 
  y_scale + 
  col_scale 

ggplot(compact, aes(displ, hwy, color = drv)) + 
  geom_point() + 
  x_scale + 
  y_scale +
  col_scale

#sets comparable cartesian plane

?theme
#the 8 avaliable ggplot themes, more available from install. packages("ggthemes")
#ggsave() will save most recent plot
#sizing: fig.width (4,6, or 7), fig.height , fig.asp (0.628 is the golden ratio), out.width (70% for 1 or 100/n% if you want multiple side by side), and out.height . Wickham only uses fig.width. out.width, 

pacman::p_load(mlbench)
data(BostonHousing2)
?BostonHousing2
ggplot(BostonHousing2, aes(rm, medv))+
  geom_point()+
  labs(x ="median house price", y = "rooms per dwelling")

ggplot(diamonds, aes(carat, price))+
  geom_bin2d()

diamond_lm <-  lm(log(price) ~ log(carat), data = diamonds)
#lm(response ~ predictor, data = data_set)
summary(diamond_lm)

data(mpg)
ggplot(mpg, aes(cty, displ))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()

mpg_lm <- lm(cty~displ, data = mpg)
summary(mpg_lm)

data(msleep)
ggplot(msleep, aes(sleep_total, sleep_rem))+
  geom_point
sleep_lm <- lm(sleep_rem ~sleep_total, data= msleep)
summary(sleep_lm)
5.466+(8*2.6248)
-0.36+8*0.22
2*0.22

data("midwest")
ggplot(midwest, aes(poptotal, percollege))+
  geom_point()+
  scale_x_log10()
mw_lm <- lm(percollege ~ log(poptotal), data = midwest )
summary(mw_lm)
#percollege = -17.0230+3.3278*log10poptotal
-17.0230+(3.3278*log(1000000))
3.3278*log(2)

plot(mw_lm, which = 1)
plot(mw_lm, which = 3)
plot(mw_lm, which = 2)
?midwest

plot(sleep_lm, which = 1)
plot(sleep_lm, which = 3)
plot(sleep_lm, which = 2)

mpg_lm <- lm(hwy ~ cty, data = mpg)
plot(mpg_lm, which = 1)
plot(mpg_lm, which = 3)
plot(mpg_lm, which = 2)
?mpg

pacman::p_load(mplot)
data(fev)
fev_lm <- lm(fev~height, data = fev)
summary(fev_lm)
new_data <- tibble(height = 60)
predict(fev_lm, new_data, interval = "confidence")
#give a range of the fev for all age groups with a height of 60inch (plural)
predict(fev_lm, new_data, interval = "prediction")
#give a range of fev for a single person of height 60 inch (singular)
mpg_lm2 <- lm(cty ~ displ, data = mpg)
fourlitre <- tibble(displ = 4)
predict(mpg_lm2, fourlitre, interval = "confidence")
predict(mpg_lm2, fourlitre, interval = "prediction")

mpg_lm3 <- lm(hwy~cty, data = mpg)
t1 <- tibble(cty = 17)
predict(mpg_lm3, t1)
predict(mpg_lm3, t1, interval = "confidence")
predict(mpg_lm3, t1, interval = "prediction", level=1)

t2 <- tibble(cty = 19)
predict(mpg_lm3, t2)
predict(mpg_lm3, t2, interval = "confidence")
predict(mpg_lm3, t2, interval = "prediction")

mpg%>%
  filter(cty ==17)%>%
  count()
mpg%>%
  filter(hwy<=20, hwy>=27)%>%
  count()
plot(mpg$cty,mpg$hwy)
27.0886-20.16898
22.84202-29.76537
20.72879-26.52878

data(trees)
head(trees)
ggplot(trees, aes(Girth, Volume))+geom_point()
trees_lm <- lm(Volume ~ Girth, data = trees)
summary(trees_lm)
plot(trees_lm, which = 1)
plot(trees_lm, which = 3)
plot(trees_lm, which = 2)

pacman::p_load(caret)
BoxCoxTrans(trees$Volume, trees$Girth)
trees <- trees %>%
  mutate(Volume_transform = (Volume^(0.4) - 1) / 0.4)
head(trees)
trees_transformed_lm <- lm(Volume_transform ~ Girth, data = trees)
plot(trees_transformed_lm, which = 1)
plot(trees_transformed_lm, which = 3)
plot(trees_transformed_lm, which = 2)
summary(trees_transformed_lm)

set.seed(2019)
df  <- tibble(
  x = seq(1,5, length = 100), 
  y = exp(x) + rnorm(100)
)
df
w4test <- lm(y~x, data = df)
plot(w4test, which = 1)
plot(w4test, which = 3)
plot(w4test, which = 2)
xBoxCoxTrans(df$y, df$x)
df <- df%>%
  mutate(trans_y = (y^(0.1)-1)/0.1)
head(df)
w4test <- lm(trans_y~x , data = df)

#week 5
dt_team  <- tribble(
  ~Person, ~Pet,
  "Lewis", "Cat",
  "Sash", "Cat",
  "Leah", "Dog",
  "Jono", "Rabbit",
  "Cath", "Rabbit",
  "Lachlann", "Dog"
)
dt_team
dt_team <- mutate(dt_team, pet = as.factor(Pet))
dt_team
pacman::p_load(modelr)
model_matrix(dt_team, ~pet)
#binary matrix

data("chickwts")
chickwts <- as_tibble(chickwts)
chickwts
ggplot(chickwts, aes(feed, weight))+
  geom_boxplot()
chickwts_lm <- lm(weight~feed, data = chickwts)
summary(chickwts_lm)
#one of the feed is taken as a reference categorical, 
#it took the fist level alphabetical one
#We can compare feeds to caesin, how about comparing feeds to weight? ANOVA
anova(chickwts_lm)

?midwest
ggplot(midwest, aes(state, percollege))+
  geom_boxplot()
midwest_lm <- lm(percollege~state, data = midwest)
summary(midwest_lm)
model_matrix(midwest, ~state)
anova(midwest_lm)
new_data <- midwest%>%
  filter(state == "WI")
jjj <- predict(midwest_lm, new_data, interval = "confidence")

?diamonds
ggplot(diamonds, aes(x= x, y = price))+geom_hex()
# for better models add more terms: yi=β0+(β1xi1)+(β2xi2)+ϵi
#in this case carat, and length of diamond appear to influence price, now model that
diamonds_lm2 <- lm(log(price)~log(carat)+x, data= diamonds)
summary(diamonds_lm2)
#a regression model can be made using these coeficients
#but is there a relationship between length and carat to begin with that also influences price?
diamonds%>%
  group_by(carat)%>%
  ggplot(diamonds, mapping = aes(x = carat, y = x)) +
           geom_hex()
#appears to be so, add in a relationship expression: β3(xi1)(xi2)
diamond_lm3 <- lm(log(price)~log(carat)+x+log(carat):x, data = diamonds)
summary(diamond_lm3)
#that relationship is significant, ***


mpg_lm4 <- lm(cty ~ displ + cyl, data= mpg)
summary(mpg_lm4)
#y=(28.2885-1.1979)*(displ)+(28.2885-1.2347)*(cyl)+intercept+ error
#or:
predict(mpg_lm4, newdata = tibble(displ=2, cyl = 6))

midwest_lm1 <- lm(percollege ~ log(poptotal)+ percbelowpoverty, data=midwest)
predict(midwest_lm1,newdata=tibble(poptotal=100000, percbelowpoverty=15), interval = "prediction")
12.94
-0.15*3
log(10)*3.13
midwest_lm1 <- lm(percollege ~ log(poptotal) + percbelowpoverty + log(poptotal):percbelowpoverty, data = midwest)
predict(midwest_lm1,newdata =tibble(poptotal=100000, percbelowpoverty=15), interval = "prediction")
midwest_lm2 <- lm(percollege ~ area + percwhite +log(popdensity), data = midwest)
summary(midwest_lm2)

#ANCOVA
mpg
mpg <- mpg%>%
  mutate(trans=ifelse(str_detect(trans,"auto"),"auto", "manual"))
ggplot(mpg, aes(x=displ, hwy, col=trans))+
  geom_point()+
  labs(y= "fuel efficiency", x= "displacement")+
  scale_color_brewer(palette="Set1")
#Seperate lines model
mpg_sep <- lm(hwy~trans + displ + trans:displ, data=mpg)
summary(mpg_sep)
ggplot(mpg, aes(displ,hwy, col=trans))+
  geom_point()+
  labs(y="fuel efficiency", x= "displacement")+
  scale_color_brewer(palette= "Set1")+
  geom_smooth(method = "lm", se=FALSE)
#fairly parallel, do they need to be seperate:
anova(mpg_sep)
#trans:displ >0.05 so not a significant term. Can be removed from equation

#Parallel lines model
mpg_par <- lm(hwy~trans +displ, data=mpg)
summary(mpg_par)
anova(mpg_par)
#appear parallel but both terms are found to be significant indicating they have different slopes

#identical lines model
mpg_iden <- lm(hwy~displ, data=mpg)
summary(mpg_iden)
ggplot(mpg, aes(displ, hwy, col = trans))+
  geom_point()+
  geom_smooth(aes(group=1), method="lm", se=FALSE, col = "black")+
  scale_colour_brewer(palette="Set1")

diamonds
diamonds <- diamonds %>%
  filter(cut %in% c("Fair", "Ideal"))
diamonds%>%
  mutate(cut = factor(cut, ordered= FALSE))
diamonds_iden <- lm(price~carat, data=diamonds)
summary(diamonds_iden)         
ggplot(diamonds, aes(carat, price, col= cut))+
  geom_point()+
  geom_smooth(aes(group=1), method="lm", se=FALSE, col="black")+
  scale_colour_brewer(palette = "Set1")

diamonds_par <- lm(price~carat+cut, data=diamonds)
summary(diamonds_par)
anova(diamonds_par)
ggplot(diamonds, aes(carat,price, col=cut))+
  geom_point()+
  labs(y="price", x= "carat")+
  scale_color_brewer(palette= "Set1")+
  geom_smooth(method = "lm", se=FALSE)

diamonds_sep <- lm(price ~ carat + cut + carat:cut, data=diamonds)
summary(diamonds_sep)
anova(diamonds_sep)
ggplot(diamonds, aes(carat, price, col=cut))+
  geom_point()+
  scale_colour_brewer(palette="Set1")+
  geom_smooth(method="lm", se=FALSE)

library(tidyverse)
data(midwest)
midwest$state <- as.factor(midwest$state)
midwest_par <- lm(percollege~log(poptotal)+state, data=midwest)
summary(midwest_par)
anova(midwest_par)
midwest_sep <- lm(percollege~log(poptotal)+state+log(poptotal):state, data=midwest)
summary(midwest_sep)
anova(midwest_sep)
predict(midwest_sep, newdata = tibble(poptotal=10000, state="OH"), interval = "prediction")
ggplot(midwest, aes(x =log(poptotal), percollege, col = state)) + 
  geom_point()  + 
  geom_smooth(method = "lm", se=FALSE)+
  scale_color_brewer(palette = "Set1")

#week 6
data(flights)
colnames(flights)
unique(flights$month)
?flights
#May must be 5
flights$airlines
data(airlines)
airlines%>%
  filter(carrier == "UA")

flights %>%
  filter(month==4)%>%
  unique()

summary(flights$distance)
unique(flights$tailnum)
where <- flights%>%
  filter(distance =="4983")

flights$tailnum %>% 
  unique() %>% 
  length()

?length()
flights %>%
  filter(dep_delay < 0)
flights %>%
  filter(arr_delay < 0)

airports %>%
  filter(name = "Airport")
?stringr

summary(str_detect(airports$name, "Airport"))

us_flights <- flights %>%
  filter(carrier == "UA")
unique(us_flights$dest)

pacman::p_load("inspectdf")
inspect_num(flights)
#for summary of all
inspect_types(flights) %>% show_plot()
#for type integer, character, double, factor, ect.
inspect_imb(flights)
#for most common type
inspect_na(flights)

inspect_num(flights)

ggplot(flights, aes(dep_delay))+
  geom_histogram()

ggplot(flights, aes(log(dep_delay)))+
  geom_histogram()

ggplot(flights, aes(log(dep_delay)))+
  geom_boxplot()

inspect_num(flights)

table(flights$origin)
flights
104662/336776
ggplot(flights, aes(log(distance)))+
  geom_histogram()

flights%>%
  filter(month<=6) %>%
  ggplot(aes(x = carrier))+
  geom_bar()

flyboy <- flights %>%
  select(dep_delay, month, origin, carrier)

flyboy <- flyboy%>%
  mutate(month = factor(month))

ggplot(flyboy, aes(x= dep_delay, y= month))+ geom_boxplot()

flyboy <- flyboy%>%
  filter(dep_delay > 0)%>%
  mutate(log_delay = log(dep_delay))
ggplot(flyboy, aes(log_delay, month))+ geom_boxplot()
colname(flights)
ggplot(flights, aes(dep_delay, sched_dep_time ))+ geom_point()

flights%>%
  group_by(origin)%>%
  ggplot(aes(dep_time, col = origin))+geom_boxplot()

flights%>%
  filter(arr_delay > 30)

flights$arr_late <- flights$arr_delay >30
flights$dep_late <- flights$dep_delay >30
flights
summary(flights$arr_late)
summary(flights$dep_late)
count(filter(flights, dep_late == "FALSE", arr_late == "TRUE"))
count(filter(flights, dep_late == "TRUE", arr_late == "FALSE"))

ggplot(flights, aes(arr_time, dep_time))+geom_point()
data(flights)
ggplot(flights, aes( x= origin, fill = carrier))+ geom_bar()
unique(airports$name)
flights$month <- factor(flights$month)
flights <- flights %>%
  filter(dep_delay > 0)%>%
  mutate(log_delay = log(dep_delay))
ggplot(flights, aes(x= month, y = log_delay))+ geom_boxplot()

flights <- flights %>%
  filter(arr_delay > 0)%>%
  mutate(log_adelay = log(arr_delay))
ggplot(flights, aes(x=month, y= log_adelay))+geom_boxplot()

ggplot(flights, aes(x= carrier, y= log(log_delay)))+geom_boxplot()


m1 <-  lm(log_delay~month+ dep_time + origin + carrier, data = flights)
m2 <-  lm(log_delay~month+ dep_time + origin + carrier + origin:dep_time, data = flights)
anova(m2)
#correlation is significant between origin and dep_time

m3 <- lm(log_delay ~ month + carrier + month:carrier, data = flights)
summary(m3)
anova(m3)
library(tidyverse)
library(dplyr)
library(stringr)
data(flights)
flights <- flights %>%
  filter(dep_delay > 0)%>%
  mutate(log_delay = log(dep_delay))
flights$month <- factor(flights$month)
q1 <- lm(log_delay ~ month, data = flights)
summary(q1)
2.72185 + 0.35894 
2.72185+-0.15459
predict(q1, newdata = tibble(month = "11"))
2.71828182846^2.567252
q5 <- lm(log_delay ~ dep_time, data = flights)
summary(q5)        
0.0008524*30
2.71828182846^ 0.025572
question_seven <- lm(log_delay ~ dep_time + month + dep_time:month, data = flights)
anova(question_seven)
summary(question_seven)
1.562e+00+-5.328e-02+5.901e-05
7.833e-04+5.901e-05 
0.00084231
q10 <- lm(log_delay ~ origin + carrier + origin:carrier, data = flights)
summary(q10)

quiz <- lm(log_delay ~ month + dep_time + origin + carrier + dep_time:origin, data = flights)
summary(quiz)

new_data  <- tibble(
  month = "8", 
  dep_time = 1100,
  origin = "JFK", 
  carrier = "EV")
predict(quiz, newdata = new_data)
exp(2.80092)
predict(quiz, newdata = new_data, interval="prediction")
exp(2.8)
exp(5.389)
218/60

unique(flights$origin)
new_data  <- tibble(
  month = "2", 
  dep_time = 1500,
  origin = "LGA", 
  carrier = "AS")
predict(quiz, newdata = new_data, interval = "confidence")%>%
  exp() 

unique(flights$origin)
flighta  <- tibble(
  month = "3", 
  dep_time = 0620,
  origin = "LGA", 
  carrier = "EV")
predict(quiz, newdata = flighta, interval = "prediction")
flightb  <- tibble(
  month = "3", 
  dep_time = 0710,
  origin = "JFK", 
  carrier = "HA")
predict(quiz, newdata = flightb, interval = "prediction")
exp(2.416153)
#for flight A = 11.2min delay
exp(1.952629)
#flight b = 7.05 min delay
exp(2.460393 )
#at best flight A can leave with a 10min delay at 0630, at worst 0631
exp(4.560411)
#at best flight b can leave with a 5.16min delay at 0715, at worst 0720

flightx  <- tibble(
  month = "12", 
  dep_time = 1900,
  origin = "EWR", 
  carrier = "AA")
predict(quiz, newdata = flightx)
#JFK = 2.859
#LGA = 3.235

inspect_num(flights)
del <- lm(log_delay~day+month+sched_dep_time+origin+carrier+distance+day:month+origin:carrier+day:sched_dep_time, data=flights)
summary(del)
anova(del)

del2 <- lm(log_delay~month+sched_dep_time+origin+carrier+distance+day:month+origin:carrier, data=flights)
anova(del2)
