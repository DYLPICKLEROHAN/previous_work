library(tidyverse)
mpg
# chapter 1
#shows the data frame, write ?mpg for info about the data + examples
#ctrl + enter = run
ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy))
#plots the two attributes
ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy,colour=class))
#sets aesthetiv of each point to indicate what class each point belongs to
ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy),colour="blue")
#makes all points blue
ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy))+facet_wrap(~class,nrow=2)
ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy))+facet_wrap(~class,ncol=2)
#introduces facets, multiple graphs representing an attribute across two other variables. basically seperates subgroups. Rows and columns
ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy))+facet_grid(drv~cyl)
#introduces two concurrent variables along the top and sides of the screen
ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy))+facet_grid(drv ~ .)
#the '.' leaves one axis blank: facet_grid(x,y)

ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy))
ggplot(data=mpg)+geom_smooth(mapping=aes(x=displ,y=hwy))
#points vs smooth linear line
ggplot(data=mpg)+geom_smooth(mapping=aes(x=displ, y=hwy, linetype=drv))
#separated data by drive type as visualized through line differences
#http://rstudio.com/cheatsheets shows extensive list of geoms
ggplot(data=mpg)+geom_smooth(mapping=aes(x=displ,y=hwy))
ggplot(data=mpg)+geom_smooth(mapping=aes(x=displ,y=hwy, group=drv))
#together, and seperated by group
ggplot(data=mpg)+geom_smooth(mapping=aes(x=displ,y=hwy, colour=drv),show.legend=FALSE)
#break groups up and alter colours
ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy))+geom_smooth(mapping=aes(x=displ,y=hwy))
#plots point and linear overlay
ggplot(data=mpg, mapping=aes(x=displ, y=hwy))+geom_point(mapping=aes(colour=class))+geom_smooth(data=filter(mpg, class=="suv"),se=FALSE)
#se=false hides deviation
ggplot(data=mpg, mapping=aes(x=displ, y=hwy))+geom_point()+geom_smooth()
ggplot()+geom_point(data=mpg, mapping=aes(x=displ, y=hwy))+geom_smooth(data=mpg, mapping=aes(x=displ, y=hwy))
#made no difference, both visualize the same thing
ggplot()+geom_point(data=mpg, mapping=aes(x=displ, y=hwy), colour="white", stroke=4)+geom_point(data=mpg, mapping=aes(x=displ, y=hwy, colour=drv), stroke=2)
#took a whiule but figured out that the layers over lap, so first one down will be at the back

#Chapter 2

ggplot(data=diamonds)+geom_bar(mapping=aes(x=cut))
#bar chart of diamond cut, count is not a diamond variable its counting the bin totals accumulated in the transformation
?geom_bar
ggplot(data=diamonds)+stat_count(mapping=aes(x=cut))
#the way the data is compiled means geom_bar and stat_count visualize the same thing. You would only really use stat if you wanted to change variables, show propotions instead of counts, or show a stat summary with means and standard deviations:
demo<- tribble(~a, ~b, "bar 1", 20, "bar 2", 30, "bar 3", 40,)
ggplot(data=demo)+geom_bar(mapping=aes(x=a, y=b), stat="identity")

ggplot(data=diamonds)+geom_bar(mapping=aes(x=cut, y=..prop.., group=1))
#proportions of group 1
ggplot(data=diamonds)+stat_summary(mapping=aes(x=cut, y=depth), fun.ymin=min, fun.ymax=max, fun.y=median)
#median & standard deviations
ggplot(data=diamonds)+geom_bar(mapping=aes(x=cut, fill= color))
?diamonds
#stacked bar chart also revealing colour breakdown
#position adjustments
ggplot(data=diamonds)+geom_bar(mapping=aes(x=cut, colour=cut))
ggplot(data=diamonds)+geom_bar(mapping=aes(x=cut, fill=cut))
#colour options
ggplot(data=diamonds)+geom_bar(mapping=aes(x=cut, fill=clarity))
#stacked bar chart
#position="identity" replaces eachobject exactly where it appears in the context of the graph not all that useful for bar charts as its exactly the same, alpha= transparency
ggplot(data=diamonds, mapping=aes(x=cut, fill=clarity))+geom_bar(alpha=1/5, position="identity")
ggplot(data=diamonds, mapping=aes(x=cut, color=clarity))+geom_bar(fill= NA, position = "identity")
#position= "fill" makes each graph the same height so proportions cn be clearly shown
ggplot(data=diamonds)+geom_bar(mapping=aes(x=cut, fill= clarity), position="fill")
#position="dodge" breaks each bar into seperate bar charts of an attribute,
ggplot(data=diamonds)+geom_bar(mapping=aes(x=cut, fill=clarity), position="dodge")
#position="jitter", or geom_jitter), will add a little noise to prevent individual plots from occupying the same space
ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy), position="jitter")
ggplot(data=mpg)+geom_jitter(mapping=aes(x=displ,y=hwy))
#geom_count seems to cluster plots 
ggplot(data=mpg)+geom_count(mapping=aes(x=displ,y=hwy))
#removes outliers and creates a box plot of remaining data
ggplot(data=mpg)+geom_boxplot(mapping=aes(x=displ,y=hwy))
#coord_flip switches the x and y axis, useful for side on box plots and long labels
ggplot(data=mpg, mapping=aes(x=class, y=hwy))+geom_boxplot()
#vs
ggplot(data=mpg, mapping=aes(x=class, y=hwy))+geom_boxplot()+coord_flip()
#coord_quickmap sorts out the aspect ratio of maps
nz <-map_data("nz")
1
ggplot(nz, aes(long, lat, group=group))+geom_polygon(fill="white", color="black")
#vs
ggplot(nz, aes(long, lat, group=group))+geom_polygon(fill="white", color="black")+coord_quickmap()
ggplot(nz, aes(long, lat, group=group))+geom_polygon(fill="white", color="black")+coord_map()
?coord_map
#quickmaps preserves straight lines, maps does not. quick maps works best for small areas around the equator. not built for large vcurved areas
#coord_polar() reveals connection between box plot and coxcomb, pizza slice looking thing
bar<-ggplot(data=diamonds)+geom_bar(mapping=aes(x=cut, fill=cut), show.legend=FALSE, width=1)+theme(aspect.ratio=1)+labs(x=NULL, y=NULL)
bar +coord_flip()
bar +coord_polar()
?labs
#labs refers to labels

#template
ggplot(data=<DATA>)+<GEOM_FUNCTION>(mapping=aes(<MAPPING.),stat=<STAT>, position=<POSITION>)+<COORDINATE_FUNCTION>+<FACET_FUNCTION>
#7 parameters
#chapter 2
#start typing a command in the console below, pres tab and hover over the one you want, there will be a window showing what it does, press f1 for moreseq(1, 10)
seq(1, 10)
#lists the sequence 1 through 10
seq(1, 10, length.out=5)
# breaks the sequence 1-10 into 5 parts
x <- "hello world"
# to introduce an object use <- , but toassign and view the sequence at the same time place it in () as below
x <- seq(1, 10)
(x <- seq(1,10))
#press alt + shift + K

#Chapter 4
getwd()
#above code takes you to current directory
