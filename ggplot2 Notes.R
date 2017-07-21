# Asa Peters, Monday July 10th 2017 #
# ggplot2 Notes #

#qplot-"quickplot", scatter,hist,boxplot possible. less customization

#qplot decides what kind of plot to make based on data entered 
# mpg data set qplot experimenting:
head(mpg)
#scatter plots
qplot(displ,hwy, data=mpg) #scatter plot of hwy mpg vs engine displacement
qplot(displ,hwy,data=mpg,color=drv,geom=c("point","smooth")) #scatterplot of hwy mpg vs engine displ. with points colored by drive type, w/ trendlines for each color, grey area represents 95% confidence interval
qplot(y=hwy,data=mpg,color=drv)#scatterplot of hwy mpg's, by drive type color, in order of data set appearance
qplot(displ,hwy, data=mpg,geom=c("point","smooth"),facets=.~drv) #note facets set equal to .~drv, making 3 panels seperated by drive type
#box and whisker plots
qplot(drv,hwy,data=mpg,geom="boxplot") #boxplot of drive type vs hwy mpg
qplot(drv,hwy,data=mpg,geom="boxplot",color=manufacturer) #boxplot of drive type vs hwy mpg colored by manufacterer

#histograms
qplot(hwy,data=mpg,fill=drv)#histogram of hwy mpg, with bars stacked colorwise by drive type

qplot(hwy,data=mpg, facets=drv ~ .,binwidth=2) #histograms of hwy mpg, seperated by drive type (stacked 3 by 1 in a column)

# ~. seperates by drive type (by 3), "~." connects to "drv"

#ggplot2 notes w/ ggplot command 

# Basic components of ggplot 2 plots: DATA FRAME, AESTHETIC MAP (color/size), GEOMS (points/lines/shapes),FACETS(panels), STATs(binning,quantiles,smoothing), SCALES(coding of aesthetic maps eg: male=red/ female=blue), COORDINATE SYSTEMS(what plots are depicted on)
#ggplot allows layering, addition to already made plot


g<-ggplot(mpg,aes(displ,hwy)) #like lattice, ggplot can assign plots to a variable. 
#ggplot cant automatically make a graph by printing "g"
#you must specify how you want the data represented. y

#Add functions to the variable/graph to customize:
g+geom_point() #creates the plot (specifies how to display the data). This command can also be assigned to a variable
g+geom_point()+geom_smooth() #creates the same plot, with a trendline and confidence interval in grey
g+geom_point()+geom_smooth(method="lm") #"method="lm"" means linear model, puts regression line thru data
g+geom_point()+geom_smooth(method="lm") + facet_grid(.~drv) # seperates data by drive type, (from facet_grid(.~drv)
g+geom_point()+geom_smooth(method="lm") + facet_grid(.~drv)+ggtitle("Swirl Rules!") #ggtitle adds title 

#Customizing ggplot plots appearance:
g+geom_point(color="pink", size=4, alpha=1/2) #note "alpha" determines shade of color
 
g+geom_point(size=4,alpha=1/2, aes(color=drv)) #point color is determined by drive type. 

g + geom_point(aes(color = drv)) + labs(title="Swirl Rules!") + labs(x="Displacement", y="Hwy Mileage") #"labs()"-label customization

g + geom_point(aes(color = drv),size=2,alpha=1/2) + geom_smooth(size=4,linetype=3,method="lm",se=FALSE) #geom_smooth specifies line size, dotted, linear regression, and no confidence intervals (in order)

g+geom_point(aes(color=drv))+theme_bw(base_family="Times") #no grey background, label font 


#customizing a ggplot scatterplot step by step
g<-ggplot(mpg,aes(x=displ,y=hwy,color=factor(year))) #displ vs. hwy mpg, colored by year 
g+geom_point() #makes a scatter plot of displ vs hwy mpg colored by year 
g + geom_point() + facet_grid(drv~cyl,margins=TRUE) #a 4x5 multipanel scatterplot, seperated by drive type and cylinder number
g + geom_point() + facet_grid(drv~cyl,margins=TRUE) +geom_smooth(method="lm",se=FALSE,size=2, color="black")# w/ trendlines/regression lines, no confidence intervals)
g + geom_point() + facet_grid(drv~cyl,margins=TRUE) +geom_smooth(method="lm",se=FALSE,size=2, color="black")+labs(x="Displacement",y="Highway Mileage", title="Swirl Rules!") #adding labels












