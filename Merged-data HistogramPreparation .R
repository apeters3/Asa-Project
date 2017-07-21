
#MERGED DATA HISTOGRAM PREP

#July 2nd, 2017, by Asa Peters
#CLeaning up data frame

# Housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)
graphics.off()

# Load Libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(gridExtra)

# Set Working Directory and Load dataframe
setwd("/Users/wolkolab/Asa Project")
d<-read.csv("Vassal_cleaned2.csv",header=TRUE)

# Clean dataframe
d<-dplyr::select(d,variety,BB,FLOW,VER,MAT)
df<-gather(d,event,doy,-variety) # condense data into 3 columns - one with event name and one with event day of year
df$doy<-as.character(as.Date(df$doy, "%d/%m/%Y")) # clean up column to recognize as Date in R
df$day<-yday(as.Date(df$doy)) # convert date to day of year
df$year<-substr(df$doy,1,4) # just gather year in new column
df<-dplyr::select(df,-doy) # remove date column
df<-na.omit(df) # clean and remove NAs
df$mean<-ave(df$day,df$variety,df$event,FUN=mean) # find the mean day of year for each event

#write.csv(df, file="/Users/wolkolab/Asa Project/vassal_hist2.csv", row.names=FALSE)


#Finding Histogram Outliers (where BB=MAT )
bb<-d%>%dplyr::select(variety, BB, MAT)
bb$BB<-as.Date(bb$BB)
bb$BB<-strftime(bb$BB, format="%j")
bb$MAT<-as.Date(bb$MAT)
bb$MAT<-strftime(bb$MAT, format="%j")
check<-bb[which(bb$BB==bb$MAT),]
unique(check$variety)

#vassal.hist2<-read.csv("vassal_hist2.csv",header=T,sep=",",as.is=T)#

outs<-c("Listan", "CarignanNoir","Sauvignon rose","Sangiovese","Gamay noir")
subs<-vassal.hist2%>%filter(variety %in% outs)




vassal.hist2<-read.csv("vassal_hist2.csv",header=T,sep=",",as.is=T)
#Subsetting Unique Values from "Mean" BB column
varietyRows<-c(1,155,347,476,496,548,598,633,704,732,764,788,799,851,931,1303,1511,2204,2372,2432,2843,3228,3443,3650,3666,3707,3737,3774,3807,3866,3896,3920,3967,3974,4007,4053,4057,4063,4069,4077,4081,4085,4103,4116,4130,4141,4153,4159,4163,4169,4181,4187,4191,4248,4251,4256,4271,4280,4283,4293,4304,4308,4311,4318,4321,4326,4329,4346,4361,4366,4405,4420,4469,4473,4531,4552,4555,4572,4587,4589,4602,4606,4610,4613,4625,4628,4632,4643,4752,4755,4761,4766,4771,4775,4784,4788,4821,4852,4872,4893,4960,5122,5376,5395,5398,5412,5607,5634,5665,5689,5736,5759,5774,5816,5874,5906,5964,5968,5975,5979,5989,6023,6030,6033,6037,6040,6053)

vassal.hist2$mean[c(varietyRows)]->vassal.hist2.BBUnique #subset budburst dates from vassal.hist

vassal.hist2$variety[c(varietyRows)]->vassal.hist2.variety

as.data.frame(vassal.hist2.variety)->vassal.hist2.variety #make unique varieties a data frame

vassal.hist2.variety$BBMean<-vassal.hist2.BBUnique

names(vassal.hist2.variety)[names(vassal.hist2.variety) == "vassal.hist2.variety"] <- "Variety"





vassal.hist2.variety



annotation_custom(tableGrob(mytable), xmin=35, xmax=50, ymin=-2.5, ymax=-1)
table(vassal.hist.variety)

#Making Histogram from vassal.hist.BBUnique with qplot
par(mfrow=c(1,2))
q<-qplot(BBMean,data=vassal.hist2.variety,xlab="Budburst DOY Mean",ylab="Frequency",main="Average Budburst Days of Year",binwidth=3,cex=1.5)
q
outliers<-vassal.hist2.variety%>%filter(BBMean>=200)

VassalBBMeans<-vassal.hist2.variety$BBMean
VassalBBVarieties<-vassal.hist2.variety$Variety
e<-ggplot(vassal.hist2.variety,aes(BBmean,VassalBBVarieties))
e+geom_point(aes(x=BBMean,y=VassalBBVarieties),size=2,color="red")+theme(panel.background=element_rect(fill="white",color="lightblue",size=0.5,linetype="solid"),panel.grid.major=element_line(size=0.5,linetype = "solid",colour = "black"),axis.text.y = element_text(face="bold", color="black"))+labs(x="Mean Budburst Day of Year by Variety",y="Variety")+ggtitle("Mean Budburst Day of Year")+scale_x_continuous(breaks=seq(0,250,10))


annotation_custom(grid.table(vassal.hist.variety,rows=NULL))xmin=35, xmax=50, ymin=-2.5, ymax=-1)
tab<-annotation_custom(grid.table(vassal.hist.variety,rows=NULL),ttheme_default(base_size=5))
