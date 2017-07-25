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
  d<-read.csv("Vassal_cleaned.csv",header=TRUE)

# Clean dataframe
d<-dplyr::select(d,variety,BB,FLOW,VER,MAT)
df<-gather(d,event,doy,-variety) # condense data into 3 columns - one with event name and one with event day of year
df$doy<-as.character(as.Date(df$doy, "%d/%m/%Y")) # clean up column to recognize as Date in R
df$day<-yday(as.Date(df$doy)) # convert date to day of year
df$year<-substr(df$doy,1,4) # just gather year in new column
df<-dplyr::select(df,-doy) # remove date column
df<-na.omit(df) # clean and remove NAs
df$mean<-ave(df$day,df$variety,df$event,FUN=mean) # find the mean day of year for each event

#write.csv(df, file="/Users/wolkolab/Asa Project/vassal_hist.csv", row.names=FALSE)



vassal.hist<-read.csv("vassal_hist.csv",header=T,sep=",",as.is=T)



#Subsetting Unique Values from "Mean" BB column
varietyRows<-c(1,155,347,476,496,548,598,633,704,732,764,788,799,851,931,1303,1511,2204,2372,2432,2843,3228,3443,3650,3666,3707,3737,3774,3807,3866,3896,3920,3967,3974,4007,4053,4057,4063,4069,4077,4081,4085,4103,4116,4130,4141,4153,4159,4163,4169,4181,4187,4191,4248,4251,4256,4271,4280,4283,4293,4304,4308,4311,4318,4321,4326,4329,4346,4361,4366,4405,4420,4469,4473,4531,4552,4555,4572,4587,4589,4602,4606,4610,4613,4625,4628,4632,4643,4752,4755,4761,4766,4771,4775,4784,4788,4821,4852,4872,4893,4960,5122,5376,5395,5398,5412,5607,5634,5665,5689,5736,5759,5774,5816,5874,5906,5964,5968,5975,5979,5989,6023,6030,6033,6037,6040,6053)

vassal.hist$mean[c(varietyRows)]->vassal.hist.BBUnique #subset budburst dates from vassal.hist

vassal.hist$variety[c(varietyRows)]->vassal.hist.variety

as.data.frame(vassal.hist.variety)->vassal.hist.variety #make unique varieties a data frame

vassal.hist.variety$BBMean<-vassal.hist.BBUnique

names(vassal.hist.variety)[names(vassal.hist.variety) == "vassal.hist.variety"] <- "Variety"
      

vassal.hist.variety


hist(whatevs, xaxt='n') ## add the rest of arguments to make the histogram look pretty
position=c(67,75,100,…) ## fill with the values of the x axis of your original histogram
axis(1, at=position, labels=c(“March 1st”,”June 2nd”,”July 3rd”,…), cex.axis=1.25,l as=1) ## fill the labels content according to the desired dates.


#Making Histogram from vassal.hist.BBUnique with hist()
vassal.hist.variety2<-subset(vassal.hist.variety,BBMean<110)
hist(vassal.hist.variety2$BBMean,22,col="indianred4",ylim=c(0,15),main=NULL,xlab="Budburst Day of Year",cex.lab=1.5,font.lab=2,xaxt="n")
BBDOYPositions<-c(65,70,75,80,85,90,95)
axis(1,at=BBDOYPositions,labels=c("March 6th","March 11th","March 16th","March 21st","March 26th","March 31st","April 5th"),cex.axis=1.25,as=1)
chardonnay.value=vassal.hist.variety2[which(vassal.hist.variety2$Variety=='Chardonnay'),2]
arrows(chardonnay.value,10,chardonnay.value,6,lwd=4)
cabSauv.value=vassal.hist.variety2[which(vassal.hist.variety2$Variety=="Cabernet_Sauvignon"),2]
arrows(cabSauv.value,10,cabSauv.value,2,lwd=4)

text(77.07212,10.5,"Chardonnay",cex=1.25,font=2)
text(89.67708,10.5,"Cabernet Sauvignon",cex=1.25,font=2)
     
     
#Lineplot BB DOY by variety 
VassalBBMeans<-vassal.hist.variety$BBMean
VassalBBVarieties<-vassal.hist.variety$Variety
e<-ggplot(vassal.hist.variety,aes(BBmean,VassalBBVarieties))
e+geom_point(aes(x=BBMean,y=VassalBBVarieties),size=2,color="red")+theme(panel.background=element_rect(fill="white",color="lightblue",size=0.5,linetype="solid"),panel.grid.major=element_line(size=0.5,linetype = "solid",colour = "black"),axis.text.y = element_text(face="bold", color="black"))+labs(x="Mean Budburst Day of Year by Variety",y="Variety")+ggtitle("Mean Budburst Day of Year")+scale_x_continuous(breaks=seq(0,250,10))

 
