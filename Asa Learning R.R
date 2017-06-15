vassal<-read.csv("vassal_pheno.csv",header=TRUE,sep=",")

## house keeping
rm(list=ls())
options(stringsAsFactors = FALSE)

# converting date to day of the year

df$doy <- as.numeric(format(df$date , "%j")) # note that you must have the date column recognized by R as a date! this is useful for looking at pheno-event earliness 

## summarizing data #"factor 1" would be variety
DifferenceBudVerSummary<-ddply(vassal,c("vassal$variety"), summarise,
         mean = mean(vassal$DiffBudVer, na.rm = TRUE))
         sd = sd(DifferenceBudVer)
         sem = sd(DifferenceBudVer)/sqrt(length(DifferenceBudVer))
        
DifferenceBudVerSummary #all means are the same for each variety... what? is DifferenceBudVer not being really taken into account?

#New column made for DifferenceBudVer, new method for diffbudver
vassal$DiffBudVer <- vassal$ver_day - vassal$bud_day
head(vassal,20)
mean(vassal$DiffBudVer, na.rm=TRUE)




#plotting
vassal$variety_ID<-as.numeric(vassal$variety_ID)
hist(vassal$variety_ID, 30, xlim=c(0,100))
head(vassal$variety_ID)


#method of getting DIfference between bud and ver (not current)
vassal$DifferenceBudVer->difftime(vassal$ver_day,vassal$bud_day, units = "days")
vassal$bud_day<-as.Date(vassal$bud_day, format="%d/%m/%Y")
vassal$flow_day<-as.Date(vassal$flow_day, format="%d/%m/%Y")
vassal$ver_day<-as.Date(vassal$ver_day, format="%d/%m/%Y")

difftime(vassal$ver_day,vassal$bud_day, units = "days")->DifferenceBudVer


#########################notes####
class(DifferenceBudVer)
DifferenceBudVer.subset<-subset(vassal, !is.na(DifferenceBudVer))
?difftime
vassal$bud_day.subset<-subset(vassal, !is.na(vassal$bud_day))
vassal$ver_day.subset<-subset(vassal, !is.na(vassal$ver_day))

DifferenceBudVer.subset #should I remove NAs from vassal$bud_day and vassal$ver_day before carrying out difftime??
head(vassal$ver_day, 20)
class(vassal$bud_day)
head(DifferenceBudVer, 20)

DifferenceBudVer<-as.numeric(DifferenceBudVer)#makes the difftime results numeric so you can get the average length 
mean(DifferenceBudVer, na.rm=TRUE)? #omits NAs from mean calculation to give mean difference. this is useful, but must now split mean across individual varieties

#next- seperating out average ver->bud day lengths for each variety, and the same for bud-flow and flow-veraison

#group_by? variety, then summarize(by_variety, mean diffbudver na.rm=TRUE)
library("dplyr")
group_by(vassal,variety)->by_variety
summarize(by_variety, mean(DifferenceBudVer, na.rm=TRUE))->DifBudVerMeans

#################################2nd project part 

#next- finding average date of budding, flow, and veraison to determine "earliness" and compare earliness in varieties across all varieties  
vassal$bud_day<-as.numeric(vassal$bud_day)
class(vassal$bud_day)
mean.POSIXct(vassal$bud_day, na.rm=TRUE) # supposedly, this gives the average vassal$bud_date
mean.POSIXct(vassal$ver_day, na.rm=TRUE)# supposedly, this gives the average vassal$ver_day
mean.POSIXct(vassal$flow_day, na.rm=TRUE)# supposedly, this gives the average vassal$flow_day

#All of the averages for all three pheno-phases are on the same day, within 10 minutes of eachother????

