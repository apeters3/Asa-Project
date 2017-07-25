#Cleaning Vassal Data Set + Generating Avg. Phenological Intervals + Correlating Phenological Intervals 

#Housekeeping
setwd("/Users/wolkolab/Asa Project")
rm(list=ls())
options(stringsAsFactors = FALSE)
library(plyr)

########### Merging Data Sheets and Cleaning Vassal ###################

#Merge Vassal sheets 
vassal.master<-read.csv("vassal_pheno.csv" ,header=T,sep=",",as.is=T)[,1:9]
vassal.johan<-read.csv("Vassal Phenology-Johan.csv" ,header=T,sep=",",as.is=T)[,1:9]
vassal.Asa1<-read.csv("Vassal_phenology_Asa.xlsx - 2015 alternative data sheets.csv" ,header=T,sep=",",as.is=T)[,1:9]
vassal.Asa2<-read.csv("Vassal_phenology_Asa.xlsx - 2015.csv" ,header=T,sep=",",as.is=T)[,1:9]

final.vassal<-rbind(vassal.master,vassal.johan,vassal.Asa1,vassal.Asa2)
final.vassal<-final.vassal[!duplicated(final.vassal),]

## Load function to clean Vassal data
clean.vassal<-function(x){
  x<-as.data.frame(x)
  for( i in 1:nrow(x) ) { #i=1
    
    print(i)
    
    ## correct BB
    if( x$BB[i] != "" & !is.na(x$BB[i])){ 
      
      if(nchar(x$BB[i])<12 & nchar(x$BB[i])>4 
         & length(strsplit(gsub( "/", " ", x$BB[i])," ")[[1]])==3){
        
        if(nchar(strsplit(gsub( "/", " ", x$BB[i])," ")[[1]][3])<3){
          
          elem.i<-x$BB[i]
          b<-strsplit(gsub( "/", " ", elem.i )," ")
          if(b[[1]][3]<20){
            elem.i<-paste(b[[1]][1],b[[1]][2],paste(20,b[[1]][3],sep=""),sep="/")
          } else {
            elem.i<-paste(b[[1]][1],b[[1]][2],paste(19,b[[1]][3],sep=""),sep="/")
          }
          x$BB[i]<-elem.i  
        }
      } else {
        x$review[i]<-"Review this line"
      }
    }
    
    ## correct FLOW
    if(x$FLOW[i]!=""& !is.na(x$FLOW[i])){ 
      
      if(nchar(x$FLOW[i])<12  & nchar(x$FLOW[i])>4
         & length(strsplit(gsub( "/", " ", x$FLOW[i])," ")[[1]])==3){
        
        if(nchar(strsplit(gsub( "/", " ", x$FLOW[i])," ")[[1]][3])<3){
          
          elem.i.fl<-x$FLOW[i]
          b.fl<-strsplit(gsub( "/", " ", elem.i.fl )," ")
          if(b.fl[[1]][3]<20){
            elem.i.fl<-paste(b.fl[[1]][1],b.fl[[1]][2],paste(20,b.fl[[1]][3],sep=""),sep="/")
          } else {
            elem.i.fl<-paste(b.fl[[1]][1],b.fl[[1]][2],paste(19,b.fl[[1]][3],sep=""),sep="/")
          }
          x$FLOW[i]<-elem.i.fl  
        }
      } else {
        x$review[i]<-"Review this line"
      }
    }
    
    ## correct VER
    if(x$VER[i]!=""& !is.na(x$VER[i])){ 
      if(nchar(x$VER[i])<12 & nchar(x$VER[i])>4
         & length(strsplit(gsub( "/", " ", x$VER[i])," ")[[1]])==3){
        if(nchar(strsplit(gsub( "/", " ", x$VER[i])," ")[[1]][3])<3){
          
          elem.i.ver<-x$VER[i]
          b.ver<-strsplit(gsub( "/", " ", elem.i.ver )," ")
          if(b.ver[[1]][3]<20){
            elem.i.ver<-paste(b.ver[[1]][1],b.ver[[1]][2],paste(20,b.ver[[1]][3],sep=""),sep="/")
          } else {
            elem.i.ver<-paste(b.ver[[1]][1],b.ver[[1]][2],paste(19,b.ver[[1]][3],sep=""),sep="/")
          }
          x$VER[i]<-elem.i.ver  
        }
      } else {
        x$review[i]<-"Review this line"
      }
      
    }
    
    ## correct MAT
    if(x$MAT[i]!=""& !is.na(x$MAT[i])){ 
      if(nchar(x$MAT[i])<12 & nchar(x$MAT[i])>4
         & length(strsplit(gsub( "/", " ", x$MAT[i])," ")[[1]])==3){
        if(nchar(strsplit(gsub( "/", " ", x$MAT[i])," ")[[1]][3])<3){
          
          elem.i.ver<-x$MAT[i]
          b.ver<-strsplit(gsub( "/", " ", elem.i.ver )," ")
          if(b.ver[[1]][3]<20){
            elem.i.ver<-paste(b.ver[[1]][1],b.ver[[1]][2],paste(20,b.ver[[1]][3],sep=""),sep="/")
          } else {
            elem.i.ver<-paste(b.ver[[1]][1],b.ver[[1]][2],paste(19,b.ver[[1]][3],sep=""),sep="/")
          }
          x$MAT[i]<-elem.i.ver  
        }
      } else {
        x$review[i]<-"Review this line"
      }
    }
  } 
  return(x)
}


dim(final.vassal)

#final.vassal<-final.vassal[,1:9]
final.vassal$BB<-final.vassal$bud_day
final.vassal$FLOW<-final.vassal$flow_day
final.vassal$VER<-final.vassal$ver_day
final.vassal$MAT<-final.vassal$mat_day


final.vassal$BB=gsub( ".", "/", final.vassal$BB,fixed=T)
final.vassal$FLOW=gsub( ".", "/", final.vassal$FLOW,fixed=T)
final.vassal$VER=gsub( ".", "/", final.vassal$VER,fixed=T)
final.vassal$MAT=gsub( ".", "/", final.vassal$MAT,fixed=T)
final.vassal$review<-rep(NA,nrow(final.vassal))


#Apply Cleaning
vassal.clean<-clean.vassal(final.vassal)


#Write new vassal csv

#write.csv(vassal.clean,file="Vassal_cleaned2.csv")


head(vassal.clean)

vassal.clean$BB<-as.Date(vassal.clean$BB,format="%d/%m/%Y")
vassal.clean$FLOW<-as.Date(vassal.clean$FLOW,format="%d/%m/%Y")
vassal.clean$VER<-as.Date(vassal.clean$VER,format="%d/%m/%Y")
vassal.clean$MAT<-as.Date(vassal.clean$MAT,format="%d/%m/%Y")


######################## Some Phenological Interval Means and Standard Deviations #############

#Budbreak-Maturation Mean and Standard Dev.

vassal.clean$DiffBudMat<-difftime(vassal.clean$MAT,vassal.clean$BB,units="days") 

nvar<-length(unique(vassal.clean$variety))
variety.mean.bud.mat<-as.data.frame(array(NA,dim=c(nvar,3)))
colnames(variety.mean.bud.mat)<-c("variety","mean Bud-Mat", "sd Bud-Mat")

for(i in 1:nvar){
  print(i)
  var.i<-unique(vassal.clean$variety)[i]
  subset.i<-subset(vassal.clean,variety==var.i)
  variety.mean.bud.mat[i,1]<-var.i
  variety.mean.bud.mat[i,2]<-as.numeric(mean(subset.i$DiffBudMat,na.rm=T))
  variety.mean.bud.mat[i,3]<-as.numeric(sd(subset.i$DiffBudMat,na.rm=T))
  
  
}

variety.mean.bud.mat

#Budbreak-Flowering Mean and Standard Dev.

vassal.clean$DiffBudFlow<-difftime(vassal.clean$FLOW,vassal.clean$BB,units="days") 


nvar<-length(unique(vassal.clean$variety))
variety.mean.bud.flow<-as.data.frame(array(NA,dim=c(nvar,3)))
colnames(variety.mean.bud.flow)<-c("variety","mean Bud-Flow", "sd Bud-Flow")

for(i in 1:nvar){
  print(i)
  var.i<-unique(vassal.clean$variety)[i]
  subset.i<-subset(vassal.clean,variety==var.i)
  variety.mean.bud.flow[i,1]<-var.i
  variety.mean.bud.flow[i,2]<-as.numeric(mean(subset.i$DiffBudFlow,na.rm=T))
  variety.mean.bud.flow[i,3]<-as.numeric(sd(subset.i$DiffBudFlow,na.rm=T))
  
  
}

variety.mean.bud.flow


#Flowering-Veraison Mean and Standard Dev

vassal.clean$DiffFlowVer<-difftime(vassal.clean$VER,vassal.clean$FLOW,units="days") 


nvar<-length(unique(vassal.clean$variety))
variety.mean.flow.ver<-as.data.frame(array(NA,dim=c(nvar,3)))
colnames(variety.mean.flow.ver)<-c("variety","mean Flow-Ver", "sd Flow-Ver")

for(i in 1:nvar){
  print(i)
  var.i<-unique(vassal.clean$variety)[i]
  subset.i<-subset(vassal.clean,variety==var.i)
  variety.mean.flow.ver[i,1]<-var.i
  variety.mean.flow.ver[i,2]<-as.numeric(mean(subset.i$DiffFlowVer,na.rm=T))
  variety.mean.flow.ver[i,3]<-as.numeric(sd(subset.i$DiffFlowVer,na.rm=T))
  
  
}
variety.mean.flow.ver

#Veraison-Maturation Mean and Standard Dev

vassal.clean$DiffVerMat<-difftime(vassal.clean$MAT,vassal.clean$VER,units="days") 


nvar<-length(unique(vassal.clean$variety))
variety.mean.ver.mat<-as.data.frame(array(NA,dim=c(nvar,3)))
colnames(variety.mean.ver.mat)<-c("variety","mean Ver-Mat", "sd Ver-Mat")

for(i in 1:nvar){
  print(i)
  var.i<-unique(vassal.clean$variety)[i]
  subset.i<-subset(vassal.clean,variety==var.i)
  variety.mean.ver.mat[i,1]<-var.i
  variety.mean.ver.mat[i,2]<-as.numeric(mean(subset.i$DiffVerMat,na.rm=T))
  variety.mean.ver.mat[i,3]<-as.numeric(sd(subset.i$DiffVerMat,na.rm=T))
  
  
}

variety.mean.ver.mat

#Budbreak-Veraison Mean and Standard Dev

vassal.clean$DiffBudVer<-difftime(vassal.clean$VER,vassal.clean$BB,units="days") 


nvar<-length(unique(vassal.clean$variety))
variety.mean.bud.ver<-as.data.frame(array(NA,dim=c(nvar,3)))
colnames(variety.mean.bud.ver)<-c("variety","mean Bud-Ver", "sd Bud-Ver")

for(i in 1:nvar){
  print(i)
  var.i<-unique(vassal.clean$variety)[i]
  subset.i<-subset(vassal.clean,variety==var.i)
  variety.mean.bud.ver[i,1]<-var.i
  variety.mean.bud.ver[i,2]<-as.numeric(mean(subset.i$DiffBudVer,na.rm=T))
  variety.mean.bud.ver[i,3]<-as.numeric(sd(subset.i$DiffBudVer,na.rm=T))
  
  
}

variety.mean.bud.ver


######################### Plotting Some Avg. Phenological Intervals ###########################

#Budbreak-Maturation Plot

data.to.plot<-variety.mean.bud.mat[with(variety.mean.bud.mat,order(`mean Bud-Mat`)),]
plot(x=NULL,y=NULL,xlim=c(10,300),ylim=c(1,130),ylab="Variety",xlab="Budbreak-Maturity in days", main="Budbreak to Maturity Interval" )
points(data.to.plot$`mean Bud-Mat`,1:201,pch=19)

for(i in 1:201){
  print(i)
  if(!is.na(data.to.plot[i,"mean Bud-Mat"])){
    low.conf<-data.to.plot[i,"mean Bud-Mat"]-data.to.plot[i,"sd Bud-Mat"]
    up.conf<-data.to.plot[i,"mean Bud-Mat"]+data.to.plot[i,"sd Bud-Mat"]
    if(!is.na(abs(up.conf-low.conf)) & abs(up.conf-low.conf)<400){
      lines(c(low.conf,up.conf),rep(i,2),type="l",col="darkgrey")
    }
  }
}

#Budbreak-Flowering Plot

data.to.plot<-variety.mean.bud.flow[with(variety.mean.bud.flow,order(`mean Bud-Flow`)),]
data.to.plot<-data.to.plot%>%filter(variety!="Marsanne")%>%filter(variety!="CarignanNoir")%>%filter(variety!="Listan")
plot(x=NULL,y=NULL,xlim=c(10,150),ylim=c(1,60),ylab="Variety",xlab="Budreak-Flowering in days",main="Budburst to Flowering Interval",cex=1.5,cex.axis=1.5, cex.lab=1.5,font.lab=2)
points(data.to.plot$`mean Bud-Flow`,1:198,pch=19)

for(i in 1:198){
  print(i)
  if(!is.na(data.to.plot[i,"mean Bud-Flow"])){
    low.conf<-data.to.plot[i,"mean Bud-Flow"]-data.to.plot[i,"sd Bud-Flow"]
    up.conf<-data.to.plot[i,"mean Bud-Flow"]+data.to.plot[i,"sd Bud-Flow"]
    if(!is.na(abs(up.conf-low.conf)) & abs(up.conf-low.conf)<400){
      lines(c(low.conf,up.conf),rep(i,2),type="l",col="darkgrey")
    }
  }
}
text(39,1,"Airen",cex=.75)
text(46,2,"Aledo",cex=.75)
text(47.82105,3,"Cabernet Sauvignon",cex=.85,font=2) ###late
text(50,4,"Sauvignon rose",cex=.75)
text(52,5,"Cab. Franc",cex=.75)
text(52,6,"Morrestal",cex=.75)
text(51.4,7,"Ugni blanc",cex=.75)
text(52.6,8,"Ries.blanc", cex=.75)
text(52,9,"Sauv.blanc",cex=.75)
text(54,10,"Durif",cex=.75)
text(54,11,"Barbera",cex=.75)
text(55.5,12,"Dimiat",cex=.75)     
text(56,13,"Souzao",cex=.75)
text(56,14,"Pinot Noir",cex=.75) 
text(56,15,"Syrah",cex=.75)
text(57,16,"Tinto Fino",cex=.75)
text(57,17,"Merlot",cex=.75)
text(57,18,"Grenache Noir",cex=.75)
text(54,19,"Verdelho de Madere",cex=.75)    
text(56,20,"Chardonnay",cex=.85,font=2)  #early
text(56,21,"Primitivo",cex=.75,font=2)
text(56,22,"Korai Olasz",cex=.75)
text(57,23,"Gold",cex=.75)
text(58,24,"Muscat Ottonel",cex=.75)
text(58.5,25,"Chasselas",cex=.75)
text(58.75,26,"Pinot gris",cex=.75)
text(59,27,"Aleatico",cex=.75)
text(59,28,"Aligote",cex=.75)
text(59,29,"Danuge",cex=.75)
text(59.1,30,"Petit Verdot",cex=.75)
text(59.25,31,"Meunier",cex=.75)
text(57,32,"Alicante H.Bouchet",cex=.75)
text(57,33,"Muscat rouge fonce",cex=.75)
text(61,34,"Touriga",cex=.75)
text(59.25,35,"Ruby Seedless",cex=.75)
text(60,36,"Pinot blanc",cex=.75)
text(61,37,"Sangiovese",cex=.75)
text(62.3,38,"Periquita",cex=.75)
text(65.3,39,"Naose",cex=.75)
text(64,40,"Teroldego",cex=.75)
text(65,41,"Sarfeher",cex=.75)
text(66,42,"Hunisa",cex=.75)
text(65,43,"Sultanine blanc",cex=.75)
text(67,44,"Aramon Noir",cex=.75)
text(69.75,45,"Vidiano",cex=.75)
text(68,46,"Sauv. rose aromatique",cex=.75)
text(75.8,47,"Troussaeu Gris", cex=.75)
text(83,48,"July Muscat",cex=.75)


#Flowering to Veraison Plot
data.to.plot<-variety.mean.flow.ver[with(variety.mean.flow.ver,order(`mean Flow-Ver`)),]
plot(x=NULL,y=NULL,xlim=c(10,200),ylim=c(1,80),ylab="Variety",xlab="Flowering-Veraison in days", main="Flowering to Veraison Interval" )
points(data.to.plot$`mean Flow-Ver`,1:198,pch=19)

for(i in 1:198){
  print(i)
  if(!is.na(data.to.plot[i,"mean Flow-Ver"])){
    low.conf<-data.to.plot[i,"mean Flow-Ver"]-data.to.plot[i,"sd Flow-Ver"]
    up.conf<-data.to.plot[i,"mean Flow-Ver"]+data.to.plot[i,"sd Flow-Ver"]
    if(!is.na(abs(up.conf-low.conf)) & abs(up.conf-low.conf)<400){
      lines(c(low.conf,up.conf),rep(i,2),type="l",col="darkgrey")
    }
  }
}



#Veraison to Maturation Plot
data.to.plot<-variety.mean.ver.mat[with(variety.mean.ver.mat,order(`mean Ver-Mat`)),]
data.to.plot<-data.to.plot%>%filter(variety!="Viognier")
plot(x=NULL,y=NULL,xlim=c(10,80),ylim=c(1,80),ylab="Variety",xlab="Veraison-Maturity in days", main="Veraison to Maturity Interval",cex=1.5,cex.axis=1.5, cex.lab=1.5,font.lab=2)
points(data.to.plot$`mean Ver-Mat`,1:200,pch=19)

for(i in 1:200){
  print(i)
  if(!is.na(data.to.plot[i,"mean Ver-Mat"])){
    low.conf<-data.to.plot[i,"mean Ver-Mat"]-data.to.plot[i,"sd Ver-Mat"]
    up.conf<-data.to.plot[i,"mean Ver-Mat"]+data.to.plot[i,"sd Ver-Mat"]
    if(!is.na(abs(up.conf-low.conf)) & abs(up.conf-low.conf)<400){
      lines(c(low.conf,up.conf),rep(i,2),type="l",col="darkgrey")
    }
  }
}
text(29.93258,46,"Cabernet Sauvignon",cex=.85,font=2)
text(24.07246,22,"Chardonnay",cex=.85,font=2)

#Budbreak to veraison
data.to.plot<-variety.mean.bud.ver[with(variety.mean.bud.ver,order(`mean Bud-Ver`)),]
plot(x=NULL,y=NULL,xlim=c(10,200),ylim=c(1,80),ylab="Variety",xlab="Budbreak-Veraison in days", main="Budbreak to Veraison Interval" )
points(data.to.plot$`mean Bud-Ver`,1:201,pch=19)

for(i in 1:201){
  print(i)
  if(!is.na(data.to.plot[i,"mean Bud-Ver"])){
    low.conf<-data.to.plot[i,"mean Bud-Ver"]-data.to.plot[i,"sd Bud-Ver"]
    up.conf<-data.to.plot[i,"mean Bud-Ver"]+data.to.plot[i,"sd Bud-Ver"]
    if(!is.na(abs(up.conf-low.conf)) & abs(up.conf-low.conf)<400){
      lines(c(low.conf,up.conf),rep(i,2),type="l",col="darkgrey")
    }
  }
}


###################### Plotting correlation between different pheno-intervals #############


#plot multi-panel plots
par(mfrow=c(1,2))

## plotting correlation between BB-FL and FL-VER
combined.datasets1<-cbind(variety.mean.bud.flow,variety.mean.flow.ver[,2:3])
combined.datasets1<-na.omit(combined.datasets1)
plot(combined.datasets1$`mean Bud-Flow`,combined.datasets1$`mean Flow-Ver`,
     xlim=c(0,250),ylim=c(0,250),pch=19,cex=1.5,cex.axis=1.5, cex.lab=1.5,cex.main=1.5,
     xlab='Budbreak to Flowering',ylab='Flowering to Veraison', 
     main='Budbreak to Flowering Intervals vs. Flowering to Veraison Intervals')
myline<-lm(combined.datasets1$`mean Flow-Ver`~combined.datasets1$`mean Bud-Flow`)    
abline(myline,col="grey",lwd=2)

text(combined.datasets1[which.min(combined.datasets1$`mean Flow-Ver`),c(2,4)],pos=3,
     combined.datasets1[which.min(combined.datasets1$`mean Flow-Ver`),1])
text(combined.datasets1[which.max(combined.datasets1$`mean Flow-Ver`),c(2,4)],pos=3,
     combined.datasets1[which.max(combined.datasets1$`mean Flow-Ver`),1])

text(combined.datasets1[which.min(combined.datasets1$`mean Bud-Flow`),c(2,4)],pos=2,
     combined.datasets1[which.min(combined.datasets1$`mean Bud-Flow`),1])
text(combined.datasets1[which.max(combined.datasets1$`mean Bud-Flow`),c(2,4)],pos=3,
     combined.datasets1[which.max(combined.datasets1$`mean Bud-Flow`),1])

cor.BBFLFLVER<-cor(combined.datasets1$`mean Bud-Flow`,combined.datasets1$`mean Flow-Ver`,use='pairwise.complete.obs')
text(50,150,cor.BBFLFLVER)



# Plot Correlation between BB-FL vs. FL-VER  without Marsanne, Carignan Noir, Listan
combined.datasets1<-cbind(variety.mean.bud.flow,variety.mean.flow.ver[,2:3])
nomarsanne<-combined.datasets1%>%filter(variety!="Marsanne")%>%filter(variety!="CarignanNoir")%>%filter(variety!="Listan")


nomarsanne<-na.omit(nomarsanne)
plot(nomarsanne$`mean Bud-Flow`,nomarsanne$`mean Flow-Ver`,
     xlim=c(0,150),ylim=c(0,150),pch=19,cex=1.5,cex.axis=1.5, cex.lab=1.5,cex.main=1.5,font.lab=2,
     xlab='Budbreak to Flowering',ylab='Flowering to Veraison', 
     main='Budbreak to Flowering Intervals vs. Flowering to Veraison Intervals')
myline<-lm(nomarsanne$`mean Flow-Ver`~nomarsanne$`mean Bud-Flow`)    
abline(myline,col="grey",lwd=2)

text(nomarsanne[which.min(nomarsanne$`mean Flow-Ver`),c(2,4)],pos=2,
     nomarsanne[which.min(nomarsanne$`mean Flow-Ver`),1])
text(nomarsanne[which.max(nomarsanne$`mean Flow-Ver`),c(2,4)],pos=2,
     nomarsanne[which.max(nomarsanne$`mean Flow-Ver`),1])

text(nomarsanne[which.min(nomarsanne$`mean Bud-Flow`),c(2,4)],pos=2,
     nomarsanne[which.min(nomarsanne$`mean Bud-Flow`),1])
text(nomarsanne[which.max(nomarsanne$`mean Bud-Flow`),c(2,4)],pos=4,
     nomarsanne[which.max(nomarsanne$`mean Bud-Flow`),1])

cor.BBFLFLVER<-cor(nomarsanne$`mean Bud-Flow`,nomarsanne$`mean Flow-Ver`,use='pairwise.complete.obs')
cor.BBFLFLVER=
text(50,100,"r=0.1714")

## plot correlation between FL-VER and VER-MAT ----main 
combined.datasets2<-cbind(variety.mean.flow.ver,variety.mean.ver.mat[,2:3])
combined.datasets2<-na.omit(combined.datasets2)
plot(combined.datasets2$`mean Flow-Ver`,combined.datasets2$`mean Ver-Mat`,
     xlim=c(0,250),ylim=c(0,250),pch=19,cex=1.5,cex.axis=1.5, cex.lab=1.5,cex.main=1.5,font.lab=2,
     xlab='Flowering to Veraison',ylab='Veraison to Maturity', 
     main='Flowering to Veraison vs. Veraison to Maturity Intervals')
myline<-lm(combined.datasets2$`mean Ver-Mat`~combined.datasets2$`mean Flow-Ver`)    
abline(myline,col="grey",lwd=2)

text(combined.datasets2[which.min(combined.datasets2$`mean Flow-Ver`),c(2,4)],pos=4,
     combined.datasets2[which.min(combined.datasets2$`mean Flow-Ver`),1])
text(combined.datasets2[which.max(combined.datasets2$`mean Flow-Ver`),c(2,4)],pos=4,
     combined.datasets2[which.max(combined.datasets2$`mean Flow-Ver`),1])

text(combined.datasets2[which.min(combined.datasets2$`mean Ver-Mat`),c(2,4)],pos=2,
     combined.datasets2[which.min(combined.datasets2$`mean Ver-Mat`),1])
text(combined.datasets2[which.max(combined.datasets2$`mean Ver-Mat`),c(2,4)],pos=3,
     combined.datasets2[which.max(combined.datasets2$`mean Ver-Mat`),1])

cor.FLVERVERMAT<-cor(combined.datasets2$`mean Flow-Ver`,combined.datasets2$`mean Ver-Mat`,use='pairwise.complete.obs')
text(50,100,"r=0.1155")

## plot correlation between BUD-VER, VER-MAT
combined.datasets3<-cbind(variety.mean.bud.ver,variety.mean.ver.mat[,2:3])
combined.datasets3<-na.omit(combined.datasets3)
plot(combined.datasets3$`mean Bud-Ver`,combined.datasets3$`mean Ver-Mat`,
     xlim=c(0,250),ylim=c(0,250),pch=19,cex=1.5,cex.axis=1.5, cex.lab=1.5,
     xlab='Budbreak to Veraison',ylab='Veraison to Maturity', 
     main='Budbreak to Veraison vs. Veraison to Maturity Intervals')
myline<-lm(combined.datasets3$`mean Ver-Mat`~combined.datasets3$`mean Bud-Ver`)    
abline(myline,col="grey",lwd=2)

text(combined.datasets3[which.min(combined.datasets3$`mean Bud-Ver`),c(2,4)],pos=2,
     combined.datasets3[which.min(combined.datasets3$`mean Bud-Ver`),1])
text(combined.datasets3[which.max(combined.datasets3$`mean Bud-Ver`),c(2,4)],pos=3,
     combined.datasets3[which.max(combined.datasets3$`mean Bud-Ver`),1])

text(combined.datasets3[which.min(combined.datasets3$`mean Ver-Mat`),c(2,4)],pos=4,
     combined.datasets3[which.min(combined.datasets3$`mean Ver-Mat`),1])
text(combined.datasets3[which.max(combined.datasets3$`mean Ver-Mat`),c(2,4)],pos=2,
     combined.datasets3[which.max(combined.datasets3$`mean Ver-Mat`),1])

cor.BBVERVERMAT<-cor(combined.datasets3$`mean Bud-Ver`,combined.datasets3$`mean Ver-Mat`,use='pairwise.complete.obs')
text(50,150,cor.BBVERVERMAT)

## plot correlation between BUD-MAT, FLO-VER
combined.datasets4<-cbind(variety.mean.bud.mat,variety.mean.flow.ver[,2:3])
combined.datasets4<-na.omit(combined.datasets4)
plot(combined.datasets4$`mean Bud-Mat`,combined.datasets4$`mean Flow-Ver`,
     xlim=c(0,250),ylim=c(0,250),pch=19,cex=1.5,cex.axis=1.5, cex.lab=1.5,
     xlab='Budbreak to Maturity',ylab='Flowering to Veraison', 
     main='Budbreak to Maturity vs. Flowering to Veraison Intervals')
myline<-lm(combined.datasets4$`mean Flow-Ver`~combined.datasets4$`mean Bud-Mat`)    
abline(myline,col="grey",lwd=2)

text(combined.datasets4[which.min(combined.datasets4$`mean Bud-Mat`),c(2,4)],pos=2,
     combined.datasets4[which.min(combined.datasets4$`mean Bud-Mat`),1])
text(combined.datasets4[which.max(combined.datasets4$`mean Bud-Mat`),c(2,4)],pos=3,
     combined.datasets4[which.max(combined.datasets4$`mean Bud-Mat`),1])

text(combined.datasets4[which.min(combined.datasets4$`mean Flow-Ver`),c(2,4)],pos=3,
     combined.datasets4[which.min(combined.datasets4$`mean Flow-Ver`),1])
text(combined.datasets4[which.max(combined.datasets4$`mean Flow-Ver`),c(2,4)],pos=3,
     combined.datasets4[which.max(combined.datasets4$`mean Flow-Ver`),1])

cor.BBMATFLOVER<-cor(combined.datasets4$`mean Bud-Mat`,combined.datasets4$`mean Flow-Ver`,use='pairwise.complete.obs')
text(50,150,cor.BBMATFLOVER)

## plot correlation between BUD-FLO,VER-MAT
combined.datasets5<-cbind(variety.mean.bud.flow,variety.mean.ver.mat[,2:3])
combined.datasets5<-na.omit(combined.datasets5)
plot(combined.datasets5$`mean Bud-Flow`,combined.datasets5$`mean Ver-Mat`,
     xlim=c(0,250),ylim=c(0,250),pch=19,cex=1.5,cex.axis=1.5, cex.lab=1.5,
     xlab='Budbreak to Flowering',ylab='Veraison to Maturity', 
     main='Budbreak to Flowering vs. Veraison to Maturity')
myline<-lm(combined.datasets5$`mean Ver-Mat`~combined.datasets5$`mean Bud-Flow`)    
abline(myline,col="grey",lwd=2)

text(combined.datasets5[which.min(combined.datasets5$`mean Bud-Flow`),c(2,4)],pos=4,
     combined.datasets5[which.min(combined.datasets5$`mean Bud-Flow`),1])
text(combined.datasets5[which.max(combined.datasets5$`mean Bud-Flow`),c(2,4)],pos=3,
     combined.datasets5[which.max(combined.datasets5$`mean Bud-Flow`),1])

text(combined.datasets5[which.min(combined.datasets5$`mean Ver-Mat`),c(2,4)],pos=3,
     combined.datasets5[which.min(combined.datasets5$`mean Ver-Mat`),1])
text(combined.datasets5[which.max(combined.datasets5$`mean Ver-Mat`),c(2,4)],pos=4,
     combined.datasets5[which.max(combined.datasets5$`mean Ver-Mat`),1])

cor.BBFLOVERMAT<-cor(combined.datasets5$`mean Bud-Flow`,combined.datasets5$`mean Ver-Mat`,use='pairwise.complete.obs')
text(50,150,cor.BBFLOVERMAT)



