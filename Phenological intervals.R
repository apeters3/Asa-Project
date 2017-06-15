#Housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)
library(plyr)
vassal<-read.csv("vassal_pheno.csv",header=TRUE,sep=",")

#Date formatting
vassal$bud_day<-as.Date(vassal$bud_day, format="%d/%m/%Y")
vassal$flow_day<-as.Date(vassal$flow_day, format="%d/%m/%Y")
vassal$ver_day<-as.Date(vassal$ver_day, format="%d/%m/%Y")


#Difference between budding and veraison date
vassal$DiffBudVer <- vassal$ver_day - vassal$bud_day

DifferenceBudVerSummary<-ddply(vassal,c("vassal$variety"), summarise,
 mean = mean(vassal$DiffBudVer, na.rm = TRUE))

sd = sd(DifferenceBudVer)
sem = sd(DifferenceBudVer)/sqrt(length(DifferenceBudVer))

DifferenceBudVerSummary #All means across varieties are identical?


#Cleaning inconsistent date formats 
rm(list=ls())
packs.to.extract<-list('raster','ncdf4','maptools','sp','foreach','rgdal')
lapply(packs.to.extract,require, character.only=T)
vassal.master<-read.csv("vassal_pheno.csv" ,header=T,sep=",",as.is=T)
vassal.master<-vassal.master[,1:9]
vassal.master$BB<-vassal.master$bud_day
vassal.master$FLOW<-vassal.master$flow_day
vassal.master$VER<-vassal.master$ver_day
vassal.master$MAT<-vassal.master$mat_day

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
vassal.master$BB<-as.Date(vassal.master$BB,format="%d/%m/%Y")
vassal.master$FLOW<-as.Date(vassal.master$FLOW,format="%d/%m/%Y")
vassal.master$VER<-as.Date(vassal.master$VER,format="%d/%m/%Y")
vassal.master$MAT<-as.Date(vassal.master$MAT,format="%d/%m/%Y")

vassal.master$BB=gsub( ".", "/", vassal.master$BB,fixed=T)
vassal.master$FLOW=gsub( ".", "/", vassal.master$FLOW,fixed=T)
vassal.master$VER=gsub( ".", "/", vassal.master$VER,fixed=T)
vassal.master$MAT=gsub( ".", "/", vassal.master$MAT,fixed=T)
vassal.master$review<-rep(NA,nrow(vassal.master))
for( i in 1:nrow(vassal.master) ) { #i=1
  
  print(i)
  
  ## correct BB
  if( vassal.master$BB[i] != "" & !is.na(vassal.master$BB[i])){ 
    
    if(nchar(vassal.master$BB[i])<12 & nchar(vassal.master$BB[i])>4 
       & length(strsplit(gsub( "/", " ", vassal.master$BB[i])," ")[[1]])==3){
      
      if(nchar(strsplit(gsub( "/", " ", vassal.master$BB[i])," ")[[1]][3])<3){
        
        elem.i<-vassal.master$BB[i]
        b<-strsplit(gsub( "/", " ", elem.i )," ")
        if(b[[1]][3]<20){
          elem.i<-paste(b[[1]][1],b[[1]][2],paste(20,b[[1]][3],sep=""),sep="/")
        } else {
          elem.i<-paste(b[[1]][1],b[[1]][2],paste(19,b[[1]][3],sep=""),sep="/")
        }
        vassal.master$BB[i]<-elem.i  
      }
    } else {
      vassal.master$review[i]<-"Review this line"
    }
  }
  
  ## correct FLOW
  if(vassal.master$FLOW[i]!=""& !is.na(vassal.master$FLOW[i])){ 
    
    if(nchar(vassal.master$FLOW[i])<12  & nchar(vassal.master$FLOW[i])>4
       & length(strsplit(gsub( "/", " ", vassal.master$FLOW[i])," ")[[1]])==3){
      
      if(nchar(strsplit(gsub( "/", " ", vassal.master$FLOW[i])," ")[[1]][3])<3){
        
        elem.i.fl<-vassal.master$FLOW[i]
        b.fl<-strsplit(gsub( "/", " ", elem.i.fl )," ")
        if(b.fl[[1]][3]<20){
          elem.i.fl<-paste(b.fl[[1]][1],b.fl[[1]][2],paste(20,b.fl[[1]][3],sep=""),sep="/")
        } else {
          elem.i.fl<-paste(b.fl[[1]][1],b.fl[[1]][2],paste(19,b.fl[[1]][3],sep=""),sep="/")
        }
        vassal.master$FLOW[i]<-elem.i.fl  
      }
    } else {
      vassal.master$review[i]<-"Review this line"
    }
  }
  
  ## correct VER
  if(vassal.master$VER[i]!=""& !is.na(vassal.master$VER[i])){ 
    if(nchar(vassal.master$VER[i])<12 & nchar(vassal.master$VER[i])>4
       & length(strsplit(gsub( "/", " ", vassal.master$VER[i])," ")[[1]])==3){
      if(nchar(strsplit(gsub( "/", " ", vassal.master$VER[i])," ")[[1]][3])<3){
        
        elem.i.ver<-vassal.master$VER[i]
        b.ver<-strsplit(gsub( "/", " ", elem.i.ver )," ")
        if(b.ver[[1]][3]<20){
          elem.i.ver<-paste(b.ver[[1]][1],b.ver[[1]][2],paste(20,b.ver[[1]][3],sep=""),sep="/")
        } else {
          elem.i.ver<-paste(b.ver[[1]][1],b.ver[[1]][2],paste(19,b.ver[[1]][3],sep=""),sep="/")
        }
        vassal.master$VER[i]<-elem.i.ver  
      }
    } else {
      vassal.master$review[i]<-"Review this line"
    }
    
  }
  
  ## correct MAT
  if(vassal.master$MAT[i]!=""& !is.na(vassal.master$MAT[i])){ 
    if(nchar(vassal.master$MAT[i])<12 & nchar(vassal.master$MAT[i])>4
       & length(strsplit(gsub( "/", " ", vassal.master$MAT[i])," ")[[1]])==3){
      if(nchar(strsplit(gsub( "/", " ", vassal.master$MAT[i])," ")[[1]][3])<3){
        
        elem.i.ver<-vassal.master$MAT[i]
        b.ver<-strsplit(gsub( "/", " ", elem.i.ver )," ")
        if(b.ver[[1]][3]<20){
          elem.i.ver<-paste(b.ver[[1]][1],b.ver[[1]][2],paste(20,b.ver[[1]][3],sep=""),sep="/")
        } else {
          elem.i.ver<-paste(b.ver[[1]][1],b.ver[[1]][2],paste(19,b.ver[[1]][3],sep=""),sep="/")
        }
        vassal.master$MAT[i]<-elem.i.ver  
      }
    } else {
      vassal.master$review[i]<-"Review this line"
    }
  }
}






