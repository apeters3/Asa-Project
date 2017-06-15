##############################################################################################################
# Function to:
#' * Load, clean and merge Vassal data
#'
#'  by Ignacio Morales-Castilla, I?aki Garc?a de Cortazar-Atauri, Elizabeth M. Wolkovich et al.
#'  09 June 2017
##############################################################################################################

## remove objects
rm(list=ls())

## load packages
packs.to.extract<-list('raster','ncdf4','maptools','sp','foreach','rgdal')
lapply(packs.to.extract,require, character.only=T)


## load packages
packs.to.extract<-list('raster','ncdf4','maptools','sp','foreach')
lapply(packs.to.extract,require, character.only=T)
library(xlsx)


## load function to clean Vassal data
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

head(vassal)


## setwd
setwd("~/MEGA/Work_Harvard_postdoc/vitis/Vassal data/")


## load vassal data using previously saved csvs
vassal.master<-read.csv("vassal_pheno.csv" ,header=T,sep=",",as.is=T)
vassal.johan<-read.csv("Vassal Phenology - Johan.dup.csv" ,header=T,sep=",",as.is=T)
vassal.JS<-read.csv("Vassal Phenology_dup_JSdata.csv" ,header=T,sep=",",as.is=T)
vassal.dup<-read.csv("Vassal Phenology_dup.csv" ,header=T,sep=",",as.is=T)
dir()

## inspect data
dim(vassal.dup)

head(vassal.dup)
vassal.JS$variety

## remove unwanted columns and rows
vassal.master<-vassal.master[,1:9]
vassal.johan<-vassal.johan[1:44,1:9]
vassal.JS<-vassal.JS[1:978,1:9]
vassal.dup<-vassal.dup[,1:9]

## add lacking columns if any
vassal.JS$bud_day<-rep(NA,nrow(vassal.JS))



## generate new variables to store corrected dates
vassal.master$BB<-vassal.master$bud_day
vassal.master$FLOW<-vassal.master$flow_day
vassal.master$VER<-vassal.master$ver_day
vassal.master$MAT<-vassal.master$mat_day


vassal.johan$BB<-vassal.johan$bud_day
vassal.johan$FLOW<-vassal.johan$flow_day
vassal.johan$VER<-vassal.johan$ver_day
vassal.johan$MAT<-vassal.johan$mat_day

vassal.JS$BB<-vassal.JS$bud_day
vassal.JS$FLOW<-vassal.JS$flow_day
vassal.JS$VER<-vassal.JS$ver_day
vassal.JS$MAT<-vassal.JS$mat_day

vassal.dup$BB<-vassal.dup$bud_day
vassal.dup$FLOW<-vassal.dup$flow_day
vassal.dup$VER<-vassal.dup$ver_day
vassal.dup$MAT<-vassal.dup$mat_day



# loop to correct days observed in vassal data****************
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

head(vassal.master)
write.table()

## convert observed phenology dates into Date format
vassal.master$BB<-as.Date(vassal.master$BB,format="%d/%m/%Y")
vassal.master$FLOW<-as.Date(vassal.master$FLOW,format="%d/%m/%Y")
vassal.master$VER<-as.Date(vassal.master$VER,format="%d/%m/%Y")
vassal.master$MAT<-as.Date(vassal.master$MAT,format="%d/%m/%Y")


## Repeath for vassal Johan and other files using the function
vassal.johan$BB=gsub( ".", "/", vassal.johan$BB,fixed=T)
vassal.johan$FLOW=gsub( ".", "/", vassal.johan$FLOW,fixed=T)
vassal.johan$VER=gsub( ".", "/", vassal.johan$VER,fixed=T)
vassal.johan$MAT=gsub( ".", "/", vassal.johan$MAT,fixed=T)
vassal.johan$review<-rep(NA,nrow(vassal.johan))

vassal.johan.clean<-clean.vassal(vassal.johan) ## function not needed
vassal.johan.clean$BB<-as.Date(vassal.johan$BB,format="%d/%m/%Y")
vassal.johan.clean$FLOW<-as.Date(vassal.johan$FLOW,format="%d/%m/%Y")
vassal.johan.clean$VER<-as.Date(vassal.johan$VER,format="%d/%m/%Y")
vassal.johan.clean$MAT<-as.Date(vassal.johan$MAT,format="%d/%m/%Y")



vassal.JS$BB=gsub( ".", "/", vassal.JS$BB,fixed=T)## gives an error because there is not data for budbreak
vassal.JS$FLOW=gsub( ".", "/", vassal.JS$FLOW,fixed=T)
vassal.JS$VER=gsub( ".", "/", vassal.JS$VER,fixed=T)
vassal.JS$MAT=gsub( ".", "/", vassal.JS$MAT,fixed=T)
vassal.JS$review<-rep(NA,nrow(vassal.JS))

vassal.JS.clean<-clean.vassal(vassal.JS) ## gives an error because JS data has no BB
vassal.JS.clean$BB<-as.Date(vassal.JS$BB,format="%d/%m/%Y")
vassal.JS.clean$FLOW<-as.Date(vassal.JS$FLOW,format="%d/%m/%Y")
vassal.JS.clean$VER<-as.Date(vassal.JS$VER,format="%d/%m/%Y")
vassal.JS.clean$MAT<-as.Date(vassal.JS$MAT,format="%d/%m/%Y")



vassal.dup$BB=gsub( ".", "/", vassal.dup$BB,fixed=T)
vassal.dup$FLOW=gsub( ".", "/", vassal.dup$FLOW,fixed=T)
vassal.dup$VER=gsub( ".", "/", vassal.dup$VER,fixed=T)
vassal.dup$MAT=gsub( ".", "/", vassal.dup$MAT,fixed=T)
vassal.dup$review<-rep(NA,nrow(vassal.dup))

vassal.dup.clean<-clean.vassal(vassal.dup) ## function not needed
vassal.dup.clean$BB<-as.Date(vassal.dup$BB,format="%d/%m/%Y")
vassal.dup.clean$FLOW<-as.Date(vassal.dup$FLOW,format="%d/%m/%Y")
vassal.dup.clean$VER<-as.Date(vassal.dup$VER,format="%d/%m/%Y")
vassal.dup.clean$MAT<-as.Date(vassal.dup$MAT,format="%d/%m/%Y")


### Merge all files to keep one single master table with all entered data

## simple workaround
head(vassal.dup,2)
final.vassal<-rbind(vassal.master,vassal.johan,vassal.dup,vassal.JS)









