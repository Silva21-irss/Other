# Create the change maps featured in the climate app
rm(list=ls())
library(terra)
library(NLP)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggmap)

gcms <- c('GISS-E2-1-G','ACCESS-ESM1-5','MRI-ESM2-0','MPI-ESM1-2-HR','EC-Earth3','GFDL-ESM4','MIROC6','CNRM-ESM2-1','UKESM1-0-LL','IPSL-CM6A-LR','BCC-CSM2-MR','INM-CM5-0','CanESM5','13GCMs_ensemble')
timeFrames <- c('2001-2020','2021-2040','2041-2060','2061-2080','2081-2100')
ssp <- c('ssp126','ssp245','ssp370','ssp585')
seasonsName <- c('ANNUAL','WINTER','SPRING','SUMMER','AUTUMN')

# Check to make sure these files exist
for(x in data$Tav){
  and <- x %in% filenames
  if(and == FALSE){break}
}

# Function to plot color bar
color.bar <- function(lut, min, max, nticks=8, ticks=seq(min, max, len=nticks), title='') {
  scale = (length(lut)-1)/(max-min)
  plot(c(2,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
  axis(2, round(ticks,2), las=1)
  for (i in 1:(length(lut)-1)) {
    y = (i-1)/scale + min
    rect(2,y,5,y+1/scale, col=lut[i], border=NA)
  }
}

c_ramp <- colorRampPalette(list('#51a9f8','#fd4437'))(50)

hubSites <- read.csv('kkzrank.includeukesm.csv',header=T)
hubSites <- names(hubSites)

for(hub in hubSites){
  setwd(paste('Y:/ByUser/Amy W/DEM250m/CSV_Points/Futures/New',hub,sep='/'))
  #setwd('Y:/ByUser/Amy W/DEM250m/CSV_Points/Futures/New/Petawawa')
  filenames <- list.files(path=getwd(),pattern = "*.tif$") # Only match when the end is .tif
  
  # Compile into a general data frame
  data <- data.frame(gcms = gcms[rep(seq_len(length(gcms)),each = 100)],
                     timeF = timeFrames[rep(seq_len(length(timeFrames)),each = 20)],
                     ssp = ssp[rep(seq_len(length(ssp)),each = 5)],
                     season = seasonsName[rep(seq_len(length(seasonsName)),each = 1)])
  
  # Combine scenarios and variables for filenames
  data$Tmin <- paste(data$gcms,data$ssp,data$timeF,data$season,'Tmi.tif',sep='_')
  data$Tavg <- paste(data$gcms,data$ssp,data$timeF,data$season,'Tav.tif',sep='_')
  data$Tmax <- paste(data$gcms,data$ssp,data$timeF,data$season,'Tma.tif',sep='_')
  data$PPT <- paste(data$gcms,data$ssp,data$timeF,data$season,'PPT.tif',sep='_')
  data$Historical <- paste('Y:/ByUser/Amy W/DEM250m/CSV_Points/Historical/',hub,'_',
                           data$season,'_',sep='')
  
  # Maximum Temperature
  a <- 1
  while (a<100) {
    # Access min and max of all 13 rasters for normalization
    nMin <- NULL
    nMax <- NULL
    for(x in seq(a,1400,100)){
      r <- rast(data$Tmax[x]) + 273.15
      Hr <- rast(paste0(data$Historical[x],'Tma.tif')) + 273.15 # Don't add 273.15 for PPT
      r@ptr$extent <- Hr@ptr$extent
      Pr <- ((r - Hr)/Hr) * 100
      nMin <- rbind(nMin,Pr@ptr$range_min)
      nMax <- rbind(nMax,Pr@ptr$range_max)
    }
    nRange <- c(min(nMin),max(nMax))

    # Make PNG image
    newPNG <- paste('Y:/ByUser/Mike_B/CMIP6 App/maps/changeMap',hub,data$timeF[a],data$ssp[a],data$season[a],'png',sep='.')
    png(file=newPNG,width=1084,height=594)
    par(mfrow=c(4,4),mar=c(1,4,1,4),oma=c(2,2,2,2))
    plot(c(2,18), c(0,1), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main='')
    mtext(paste0('Change in ',data$season[a],'\n Maximum Temperature (%)'),side=1,line=-9,font=2)
    mtext(paste0('Hub : ',hub,'\n SSP : ',data$ssp[a],'\n Years : ',data$timeF[a]),side=1,line=-3,font=0.5)

    color.bar(c_ramp,nRange[1],nRange[2])
    mtext(paste0('% Change'),side=2,line=3.2,font=1,cex=1.2)
    # Add plots to the figure through loop
    for(x in seq(a,1400,100)){
      r <- rast(data$Tmax[x]) + 273.15
      Hr <- rast(paste0(data$Historical[x],'Tma.tif')) + 273.15 # Don't add 273.15 for PPT
      Pr <- ((r - Hr)/Hr) * 100
      plot(Pr, main = paste0(data$gcms[x]),
           axes=FALSE,cex.main = 1.5,col = c_ramp,range= nRange,legend=FALSE)
      #plot(Pr, main = paste0(data$gcms[x]),axes=FALSE,cex.main = 2,col = c_ramp)
    }
    dev.off()
    a <- a+1
  }
  # Minimum Temperature
  a <- 1
  while (a<100) {
    # Access min and max of all 13 rasters for normalization
    nMin <- NULL
    nMax <- NULL
    for(x in seq(a,1400,100)){
      r <- rast(data$Tmin[x]) + 273.15
      Hr <- rast(paste0(data$Historical[x],'Tmi.tif')) + 273.15 # Don't add 273.15 for PPT
      Pr <- ((r - Hr)/Hr) * 100
      nMin <- rbind(nMin,Pr@ptr$range_min)
      nMax <- rbind(nMax,Pr@ptr$range_max)
    }
    nRange <- c(min(nMin),max(nMax))
    
    # Make PNG image
    newPNG <- paste('Y:/ByUser/Mike_B/CMIP6 App/maps/changeMinMap',hub,data$timeF[a],data$ssp[a],data$season[a],'png',sep='.')
    png(file=newPNG,width=1084,height=594)
    par(mfrow=c(4,4),mar=c(1,4,1,4),oma=c(2,2,2,2))
    plot(c(2,18), c(0,1), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main='')
    mtext(paste0('Change in ',data$season[a],'\n Minimum Temperature (%)'),side=1,line=-9,font=2)
    mtext(paste0('Hub : ',hub,'\n SSP : ',data$ssp[a],'\n Years : ',data$timeF[a]),side=1,line=-3,font=0.5)
    
    color.bar(c_ramp,nRange[1],nRange[2])
    mtext(paste0('% Change'),side=2,line=3.4,font=1,cex=1.2)
    # Add plots to the figure through loop
    for(x in seq(a,1400,100)){
      r <- rast(data$Tmin[x]) + 273.15
      Hr <- rast(paste0(data$Historical[x],'Tmi.tif')) + 273.15 # Don't add 273.15 for PPT
      Pr <- ((r - Hr)/Hr) * 100
      plot(Pr, main = paste0(data$gcms[x]),
           axes=FALSE,cex.main = 1.5,col = c_ramp,range= nRange,legend=FALSE)
      #plot(Pr, main = paste0(data$gcms[x]),axes=FALSE,cex.main = 2,col = c_ramp)
    }
    dev.off()
    a <- a+1
  }
  # Average Temperature
  a <- 1
  while (a<100) {
    # Access min and max of all 13 rasters for normalization
    nMin <- NULL
    nMax <- NULL
    for(x in seq(a,1400,100)){
      r <- rast(data$Tavg[x]) + 273.15
      Hr <- rast(paste0(data$Historical[x],'Tav.tif')) + 273.15 # Don't add 273.15 for PPT
      Pr <- ((r - Hr)/Hr) * 100
      nMin <- rbind(nMin,Pr@ptr$range_min)
      nMax <- rbind(nMax,Pr@ptr$range_max)
    }
    nRange <- c(min(nMin),max(nMax))
    
    # Make PNG image
    newPNG <- paste('Y:/ByUser/Mike_B/CMIP6 App/maps/changeAvgMap',hub,data$timeF[a],data$ssp[a],data$season[a],'png',sep='.')
    png(file=newPNG,width=1084,height=594)
    par(mfrow=c(4,4),mar=c(1,4,1,4),oma=c(2,2,2,2))
    plot(c(2,18), c(0,1), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main='')
    mtext(paste0('Change in ',data$season[a],'\n Average Temperature (%)'),side=1,line=-9,font=2)
    mtext(paste0('Hub : ',hub,'\n SSP : ',data$ssp[a],'\n Years : ',data$timeF[a]),side=1,line=-3,font=0.5)
    
    color.bar(c_ramp,nRange[1],nRange[2])
    mtext(paste0('% Change'),side=2,line=3.4,font=1,cex=1.2)
    # Add plots to the figure through loop
    for(x in seq(a,1400,100)){
      r <- rast(data$Tavg[x]) + 273.15
      Hr <- rast(paste0(data$Historical[x],'Tav.tif')) + 273.15 # Don't add 273.15 for PPT
      Pr <- ((r - Hr)/Hr) * 100
      plot(Pr, main = paste0(data$gcms[x]),
           axes=FALSE,cex.main = 1.5,col = c_ramp,range= nRange,legend=FALSE)
      #plot(Pr, main = paste0(data$gcms[x]),axes=FALSE,cex.main = 2,col = c_ramp)
    }
    dev.off()
    a <- a+1
  }
  # Precipitation
  a <- 1
  while (a<100) {
    # Access min and max of all 13 rasters for normalization
    nMin <- NULL
    nMax <- NULL
    for(x in seq(a,1400,100)){
      r <- rast(data$PPT[x])
      Hr <- rast(paste0(data$Historical[x],'PPT.tif')) # Don't add 273.15 for PPT
      Pr <- ((r - Hr)/Hr) * 100
      nMin <- rbind(nMin,Pr@ptr$range_min)
      nMax <- rbind(nMax,Pr@ptr$range_max)
    }
    nRange <- c(min(nMin),max(nMax))
    
    # Make PNG image
    newPNG <- paste('Y:/ByUser/Mike_B/CMIP6 App/maps/changePPTMap',hub,data$timeF[a],data$ssp[a],data$season[a],'png',sep='.')
    png(file=newPNG,width=1084,height=594)
    par(mfrow=c(4,4),mar=c(1,4,1,4),oma=c(2,2,2,2))
    plot(c(2,18), c(0,1), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main='')
    mtext(paste0('Change in ',data$season[a],'\n  Precipitation (%)'),side=1,line=-9,font=2)
    mtext(paste0('Hub : ',hub,'\n SSP : ',data$ssp[a],'\n Years : ',data$timeF[a]),side=1,line=-3,font=0.5)
    
    color.bar(c_ramp,nRange[1],nRange[2])
    mtext(paste0('% Change'),side=2,line=3.6,font=1,cex=1.2)
    # Add plots to the figure through loop
    for(x in seq(a,1400,100)){
      r <- rast(data$PPT[x]) 
      Hr <- rast(paste0(data$Historical[x],'PPT.tif'))  # Don't add 273.15 for PPT
      Pr <- ((r - Hr)/Hr) * 100
      plot(Pr, main = paste0(data$gcms[x]),
           axes=FALSE,cex.main = 1.5,col = c_ramp,range= nRange,legend=FALSE)
      #plot(Pr, main = paste0(data$gcms[x]),axes=FALSE,cex.main = 2,col = c_ramp)
    }
    dev.off()
    a <- a+1
  }
}