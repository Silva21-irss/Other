
# ==========================================
# step 7: use KKZ to structure the ensemble subsets
# this is just an excerpt. it won't run by itself. you'll have to adapt it to your data. 
# ==========================================
# colin.mahony@gov.bc.ca

library(MASS) # Do we need this????

setwd('Y:/ByUser/Mike_B/CMIP6 App') # set directory

temp <- list.files(pattern = 'change.*') # Access all files that begin with "change"

source("Our_KKZ.R")

a <- 1
while(a < length(temp)+1){
  temp[a] <- gsub('change.','',temp[a])
  temp[a] <- gsub('.csv','',temp[a])
  a <- a+1
}
ecoprovs <- temp

data <- read.csv(paste("change", ecoprovs[1], "csv", sep="."))
rownames(data) <- NULL

gcms <- unique(data$gcm)[-c(1,2)] # Also remove the gcm-emsemble
gcms.excluded <- gcms[c(9,2, 8, 3)]
gcms.screened <- gcms[-which(gcms%in%gcms.excluded)]
scenarios <- unique(data$scenario)[-c(1,5)] # Also remove ssp5-85
scenario.names <- c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0")

# Other definitions
monthdays <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
seasonmonth.mat <- matrix(monthcodes[c(12, 1:11)],4, byrow=T)

proj.years <- seq(2001, 2100, 20)
proj.year.names <- c("2021-2040", "2041-2060", "2061-2080", "2081-2100")

seasons <- c("wt", "sp", "sm", "at")
season.names <- c("Winter", "Spring", "Summer", "Autumn")

yeartimes <- c(seasons, monthcodes)
yeartime.names <- c(season.names, month.name)

# ## visualization for testing purposes
# scenario <- scenarios[2]
# proj.year <- proj.years[3]
# #select data for a single time slice and scenario
# x <- data[which(data$scenario==scenario & data$proj.year==proj.year), grep(paste(seasons, collapse = "|"), names(data))]
# x <- as.matrix(x) # necessary for the subset.kkz function to work
# rownames(x) <- NULL
# x <- scale(x) #z-standardize the data
# attr(x,"scaled:center")<-NULL
# attr(x,"scaled:scale")<-NULL
# x.pca <- predict(prcomp(x), x)
# par(mar=c(3,3,0.1,0.1))
# eqscplot(x.pca[,1:2], col="white")
# text(x.pca[,1:2], as.character(1:13), cex=0.7)

## create an ordered list of ensemble subsets for each ecoprovince, using the mean of z-scores for change in each time period and scenario. 
for(include.UKESM in c(T,F)){
  gcms.select <- if(include.UKESM==T) gcms.screened else gcms.screened[-grep("UKESM", gcms.screened)]
  kkz.rank <- matrix(rep(NA, length(gcms.select)*length(ecoprovs)), nrow = length(gcms.select))
  for(ecoprov in ecoprovs){
    # concatenate change z-scores for all time periods and scenarios
    scenario=scenarios[1]
    for(scenario in scenarios[-length(scenarios)]){
      for(proj.year in proj.years[-1]){ #removed the past period because it will most be affected by noise
        data <- read.csv(paste("change", ecoprov, "csv", sep="."))
        rownames(data) <- NULL
        
        x <- data[which(data$gcm%in%gcms.select & data$scenario==scenario & data$proj.year==proj.year), grep(paste(seasons, collapse = "|"), names(data))]
        # x <- as.matrix(x) # necessary for the subset.kkz function to work
        rownames(x) <- gcms.select
        #rownames(x) <- gcms.screened
        x <- scale(x) #z-standardize the data
        attr(x,"scaled:center")<-NULL
        attr(x,"scaled:scale")<-NULL
        
        xc <- if(scenario == scenarios[1] & proj.year==proj.years[-1][1]) x else rbind(xc, x)
      }
    }
    xmean <- aggregate(xc, by=list(rownames(xc)), FUN=mean)[,-1] #mean of the z-scores for each model
    xmean <- as.matrix(xmean) # necessary for the subset.kkz function to work
    
    x.kkz <- subset.kkz(xmean,n.cases=13) # this is the KKZ algorithm sourced from the KKZ.R script
    kkz.rank[, which(ecoprovs==ecoprov)] <-  gcms.select[as.numeric(row.names(x.kkz$cases))]
    
    print(ecoprov)
  }
  kkz.rank <- as.data.frame(kkz.rank)
  names(kkz.rank) <- ecoprovs
  
  #add in screened models
  kkz.add <- as.data.frame(matrix(rep(if(include.UKESM==T) gcms.excluded else c(gcms.screened[grep("UKESM", gcms.screened)], gcms.excluded), times=length(ecoprovs)), ncol = length(ecoprovs), byrow=F))
  names(kkz.add) <- ecoprovs
  kkz.rank <- rbind(kkz.rank, kkz.add)
  
  filename <- paste("kkzRank", if(include.UKESM==T) "includeUKESM" else "excludeUKESM", sep=".")
  write.csv(kkz.rank, paste(filename,"1.csv", sep=""), row.names=F)
  
}

