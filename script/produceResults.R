#Read data and set some parameters----------
library(IBTSindices)
year =2018
quarter = 1
path = "Papers/manuscript/results/"
species = "Gadus morhua";
dat = readIBTSData(survey = "NS-IBTS", year = year, quarter = quarter,species = species)
n=2 #Number bootstrap samples
#-------------------------------------


#Calculates CPUEs on age-level in the North Sea---------
bootstrapProcedure = "datras"
ALKprocedure = "datras"
set.seed(1455)
mCPUEDatras = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = dat,
                                 bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure)

bootstrapProcedure = "stratifiedHLandCA"
ALKprocedure = "datras"
set.seed(1455)
mCPUEStratifiedHL = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = dat,
                           bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure)

bootstrapProcedure = "stratifiedHLandCA"
ALKprocedure = "haulBased"
set.seed(1455)
mCPUEHaulBasedStratifiedHLandCA = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = dat,
                           bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure)
#-------------------------------------



#Resample data with different length groups-----------------
library(doParallel)
nCores = 3
samplesWithinEachIntervall = 5
ALKprocedure = "haulBased"
yearSpan = 2015:2018
quarters = c(1,3)
sampleOtolithSpan = 1:5
for(quarter in quarters){
  for(year in yearSpan){
    set.seed(1455)
    cl <- makeCluster(nCores)
    registerDoParallel(cl)
    dat = readIBTSData(survey = "NS-IBTS", year = year, quarter = quarter,species = species)
    foreach(dl = sampleOtolithSpan) %dopar%{
    library(IBTSindices)
      lengthDivision = c(seq(0,max(round(dat$ca_hh$LngtCm),round(dat$hl_hh$LngtCm)) + 2*dl,by = dl))
      removeOtoliths = investigateRemoval(species = species, year = year, quarter = quarter,dat = dat,B = n,
                                          lengthDivision = lengthDivision,
                                          samplesWithinEachIntervall = samplesWithinEachIntervall)
      #Save work
      saveRDS(removeOtoliths, file = paste(path,"resamplingOtoliths/RemovalCodDl",dl,"year",year,"Q",quarter,"n", n,ALKprocedure,samplesWithinEachIntervall, sep= ""))
    }
  }
}



#Resample data with different number of otoliths sampled within each length group------
library(doParallel)
nCores = 3
dl = 5
ALKprocedure = "haulBased"
yearSpan = 2015:2018
quarters = c(1,3)
sampleOtolithSpan = c(2:5,999) #Note that 1 otolith was sampled in the previous experiement
for(quarter in quarters){
  for(year in yearSpan){
    cl <- makeCluster(nCores)
    registerDoParallel(cl)
    dat = readIBTSData(survey = "NS-IBTS", year = year, quarter = quarter,species = species)
    lengthDivision = c(seq(0,max(round(dat$ca_hh$LngtCm),round(dat$hl_hh$LngtCm)) + 2*dl,by = dl))
    foreach(samplesWithinEachIntervall = sampleOtolithSpan)%dopar% {
      library(IBTSindices)
      set.seed(1455)
      removeOtoliths = investigateRemoval(species = species, year = year, quarter = quarter,dat = dat,B = n,
                                          lengthDivision = lengthDivision,
                                          samplesWithinEachIntervall = samplesWithinEachIntervall)
      #Save work
      saveRDS(removeOtoliths, file = paste(path,"resamplingOtoliths/RemovalCodDl",dl,"year",year,"Q",quarter,"n", n,ALKprocedure,samplesWithinEachIntervall, sep= ""))
    }
  }
}


#Resample different number of hauls and ages.
library(doParallel)
nCores = 3
cl <- makeCluster(nCores)
registerDoParallel(cl)
bootstrapProcedure = "datrasHLstratifiedCA"
ALKprocedure = "datras"
dl=5
yearSpan = 2015:2018
quarters = c(1,3)
Nspan = c(50,75,100,200,300,400,500)
sampleOtolithSpan = c(1,5)
for(quarter in quarters){
  foreach(year = yearSpan) %dopar% {
    library(IBTSindices)
    dat = readIBTSData(survey = "NS-IBTS", year = year, quarter = quarter,species = species)
    for(N in Nspan) {
      for(samplesWithinEachIntervall in sampleOtolithSpan){
        set.seed(1455)
        lengthDivision = c(seq(0,max(round(dat$ca_hh$LngtCm),round(dat$hl_hh$LngtCm)) + 2*dl,by = dl))
        mCPUEStratifiedHL = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = dat,
                                         bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure,
                                         onlySimulate = TRUE,nSimHauls = N,samplesWithinEachIntervall = samplesWithinEachIntervall)
        saveRDS(mCPUEStratifiedHL, file = paste(path,"resamplingNandotoliths/resampleCodDl",dl,"N",N,"year",year,"Q",quarter,"n", n,ALKprocedure,samplesWithinEachIntervall, sep= ""))
      }
    }
  }
}
