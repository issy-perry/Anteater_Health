# 1 Movement Modeling
# This script includes the following:
#     1. Fit continuous-time movement models (hereafter ctmm) 
#     2. Estimate utilization distributions, or UDs 

# load packages
library(ctmm) # working with tel data and fitting models
library(tictoc) # determine how long each model takes to run

# load data
load("./DATA/Wild_raised_tel_data/Data_telemetry.rda")


# generate lists 
FITS <- list()
AKDE <- list()

# for.loop for running models for each individual
for(i in 1:length(DATA_wild)){

  # extract individual
  DATA <- DATA_wild[[i]]
  
  # determine how long models take for each individual
  tic("individual")
  
  # create variograms based on individuals
  GUESS <- ctmm.guess(DATA, CTMM=ctmm(error = TRUE), interactive = FALSE) 
  
  # fit models to variograms
  FIT <- ctmm.select(DATA, GUESS, trace = TRUE, cores = -1)
  
  # add to list
  FITS[[i]] <- FIT
  
  # calculate wAKDE
  AKDE[[i]] <- akde(DATA, FIT, weights = TRUE) # weights = TRUE because we do have dispersing populations
  
  toc() # end of timing
}

#transfer names from tel data list to the movement model list
names(FITS) <- names(DATA_wild)
names(AKDE) <- names(DATA_wild)


#save outputs
save(FITS, file = "./RESULTS/Fits/Fits.rda") 
save(AKDE, file = "./RESULTS/AKDEs/UDs.rda")
