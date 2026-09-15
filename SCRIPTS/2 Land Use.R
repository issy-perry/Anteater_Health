# 2 Calculating Land Use
# This script was adapted, with permission, from Dr. Michael Noonan's work (found at: https://github.com/QuantitativeEcologyLab/microplastics_brazil/blob/main/scripts/01_3_tapir_movement_modelling.R)
#     1. Load script for downloading land coverage data
#     2. Calculate the weighted percentage of coverage for each land type within an individual's HR

# load data and UDs
load("./RESULTS/AKDEs/UDs_wild.rda")
load("./DATA/Wild_raised_tel_data/Data_telemetry.rda")


# land use ----
#make a list
RES <- list()
for(i in 1:length(DATA_17)){
  # Generate a brief message to keep track of progress
  cat("Working on individual ", i, " of ", length(DATA_17), "\n")
  
  # load data
  DATA <- DATA_17[[i]]
  
  # load UD
  AKDES <- AKDE_17[[i]]
  
  HR <- rast(raster(AKDES, DF = "PMF"))
  HR2 <- project(HR, crs(land_types), res = res(land_types))
  HR.df2 <- terra::as.data.frame(HR2, xy = TRUE, na.rm = TRUE)
  #Renormalize
  HR.df2$layer <- HR.df2$layer/sum(HR.df2$layer)
  HR <- project(HR, crs(land_types), res = res(land_types))
  HR.df <- terra::as.data.frame(HR, xy = TRUE, na.rm = TRUE)
  #Renormalize
  HR.df$layer <- HR.df$layer/sum(HR.df$layer)
  
  #Extract habitat values
  #for some reason, just adding an "other" category and removing the other covariates makes things messy, so for now, we will include all covariates and then remove more later
  HR.df$land_class <- extract(land_types, HR.df[,1:2])[,2]
  HR.df$land_class[HR.df$land_class %in% c("1","3", "4","5","6","49","29")] <- "Native_forest"
  HR.df$land_class[HR.df$land_class %in% c("9")] <- "Plantation"
  HR.df$land_class[HR.df$land_class %in% c("12","15")] <- "Pasture"
  HR.df$land_class[HR.df$land_class %in% c("18","19","20","21","39","40","41","62","36","46","47","35","48")] <- "Agriculture"
  HR.df$land_class[HR.df$land_class %in% c("24","25","30","75")] <- "Development"
  HR.df$land_class[HR.df$land_class %in% c("11","26","33")] <- "Water"
  #HR.df$land_class[HR.df$land_class %in% c("1","3", "4","5","6","49","29","11","26","33","12","15","18","19","20","21","39","40","41","62","36","46","47","35","48","24","25","30")] <- "Other"
  
  # Use the home range PDF to calculate the weighted proportions of time spent the different land class types
  PROPS <- round(wpct(HR.df$land_class, HR.df$layer)*100,2)
  PROPS2 <- data.frame(class = names(PROPS),
                       proportion = as.numeric(PROPS))
  PROPS <- data.frame(t(PROPS2))[2,]
  names(PROPS) <- PROPS2$class
  
  
  res <- data.frame(binomial = "Myrmecophaga_tridactyla")
  res$ID <- AKDES@info$identity
  
  res <- cbind(res,PROPS)
  RES[[i]] <- res
  
} # closes loop
# bind list together
res <- do.call(dplyr::bind_rows, RES)
# replace NAs with 0s
res[is.na(res)] <- 0

# save the land use data as a csv
write.table(res,
            file = "~/Downloads/Land_Use/RESULTS/Use_2017.csv",
            row.names=FALSE,
            col.names=TRUE,
            sep=",")







#2018 use ----
#make a list
RES <- list()
for(i in 1:length(DATA_18)){
  # Generate a brief message to keep track of progress
  cat("Working on individual ", i, " of ", length(DATA_18), "\n")
  
  # load data
  DATA <- DATA_18[[i]]
  
  # load UD
  AKDES <- AKDE_18[[i]]
  
  HR <- rast(raster(AKDES, DF = "PMF"))
  HR2 <- project(HR, crs(land_types), res = res(land_types))
  HR.df2 <- terra::as.data.frame(HR2, xy = TRUE, na.rm = TRUE)
  #Renormalize
  HR.df2$layer <- HR.df2$layer/sum(HR.df2$layer)
  HR <- project(HR, crs(land_types), res = res(land_types))
  HR.df <- terra::as.data.frame(HR, xy = TRUE, na.rm = TRUE)
  #Renormalize
  HR.df$layer <- HR.df$layer/sum(HR.df$layer)
  
  #Extract habitat values
  #for some reason, just adding an "other" category and removing the other covariates makes things messy, so for now, we will include all covariates and then remove more later
  HR.df$land_class <- extract(land_types, HR.df[,1:2])[,2]
  HR.df$land_class[HR.df$land_class %in% c("1","3", "4","5","6","49","29")] <- "Native_forest"
  HR.df$land_class[HR.df$land_class %in% c("9")] <- "Plantation"
  HR.df$land_class[HR.df$land_class %in% c("12","15")] <- "Pasture"
  HR.df$land_class[HR.df$land_class %in% c("18","19","20","21","39","40","41","62","36","46","47","35","48")] <- "Agriculture"
  HR.df$land_class[HR.df$land_class %in% c("24","25","30","75")] <- "Development"
  HR.df$land_class[HR.df$land_class %in% c("11","26","33")] <- "Water"
  #HR.df$land_class[HR.df$land_class %in% c("1","3", "4","5","6","49","29","11","26","33","12","15","18","19","20","21","39","40","41","62","36","46","47","35","48","24","25","30")] <- "Other"
  
  # Use the home range PDF to calculate the weighted proportions of time spent the different land class types
  PROPS <- round(wpct(HR.df$land_class, HR.df$layer)*100,2)
  PROPS2 <- data.frame(class = names(PROPS),
                       proportion = as.numeric(PROPS))
  PROPS <- data.frame(t(PROPS2))[2,]
  names(PROPS) <- PROPS2$class
  
  res <- data.frame(binomial = "Myrmecophaga_tridactyla")
  res$ID <- AKDES@info$identity
  
  res <- cbind(res,PROPS)
  RES[[i]] <- res
  
} # Closes the loop that runs over the telemetry object (i.e., i)

# bind list together
res <- do.call(dplyr::bind_rows, RES)
# replace NAs with 0s
res[is.na(res)] <- 0

# save the land use data as a csv
write.table(res,
            file = "~/Downloads/Land_Use/RESULTS/Use_2018.csv",
            row.names=FALSE,
            col.names=TRUE,
            sep=",")







# HFI in HRs ----
RES <- list()
for(i in 1:length(DATA_17)){
  # Generate a brief message to keep track of progress
  cat("Working on individual ", i, " of ", length(DATA_17), "\n")
  # extract IND's data
  DATA <- DATA_17[[i]]
  
  # extract UD
  AKDES <- AKDE_17[[i]]
  
  HR <- rast(raster(AKDES, DF = "PMF"))
  HR2 <- project(HR, crs(HFI), res = res(HFI))
  HR.df2 <- terra::as.data.frame(HR2, xy = TRUE, na.rm = TRUE)
  
  #Renormalize
  HR.df2$layer <- HR.df2$layer/sum(HR.df2$layer)
  HR <- project(HR, crs(HFI), res = res(HFI))
  HR.df <- terra::as.data.frame(HR, xy = TRUE, na.rm = TRUE)
  
  #Renormalize
  HR.df$layer <- HR.df$layer/sum(HR.df$layer)
  #Extract habitat values
  HR.df2$HFI <- extract(HFI, HR.df2[,1:2])[,2]/1000 
  
  # Use the home range PDF to calculate the weighted proportions of time spent the different land class types
  #PROPS <- round(wpct(HR.df$land_class, HR.df$layer)*100,2)
  #PROPS2 <- data.frame(class = names(PROPS),
  #                     proportion = as.numeric(PROPS))
  #PROPS <- data.frame(t(PROPS2))[2,]
  #names(PROPS) <- PROPS2$class
  res <- data.frame(binomial = "Myrmecophaga_tridactyla")
  res$ID <- AKDES@info$identity
  
  # HFI
  res$min_HFI <- min(HR.df2$HFI)
  res$mean_HFI <- sum(HR.df2$layer*HR.df2$HFI)
  res$max_HFI <- max(HR.df2$HFI)
  
  #res <- cbind(res,PROPS)
  RES[[i]] <- res
  
} # closes the loop
res <- do.call(dplyr::bind_rows, RES)
res[is.na(res)] <- 0
# Save the land use data as a csv
write.table(res,
            file = "./RESULTS/HFI_2017.csv",
            row.names=FALSE,
            col.names=TRUE,
            sep=",")



RES <- list()
for(i in 1:length(DATA_18)){
  # Generate a brief message to keep track of progress
  cat("Working on individual ", i, " of ", length(DATA_18), "\n")
  #Import the HR estimate for the ith animal
  DATA <- DATA_18[[i]]
  
  AKDES <- AKDE_18[[i]]
  
  HR <- rast(raster(AKDES, DF = "PMF"))
  HR2 <- project(HR, crs(HFI), res = res(HFI))
  HR.df2 <- terra::as.data.frame(HR2, xy = TRUE, na.rm = TRUE)
  #Renormalize
  HR.df2$layer <- HR.df2$layer/sum(HR.df2$layer)
  HR <- project(HR, crs(HFI), res = res(HFI))
  HR.df <- terra::as.data.frame(HR, xy = TRUE, na.rm = TRUE)
  #Renormalize
  HR.df$layer <- HR.df$layer/sum(HR.df$layer)
  #Extract habitat values
  HR.df2$HFI <- extract(HFI, HR.df2[,1:2])[,2]/1000 
  
  # Use the home range PDF to calculate the weighted proportions of time spent the different land class types
  #PROPS <- round(wpct(HR.df$land_class, HR.df$layer)*100,2)
  #PROPS2 <- data.frame(class = names(PROPS),
  #                     proportion = as.numeric(PROPS))
  #PROPS <- data.frame(t(PROPS2))[2,]
  #names(PROPS) <- PROPS2$class
  res <- data.frame(binomial = "Myrmecophaga_tridactyla")
  res$ID <- AKDES@info$identity
  # HFI
  res$min_HFI <- min(HR.df2$HFI)
  res$mean_HFI <- sum(HR.df2$layer*HR.df2$HFI)
  res$max_HFI <- max(HR.df2$HFI)
  #res <- cbind(res,PROPS)
  RES[[i]] <- res
} # Closes the loop that runs over the telemetry object (i.e., i)
res <- do.call(dplyr::bind_rows, RES)
res[is.na(res)] <- 0
# Save the land use data as a csv
write.table(res,
            file = "./RESULTS/HFI_2018.csv",
            row.names=FALSE,
            col.names=TRUE,
            sep=",")





