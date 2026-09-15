# 3 Principal Component Analysis
# This script includes the following:
#     1. Group variables into immune system
#     2. Prep data for principal component analysis (PCA)
#     3. Run PCA

# load packages
library(glmmTMB)
library(lme4)
library(ggplot2) # plotting
library(ggpubr) # plotting
library(dplyr) 

# load health data
HEALTH <- read.csv("./Projects/RSF_Health/DATA/Health_new.csv") 
HEALTH_ref <- read.csv("./Projects/RSF_Health/DATA/Alves_ind_health.csv", fileEncoding = "latin1") 


# several no entry columns have ".", fix by replacing with NA
HEALTH[HEALTH == c(".")] <- NA
HEALTH[HEALTH == ""] <- NA
HEALTH_ref[HEALTH_ref == c(".")] <- NA
HEALTH_ref[HEALTH_ref == ""] <- NA
# still some spaces but these will be removed later when converting cells to proper classes

# filter to only include adults
HEALTH <- HEALTH[HEALTH$Age.range == "Adult",]
HEALTH_ref <- HEALTH_ref[HEALTH_ref$Age == "Adult",]

# pull out columns you want
DF <- HEALTH[,c("Name", "White.Blood.Cells", "Segmented.Neutrophils.Absolute...mm3.", "Eosinophils.Absolute...mm3.", 
                "Lymphocytes.Absolute...mm3.", "Monocytes.Absolute....mm3.",
                "Platelets...x10.mm3.", "Globulin..g.dL.")] 
DF_ref <- HEALTH_ref[,c("Animal", "WBC...mm.3."  , "Eosinophils...mm.3.", "Neutrophils...mm.3.", "Lymphocytes...mm.3.",
                        "Monocytes...mm.3.", "Platelets..x10.mm.3.", "Globulin..g.dL.")] 

# remove rows with NAs
DF <- na.omit(DF)
DF_ref <- na.omit(DF_ref)


# extract IDs into a vector before we remove
ID <- DF$Name #same as DF_per
ID_ref1 <- DF_ref$Animal


# remove ID column 
DF$Name <- NULL
DF_ref$Animal <- NULL


# ensure remaininNULL# ensure remaining columns are numeric
DF[, 1:7] <- sapply(DF[,1:7], as.numeric)
DF_ref[, 1:7] <- sapply(DF_ref[,1:7], as.numeric)


# run PCA
PCA_DF <- prcomp(DF, scale=FALSE)
PCA_DF_ref <- prcomp(DF_ref, scale=FALSE)


# pull results from PCA into a dataframe (should be one value per ID)
RES_DF <- data.frame(PCA_DF$x)
RES_DF_ref <- data.frame(PCA_DF_ref$x)


# add IDs onto dataframe
RES_DF$ID <- ID
RES_DF_ref$ID <- ID_ref1


# add information on whether the dataframe is a reference or not
RES_DF$Info <- "Data"
RES_DF_ref$Info <- "Reference"


#bind results together
RES <- bind_rows(RES_DF)


# pull eigenvectors to see what variable influences the most variation in the dataset
PCA_DF$rotation[,1] # white blood cells and segmented neutrophils account for the majority of variance
PCA_DF_ref$rotation[,1] # white blood cells and segmented neutrophils account for the majority of variance


# save results
save(RES, file = "./Projects/RSF_Health/RESULTS/PCA_absolute.rda")






