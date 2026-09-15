# 4 Health Modeling
# This script includes the following:
#     1. Fit generalized additive models to the principal components of the PCA
#     2. Fit generalized additive models to the individual variables included in the PCA

# load packages
library(glmmTMB)
library(lme4)
library(dplyr) 

# load results and data
load("./Projects/RSF_Health/RESULTS/PCA_absolute.rda")
USE <- read.csv("./Projects/RSF_Health/RESULTS/Land_Use.csv")
HEALTH <- read.csv("./Projects/RSF_Health/DATA/Health_new.csv")
HEALTH_ref <- read.csv("./Projects/RSF_Health/DATA/Alves_ind_health.csv", fileEncoding = "latin1") 


# replace empty rows of health dataframe with NAs
HEALTH[HEALTH == c(".")] <- NA
HEALTH[HEALTH == ""] <- NA
HEALTH_ref[HEALTH_ref == c(".")] <- NA
HEALTH_ref[HEALTH_ref == ""] <- NA

# remove empty rows
HEALTH <- HEALTH [!is.na(HEALTH$Name),]

# ensure all individuals are adults
HEALTH <- HEALTH[HEALTH$Age.range == "Adult",]
HEALTH_ref <- HEALTH_ref[HEALTH_ref$Age == "Adult",]

# remove columns with no variance / only NAs
#HEALTH[c("Icteric.Index", "Nucleated.RBCs.or.Metarubricytes", "Band.Neutrophils.Absolute...mm3.",
#         "Band.Neutrophils.Relative....", "VLDL...mg.dL.", "Uric.Acid...mg.dL.", "Chloride..mEq.L.",
#         "Hemolysis", "Lipemia", "Icterus", "Progesterone.ng.mL", "Free.T4", "Estradiol", "F.S.H.", 
#         " Calcium...mg.dL.")] <- NULL 

# remove all columns apart from ID, date, sex, and immune system variables
HEALTH <- HEALTH[,c("Animal", "Name", "Date", "Age.range", "Sex", "General.condition", "Weight..kg.", 
                    "White.Blood.Cells", "Segmented.Neutrophils.Absolute...mm3.", "Segmented.Neutrophils.Relative....", 
                    "Band.Neutrophils.Absolute...mm3.", "Band.Neutrophils.Relative....", "Eosinophils.Absolute...mm3.", 
                    "Eosinophils.Relative....", "Basophils.Absolute...mm3.", "Basophils.Relative....", 
                    "Lymphocytes.Absolute...mm3.", "Lymphocytes.Relative....", "Monocytes.Absolute....mm3.", 
                    "Monocytes.Relative.....", "Platelets...x10.mm3.", "Globulin..g.dL.", "Albumin.Globulin.Ratio")]

# extract coluumn names
col_names <- colnames(HEALTH)[c(8:22)] # column names for response variables

# merge dataframes together by values in the "Name" and "ID" columns
TOTAL <- merge(HEALTH, USE, by.x = "Name", by.y = "ID")

# remove extra duplicate values
TOTAL[,c("binomial", "ID.1", "binomial.1", "ID.2", "binomial.2")] <- NULL



# PCA GAM
# merge PCA results with land use
RES_df <- merge(RES, USE, by = "ID") # excludes reference population

# extract PCA results for reference population
REF <- RES[RES$Info == "Reference",]

# calculate median of principal components for plotting
REF_MED <- median(REF$PC1)

# ensure number columns are numeric and ID column is a factor
RES_df[,c(2,11:16,19:21)] <- sapply(RES_df[,c(2,11:16,19:21)], as.numeric)
RES_df$ID <- as.factor(RES_df$ID)

# plot relationships between land use and principal components before plotting so that there are not many covariates in the model
plot(x = RES_df$Agriculture, RES_df$PC1, pch = 19, col = "red") # nothing
plot(x = RES_df$Pasture, RES_df$PC1, pch = 19, col = "red") # model
plot(x = RES_df$Plantation, RES_df$PC1, pch = 19, col = "red")  # nothing
plot(x = RES_df$Native_forest, RES_df$PC1, pch = 19, col = "red")  # nothing
plot(x = RES_df$Water, RES_df$PC1, pch = 19, col = "red")  # nothing
plot(x = RES_df$min_HFI, RES_df$PC1, pch = 19, col = "red") # nothing
plot(x = RES_df$mean_HFI, RES_df$PC1, pch = 19, col = "red")  # nothing
plot(x = RES_df$max_HFI, RES_df$PC1, pch = 19, col = "red")  # nothing

# run GAM
M_PCA <- gam(PC1 ~ Pasture + 
               s(ID, bs = "re"), # random effect on ID
             data = RES_df,
             family = gaussian(link = "identity"), # gaussian distribution since PCs are negative
             method = "REML")
# view summary
summary(M_PCA)

# create a dataframe to feed into predictions
New_df <- data.frame(Pasture = seq(from = 0, to = 100, by = 0.5),
                     ID = "null")
# make predictions
PREDS <- predict(M_PCA,
                 newdata = New_df,
                 type = "link",
                 exclude= "s(ID)", 
                 se.fit = TRUE)
# make a new dataframe to include model predictions
New_df <- data.frame(x = seq(from = 0, to = 100, by = 0.5),
                     predicted = PREDS$fit,
                     CI_low = PREDS$fit - 1.96*PREDS$se.fit,
                     CI_high = PREDS$fit + 1.96*PREDS$se.fit)

# save results for plotting
save(New_df, file = "./Projects/RSF_Health/RESULTS/PCA_GAM_preds.rda")





# individual vairables ----
# ensure columns are numeric
HEALTH_ref[,c(7:24)] <- sapply(HEALTH_ref[,c(7:24)], as.numeric)
# create dataframe of reference medians/ means for plotting
REF <- data.frame(White.Blood.Cells = median(HEALTH_ref$WBC...mm.3., na.rm = TRUE),
                  Segmented.Neutrophils.Relative.... = median(HEALTH_ref$Neutrophils....., na.rm = TRUE),
                  Globulin..g.dL. = mean(HEALTH_ref$Globulin..g.dL., na.rm = TRUE),
                  Platelets...x10.mm3. = median(HEALTH_ref$Platelets..x10.3.mm.3., na.rm = TRUE),
                  Monocytes.Relative..... = median(HEALTH_ref$Monocytes...., na.rm = TRUE))

# save results for plotting
save(REF, file = "./Projects/RSF_Health/RESULTS/Reference_medians.rda")

# change name
DF <- TOTAL
# ensure number columns are numeric and ID column is factor
DF[,c(8:51)] <- sapply(DF[,c(8:51)], as.numeric)
DF$ID <- as.factor(DF$Name)

# create a list to hold results
GAM_RES <- list()

# white blood cell count ----
# plot interactions to decide what to include in the model
plot(x = DF$Agriculture, DF$White.Blood.Cells, pch = 19, col = "red") # maybe
plot(x = DF$Pasture, DF$White.Blood.Cells, pch = 19, col = "red") # yes
plot(x = DF$Plantation, DF$White.Blood.Cells, pch = 19, col = "red") 
plot(x = DF$Native_forest, DF$White.Blood.Cells, pch = 19, col = "red") 
plot(x = DF$Water, DF$White.Blood.Cells, pch = 19, col = "red") 
plot(x = DF$min_HFI, DF$White.Blood.Cells, pch = 19, col = "red") 
plot(x = DF$mean_HFI, DF$White.Blood.Cells, pch = 19, col = "red") 
plot(x = DF$max_HFI, DF$White.Blood.Cells, pch = 19, col = "red") 

M_WBC <- gam(White.Blood.Cells ~ Agriculture + 
               Pasture + 
               s(ID, bs = "re"), # random effect on ID
             data = DF,
             family = tw(link = "log"), # tweedie distribution
             method = "REML")
summary(M_WBC) # significant effects


# need to make new separate models because it's not working by excluding effects
M_WBC1 <- gam(White.Blood.Cells ~ #Agriculture + 
                Pasture + 
                #min_HFI + 
                #mean_HFI +
                s(ID, bs = "re"), # random effect on ID
              data = DF,
              family = tw(link = "log"), # tweedie distribution
              method = "REML")
summary(M_WBC1)


# create a dataframe to feed into predictions
New_df <- data.frame(Pasture = seq(from = 1, to = 100, by = 2),
                     ID = "null")
New_df$Pasture <- as.numeric(New_df$Pasture)
New_df$ID <- as.factor(New_df$ID)

# make predictions
Predict_M <- predict(M_WBC1,
                     newdata = New_df,
                     type = "response",
                     exclude = c("s(ID)"), 
                     se.fit = TRUE)

# make a new dataframe to include model predictions
New_df <- data.frame(x = seq(from = 1, to = 100, by = 2),
                     predicted = Predict_M$fit,
                     CI_low = Predict_M$fit - 1.96*Predict_M$se.fit,
                     CI_high = Predict_M$fit + 1.96*Predict_M$se.fit)

# add to list
GAM_RES[[1]] <- New_df

# need to make new separate models because it's not working by excluding effects
M_WBC2 <- gam(White.Blood.Cells ~ Agriculture + 
                #Pasture + 
                #min_HFI + 
                #mean_HFI +
                s(ID, bs = "re"), # random effect on ID
              data = DF,
              family = tw(link = "log"), # tweedie distribution
              method = "REML")
summary(M_WBC2) 


# create a dataframe to feed into predictions
New_df <- data.frame(Agriculture = seq(from = 0, to = 100, by = 0.25),
                     ID = "null")
New_df$Agriculture <- as.numeric(New_df$Agriculture)
New_df$ID <- as.factor(New_df$ID)

# make predictions
Predict_M <- predict(M_WBC2,
                     newdata = New_df,
                     type = "response",
                     exclude = c("s(ID)"), 
                     se.fit = TRUE)

# make a new dataframe to include model predictions
New_df <- data.frame(x = seq(from = 0, to = 100, by = 0.25),
                     predicted = Predict_M$fit,
                     CI_low = Predict_M$fit - 1.96*Predict_M$se.fit,
                     CI_high = Predict_M$fit + 1.96*Predict_M$se.fit)

# add to list
GAM_RES[[2]] <- New_df



# relative percentage of segmented neutrophils -----
plot(x = DF$Agriculture, DF$Segmented.Neutrophils.Relative...., pch = 19, col = "red") 
plot(x = DF$Pasture, DF$Segmented.Neutrophils.Relative...., pch = 19, col = "red") 
plot(x = DF$Plantation, DF$Segmented.Neutrophils.Relative...., pch = 19, col = "red") 
plot(x = DF$Native_forest, DF$Segmented.Neutrophils.Relative...., pch = 19, col = "red")
plot(x = DF$Water, DF$Segmented.Neutrophils.Relative...., pch = 19, col = "red") 
plot(x = DF$min_HFI, DF$Segmented.Neutrophils.Relative...., pch = 19, col = "red") 
plot(x = DF$mean_HFI, DF$Segmented.Neutrophils.Relative...., pch = 19, col = "red") 
plot(x = DF$max_HFI, DF$Segmented.Neutrophils.Relative...., pch = 19, col = "red") 


M_NEU_rel1 <- gam(Segmented.Neutrophils.Relative.... ~Pasture + 
                    s(ID, bs = "re"), # random effect on ID
                  data = DF,
                  family = tw(link = "log"), # tweedie distribution
                  method = "REML")

# create a dataframe to feed into predictions
New_df <- data.frame(Pasture = seq(from = 0, to = 100, by = 0.25),
                     ID = "null")
New_df$Pasture <- as.numeric(New_df$Pasture)
New_df$ID <- as.factor(New_df$ID)

# make predictions
Predict_M <- predict(M_NEU_rel1,
                     newdata = New_df,
                     type = "response",
                     exclude = c("s(ID)"), 
                     se.fit = TRUE)

# make a new dataframe to include model predictions
New_df <- data.frame(x = seq(from = 0, to = 100, by = 0.25),
                     predicted = Predict_M$fit,
                     CI_low = Predict_M$fit - 1.96*Predict_M$se.fit,
                     CI_high = Predict_M$fit + 1.96*Predict_M$se.fit)

# add to list
GAM_RES[[3]] <- New_df




# relative percentage of basophils -----
plot(x = DF$Agriculture, DF$Basophils.Relative...., pch = 19, col = "red") 
plot(x = DF$Pasture, DF$Basophils.Relative...., pch = 19, col = "red") 
plot(x = DF$Plantation, DF$Basophils.Relative...., pch = 19, col = "red") 
plot(x = DF$Native_forest, DF$Basophils.Relative...., pch = 19, col = "red") 
plot(x = DF$Water, DF$Basophils.Relative...., pch = 19, col = "red") 
plot(x = DF$min_HFI, DF$Basophils.Relative...., pch = 19, col = "red") 
plot(x = DF$mean_HFI, DF$Basophils.Relative...., pch = 19, col = "red") 
plot(x = DF$max_HFI, DF$Basophils.Relative...., pch = 19, col = "red") 

M_BAS <- gam(Basophils.Relative.... ~ 
               mean_HFI +
               s(ID, bs = "re"), # random effect on ID
             data = DF,
             family = tw(link = "log"), # tweedie distribution
             method = "REML")
summary(M_BAS) # min HFI


# create a dataframe to feed into predictions
New_df <- data.frame(mean_HFI = seq(from = min(DF$mean_HFI), to = max(DF$mean_HFI), length.out = 100),
                     ID = "null")
New_df$max_Water_dist <- as.numeric(New_df$mean_HFI)
New_df$ID <- as.factor(New_df$ID)

# make predictions
Predict_M <- predict(M_BAS,
                     newdata = New_df,
                     type = "response",
                     exclude = c("s(ID)"), 
                     se.fit = TRUE)

# make a new dataframe to include model predictions
New_df <- data.frame(x = seq(from = min(DF$mean_HFI), to = max(DF$mean_HFI), length.out = 100),
                     predicted = Predict_M$fit,
                     CI_low = Predict_M$fit - 1.96*Predict_M$se.fit,
                     CI_high = Predict_M$fit + 1.96*Predict_M$se.fit)

# add to list
GAM_RES[[4]] <- New_df






# platelets ----
plot(x = DF$Agriculture, DF$Platelets...x10.mm3., pch = 19, col = "red") 
plot(x = DF$Pasture, DF$Platelets...x10.mm3., pch = 19, col = "red") 
plot(x = DF$Plantation, DF$Platelets...x10.mm3., pch = 19, col = "red") 
plot(x = DF$Native_forest, DF$Platelets...x10.mm3., pch = 19, col = "red") 
plot(x = DF$Water, DF$Platelets...x10.mm3., pch = 19, col = "red") 
plot(x = DF$min_HFI, DF$Platelets...x10.mm3., pch = 19, col = "red") 
plot(x = DF$mean_HFI, DF$Platelets...x10.mm3., pch = 19, col = "red") 
plot(x = DF$max_HFI, DF$Platelets...x10.mm3., pch = 19, col = "red") 

M_PLAT <- gam(Platelets...x10.mm3. ~ Agriculture + 
                s(ID, bs = "re"), # random effect on ID
              data = DF,
              family = tw(link = "log"), # tweedie distribution
              method = "REML")
summary(M_PLAT) # agriculture

# create a dataframe to feed into predictions
New_df <- data.frame(Agriculture = seq(from = 0, to = 100, by = 0.25),
                     ID = "null")
New_df$Agriculture <- as.numeric(New_df$Agriculture)
New_df$ID <- as.factor(New_df$ID)

# make predictions
Predict_M <- predict(M_PLAT,
                     newdata = New_df,
                     type = "response",
                     exclude = c("s(ID)"), 
                     se.fit = TRUE)

# make a new dataframe to include model predictions
New_df <- data.frame(x = seq(from = 0, to = 100, by = 0.25),
                     predicted = Predict_M$fit,
                     CI_low = Predict_M$fit - 1.96*Predict_M$se.fit,
                     CI_high = Predict_M$fit + 1.96*Predict_M$se.fit)

# add to list
GAM_RES[[5]] <- New_df



# globulin ----
plot(x = DF$Agriculture, DF$Globulin..g.dL., pch = 19, col = "red") 
plot(x = DF$Pasture, DF$Globulin..g.dL., pch = 19, col = "red") 
plot(x = DF$Plantation, DF$Globulin..g.dL., pch = 19, col = "red") 
plot(x = DF$Native_forest, DF$Globulin..g.dL., pch = 19, col = "red") 
plot(x = DF$Water, DF$Globulin..g.dL., pch = 19, col = "red") 
plot(x = DF$min_HFI, DF$Globulin..g.dL., pch = 19, col = "red") 
plot(x = DF$mean_HFI, DF$Globulin..g.dL., pch = 19, col = "red") 
plot(x = DF$max_HFI, DF$Globulin..g.dL., pch = 19, col = "red") 

M_GLOB <- gam(Globulin..g.dL. ~ min_HFI +
                s(ID, bs = "re"), # random effect on ID
              data = DF,
              family = tw(link = "log"), # tweedie distribution
              method = "REML")
summary(M_GLOB) 

# create a dataframe to feed into predictions
New_df <- data.frame(min_HFI = seq(from = min(DF$min_HFI), to = max(DF$min_HFI), length.out = 100),
                     ID = "null")
New_df$min_HFI <- as.numeric(New_df$min_HFI)
New_df$ID <- as.factor(New_df$ID)

# make predictions
Predict_M <- predict(M_GLOB,
                     newdata = New_df,
                     type = "response",
                     exclude = c("s(ID)"), 
                     se.fit = TRUE)

# make a new dataframe to include model predictions
New_df <- data.frame(x = seq(from = min(DF$min_HFI), to = max(DF$min_HFI), length.out = 100),
                     predicted = Predict_M$fit,
                     CI_low = Predict_M$fit - 1.96*Predict_M$se.fit,
                     CI_high = Predict_M$fit + 1.96*Predict_M$se.fit)

# add to list
GAM_RES[[6]] <- New_df

# add names to results list
names(GAM_RES) <- c("WBC.Pasture", "WBC.Agriculture", "Seg.Neutrophils.Pasture", "Basophils.HFI", "Platelets.Agriculture")

# save results for plotting
save(GAM_RES, file = "./Projects/RSF_Health/RESULTS/GAM_IND_preds.rda")





