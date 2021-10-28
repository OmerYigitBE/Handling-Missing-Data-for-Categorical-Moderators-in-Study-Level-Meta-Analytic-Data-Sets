###################
### PREPARATION ###
###################

# Importing relevant libraries
library(ggplot2)
library(missMethods)
library(metafor)
library(Amelia)
library(fastDummies)
library(car)
library(nnet)
library(MASS)
library(jomo)
library(tidyverse)
library(dplyr)
library(mice)
library(robustbase)
library(mitools)
library(mitml)
library(sjstats)
library(xtable)

# Removing all objects for repeated runs
rm(list=setdiff(ls(), "full_datasets"))

# Setting global parameters (assumed constants)
sampling_variance <- 1/(100-3)
m <- 5
tau <- 0.171

# Importing functions for experimental setting
source("Experimental Setting.R")



###########################
### SIMULATION SETTINGS ###
###########################

# Defining simulation settings and conditions
# There are 3*3*3*3 = 81 conditions
mechanism <- c("mcar","mar","mnar")     # 3 missingness mechanisms
percentage <- c(0.1, 0.25, 0.4)         # 3 missingness percentages (10%, 25%, 40%)
n <- c(10,25,50)                        # 3 sample sizes (number of individual studies)
moderator <- c("1c","1c1c","2c")        # 3 moderator settings (see function: generate_data)
method <- c("CC","MLR","MI","JOMO")     # Missing data handling methods (see function: recover_missing_data)
alpha <- .05                            # Statistical significance level
I <-1000                                # Number of iterations (per condition)


# Creating a dataframe for simulation conditions
conditions <- expand.grid(mechanism, percentage, n, moderator)    # Combining condition factors into data frame
names(conditions) <- c("mechanism","percentage","n","moderator")  # Renaming data frame columns
conditions <- cbind('id' = 1:nrow(conditions),conditions)         # Adding ID column to conditions data frame


# Creating a dataframe for simulation results
simresults <- data.frame('id' = numeric(I*nrow(conditions)),                         # Initialize simulation results data frame
                         'simulation' = numeric(I*nrow(conditions)),
                         'MSE_full' = numeric(I*nrow(conditions)),                   # Performance results for full datasets
                         'deviance_full' = numeric(I*nrow(conditions)),
                         'coef_bias_full' = numeric(I*nrow(conditions)),
                         'coef_var_full' = numeric(I*nrow(conditions)),
                         'heterogeneity.p_full' = numeric(I*nrow(conditions)),
                         'coef.p_full' = numeric(I*nrow(conditions)),
                         'meta.regression.p_full' = numeric(I*nrow(conditions)),
                         'MSE_recovered' = numeric(I*nrow(conditions)),              # Performance results for recovered datasets
                         'deviance_recovered' = numeric(I*nrow(conditions)),
                         'coef_bias_recovered' = numeric(I*nrow(conditions)),
                         'coef_var_recovered' = numeric(I*nrow(conditions)),
                         'heterogeneity.p_recovered' = numeric(I*nrow(conditions)),
                         'coef.p_recovered' = numeric(I*nrow(conditions)),
                         'meta.regression.p_recovered' = numeric(I*nrow(conditions)))



################################
### GENERATING FULL DATASETS ###
################################

# Defining a dataframe for data generation conditions, and an empty list for full datasets
data_generation_conditions <- conditions[,c("id","n","moderator")]
full_datasets <- list()


# Generating full datasets over a nested loop
step <- 1
for (id in data_generation_conditions$id){
        
        # Getting the condition information
        cond <- data_generation_conditions[data_generation_conditions$id==id,]
        n <- cond$n
        moderator <- cond$moderator
        
        # Generating full datasets for each condition, 1000 times
        # In total 81*1000 = 81000 datasets are generated.
        for (i in 1:I){
                full_datasets[[step]] <- generate_data(n,moderator)
        }
}



#################################
### ACTUAL SIMULATION PROCESS ###
#################################

# Defining initial steps
# Each method is manually simulated separately (due to time & computation power limits).
sim <- 1
method <- "MI" 


# Simulation loop for each condition
for (id in conditions$id){
        
        # Getting conditions from conditions dataframe
        condition <- conditions[conditions$id == id, ]
        
        # Getting simulation settings
        mechanism <- condition$mechanism
        percentage <- condition$percentage
        n <- condition$n
        moderator <- condition$moderator
        
        # Simulation loop for each dataset generated for a condition
        for (i in 1:I){
                
                # Getting ID number and simulation number
                simresults$id[sim] <- id
                simresults$simulation[sim]<-i
                
                # Getting generated data
                generated_data <- full_datasets[[sim]]
                
                # Obtaining performance measures for full dataset
                full_performance <- performance(generated_data,moderator)
                simresults$MSE_full[sim] <- full_performance[1]
                simresults$coef_bias_full[sim] <- full_performance[2]
                simresults$coef_var_full[sim] <- full_performance[3]
                simresults$heterogeneity.p_full[sim] <- full_performance[4]
                simresults$coef.p_full[sim] <- full_performance[5]
                simresults$meta.regression.p_full[sim] <- full_performance[6]
                simresults$deviance_full[sim] <- full_performance[7]
                
                # Simulating missingness
                missing_data <- create_missing_data(generated_data,mechanism,percentage)
                
                # Recovering missing data, based on the method in line 112
                recovered_data <- recover_missing_data(missing_data,method)
                
                # Obtaining performance measures for recovered dataset
                recovered_performance <- recovered_data
                simresults$MSE_recovered[sim] <- recovered_performance[1]
                simresults$coef_bias_recovered[sim] <- recovered_performance[2]
                simresults$coef_var_recovered[sim] <- recovered_performance[3]
                simresults$heterogeneity.p_recovered[sim] <- recovered_performance[4]
                simresults$coef.p_recovered[sim] <- recovered_performance[5]
                simresults$meta.regression.p_recovered[sim] <- recovered_performance[6]
                simresults$deviance_recovered[sim] <- recovered_performance[7]
        }
}

# Writing the simulation results as a checkpoint
# Name is changed manually based on method used.
write.csv(simresults, "MI.csv", row.names = F)


#############################
### OBTAINING RAW RESULTS ###
#############################

# Reading results for each method
simcc <- read.csv("CC.csv")
simmlr <- read.csv("MLR.csv")
simmi <- read.csv("MI.csv")
simjomo <- read.csv("JOMO.csv")

# Adding method columns
simcc$method <- "CC"
simmlr$method <- "MLR"
simmi$method <- "MI"
simjomo$method <- "JOMO"

# Combining the results into a single dataframe
simresults <- rbind(simcc,simmlr,simmi,simjomo)

# Writing it as a checkpoint
simresults <- write.csv(simresults, "SimulationResultsFULL.csv",row.names = F)

# Removing all objects for future analyses
rm(list = ls())