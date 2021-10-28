#########################
#### DATA GENERATION ####
#########################

generate_categorical_columns <- function(n, categories){
        
        # ------------------------------------------------------------------------------------#
        # The aim is to create a categorical column with equally distributed levels.          #
        # No level should have significantly higher counts than the others.                   #
        #                                                                                     #
        # INPUTS                                                                              #
        # n: Sample size (number of rows) --- integer                                         #
        # categories: Levels of a categorical variable (Always 3 levels) --- character vector #
        #                                                                                     #
        # OUTPUT                                                                              #
        # A column with categorical data (3 levels) --- character vector                      #
        #-------------------------------------------------------------------------------------#
        
        #Creating empty array with sample size n
        X <- rep(0,n)
        
        # n is always either 10, 25 or 50
        if (n==10){
                
                # 3 levels * 3 samples + 1 sample = 10 (for equal level distribution)
                X <- c(rep(categories, each=3), sample(categories, 1))
        }
        
        if (n==25){
                
                # 3 levels * 8 samples + 1 sample = 25 (for equal level distribution)
                X <- c(rep(categories, each=8), sample(categories, 1))
        }
        
        if (n==50){
                
                # 3 levels * 17 samples - 1 sample = 50 (for equal level distribution)
                X <- c(rep(categories, each=17))
                X <- X[-sample(1:length(X),1)]
        }
        
        # Returning the array with categorical values
        return (X) 
}


generate_data <- function(n, moderator){
        
        #-----------------------------------------------------------------------------------#
        # The aim is to create a dataset based on certain conditions.                       #
        #                                                                                   #
        # INPUTS                                                                            #
        # n: Sample size (number of rows) --- integer                                       #
        # moderator: Conditions (number of categorical/continuous moderators) --- character #
        #            1c:   1 categorical                                                    #
        #            1c1c: 1 categorical + 1 continuous                                     #
        #            2c:   2 categorical                                                    #
        #                                                                                   #
        # OUTPUT                                                                            #
        # A dataframe with effect size column and moderator columns --- list(dataframe)     #
        #-----------------------------------------------------------------------------------#
        
        # Identifying the levels for categorical variables
        categories1 <- c("c1","c2","c3")
        categories2 <- c("c4","c5","c6")
        
        # Identifying beta parameter for regression equations (a constant)
        b <- 0.2

        if(moderator=="1c"){
                
                # Generating a categorical column named X1
                X1 <- sample(generate_categorical_columns(n, categories1))
                
                # Calculating the effect size column as Y, using the regression equation
                Y <- b + b*(X1=="c2") + b*(X1=="c3")
                     + rnorm(n=n, mean=0, sd=sqrt(sampling_variance)) + rnorm(n=n, mean=0, sd=sqrt(tau)) # Between/Within study variances
                
                # Combining the columns into a meta-analytical data set
                generated_data <- data.frame(Y=Y, X1=X1)
        }
        
        if (moderator=="1c1c"){
                
                # Generating a categorical column named X1
                X1 <- sample(generate_categorical_columns(n, categories1))
                
                # Generating a continuous column named X2, parameters are selected arbitrarily
                X2 <- rnorm(n=n, mean=10, sd=2)
                
                # Calculating the effect size column as Y, using the regression equation
                Y <- b + b*(X1=="c2") + b*(X1=="c3") + b*X2
                     + rnorm(n=n, mean=0, sd=sqrt(sampling_variance)) + rnorm(n=n, mean=0, sd=sqrt(tau)) # Between/Within study variances
                
                # Combining the columns into a meta-analytical data set
                generated_data <- data.frame(Y=Y, X1=X1, X2=X2)
        }
        
        if (moderator=="2c"){
                
                # Generating a categorical column named X1
                X1 <- sample(generate_categorical_columns(n, categories1))
                
                # Generating a categorical column named X2
                X2 <- sample(generate_categorical_columns(n, categories2))
                
                # Calculating the effect size column as Y, using the regression equation
                Y <- b + b*(X1=="c2") + b*(X1=="c3") + b*(X2=="c5") + b*(X2=="c6")
                     + rnorm(n=n, mean=0, sd=sqrt(sampling_variance)) + rnorm(n=n, mean=0, sd=sqrt(tau)) # Between/Within study variances
                
                # Combining the columns into a meta-analytical data set
                generated_data <- data.frame(Y=Y, X1=X1, X2=X2)
        }
        
        # Returning the dataset based on the given conditions
        return(generated_data) 
}



###################
#### MECHANISM ####
###################

create_missing_data <- function(data, mechanism, percentage) {
        
        #------------------------------------------------------------------------------------#
        # The aim is to simulate missing data based on missingness conditions.               #
        #                                                                                    #
        # INPUTS                                                                             #
        # data: Full dataset without missing values --- list(dataframe)                      #
        # mechanism: Missingness mechanisms --- character                                    #
        #            mcar: missing completely at random                                      #
        #            mar:  missing at random                                                 #
        #            mnar: missing not at random                                             #
        # percentage: Missingness percentage on a column --- double                          #   
        #                                                                                    #
        # OUTPUT                                                                             #
        # A dataset with missing values on one column --- list(dataframe)                    #
        #------------------------------------------------------------------------------------#
        
        # Determining the column which will have missing values (X1, the one after Y)
        column <- names(data)[2]
        
        # Repeating the simulation until the condition is satisfied
        # Condition: All 3 levels of a category should still exist after missingness is simulated. No level should be lost.
        repeat{
                
                if (mechanism=="mcar"){
                        
                        # Simulating missing values from the determined column, based on the mechanism and percentage
                        missing_data <- delete_MCAR(ds=data, p=percentage, cols_mis=column)
                        
                        # Checking the condition (all 3 unique levels should exist in the column after missing values simulated)
                        if (sum(!is.na(unique(missing_data$X1))) == 3) break
                }
                
                if (mechanism=="mar"){
                        
                        # Simulating missing values from the determined column, based on the mechanism and percentage
                        missing_data <- delete_MAR_1_to_x(ds=data, p=percentage, cols_mis=column, cols_ctrl=names(data)[1], x=3)
                        
                        # Checking the condition (all 3 unique levels should exist in the column after missing values simulated)
                        if (sum(!is.na(unique(missing_data$X1))) == 3) break
                }
                
                if (mechanism=="mnar"){
                        
                        # Simulating missing values from the determined column, based on the mechanism and percentage
                        missing_data <- delete_MNAR_rank(ds=data, p=percentage, cols_mis=column, ties.method = "first")
                        
                        # Checking the condition (all 3 unique levels should exist in the column after missing values simulated)
                        if (sum(!is.na(unique(missing_data$X1))) == 3) break
                }
        }
        
        # Returning the dataset with missing values simulated
        return(missing_data)
}



#####################
#### PERFORMANCE ####
#####################

performance <- function(data, moderator){
        
        #----------------------------------------------------------------------------#
        # The aim is to extract the performance metrics of the meta-analysis models. #
        #                                                                            #
        # INPUTS                                                                     #
        # data: Dataset, full or recovered --- list(dataframe)                       #
        # moderator: Conditions (see function: generate_data) --- character          #
        #                                                                            #
        # OUTPUT                                                                     #
        # A vector of performance metrics --- double vector                          #
        #----------------------------------------------------------------------------#
        
        # Simple meta-analysis model with random effects (restricted maximum likelihood)
        meta_model <- rma(yi=Y, vi=sampling_variance+tau, method="REML", data=data)
        
        # Meta-regression model with moderators as predictors
        meta_regression <- rma(yi=Y, vi=sampling_variance+tau, method="REML", mods=~.-Y, data=data) 
        
        # Calculating mean-squared-error from the model
        MSE <- sum(residuals.rma(meta_regression)**2)/(nrow(data)-meta_regression$p)
        
        # Calculating deviance from the model
        deviance <- deviance.rma(meta_regression)
        
        # Calculating other parameters depends if there is 1 or more moderators.
        # If only 1 moderator (1C) => Intercept is included in calculations.
        # If more than 1 moderator (1C1C or 2C) => Intercept isn't included in calculations.
        if (moderator=="1c"){
                
                # Calculating bias of the meta-regression coefficients
                # b is fixed beta with 0.2 (see function: generate_data)
                coef_bias <- mean(meta_regression$b)-0.2
                
                # Calculating variance of the meta-regression coefficients
                coef_var <- mean(meta_regression$se**2)
                
                # Calculating p-value of meta-regression model
                coef_p <- anova.rma(meta_regression, btt="X1")$QMp
                
                # If <0.05 => the model is statistically significant.
                meta_regression_p <- coef_p 
                
        } else {
                
                # Calculating bias of the meta-regression coefficients
                # b is fixed beta with 0.2 (see function: generate_data)
                coef_bias <- mean(meta_regression$b[2:3])-0.2
                
                # Calculating variance of the meta-regression coefficients
                coef_var <- mean((meta_regression$se**2)[2:3])
                
                # Calculating p-value of meta-regression model
                coef_p <- anova.rma(meta_regression, btt="X1")$QMp
                
                # If <0.05 => the model is statistically significant.
                meta_regression_p <- anova.rma(meta_regression)$QMp
        }
        
        # Calculating heterogeneity of the effect sizes
        # If <0.05 => There is statistically significant heterogeneity.
        heterogeneity_p <- meta_model$QEp
        
        # Combining all performance metrics calculated into a vector.
        performance <- c("MSE"=MSE, "bias"=coef_bias, "var"=coef_var, "hp"=heterogeneity_p, "coefp"=coef_p, "regp"=meta_regression_p, "deviance"=deviance)
        
        # Returning the performance vector of a meta-analysis/meta-regression model
        return(performance)
}



################
#### METHOD ####
################


recover_missing_data <- function(missing_data, method){
        
        #------------------------------------------------------------------------------------#
        # The aim is to recover missing data based on missing data handling methods.         #
        #                                                                                    #
        # INPUTS                                                                             #
        # missing_data: Dataset with missing values --- list(dataframe)                      #
        # method: Missing data handling method --- character                                 #
        #         CC:   Complete Case                                                        #
        #         MLR:  Multinomial Logistic Regression                                      #
        #         MI:   Multiple Imputation (with chained equations)                         #
        #         JOMO: JOint MOdelling multiple imputation                                  #
        #                                                                                    #
        # OUTPUT                                                                             #
        # A vector of performance metrics based on recovered data --- double vector          #
        #------------------------------------------------------------------------------------#
        
        if (method=="CC"){
                
                # Recovered data is only the complete cases.
                recovered_data <- missing_data[complete.cases(missing_data),]
                
                # Overwriting the performance metrics
                recovered_data <- performance(recovered_data, moderator)
        }
        
        if (method=="MLR"){
                
                # Separating the data into complete and missing parts
                complete_data <- missing_data[complete.cases(missing_data),]
                missing_part <- missing_data[!complete.cases(missing_data),]
                
                # Fitting multinomial logistic regression on complete data
                mlr <- multinom(X1~., data=complete_data)
                
                # Defining the missing part as a new data set (The column with missing values is excluded.)
                newdata <- data.frame(missing_part[,-2])
                colnames(newdata)[1] <- "Y"
                
                # Predicting the missing values using the model fitted
                predictions <- predict(mlr, newdata = newdata, "class")
                
                # Adding predicted values into missing part
                missing_part$X1 <- predictions
                
                # Combining complete and missing(with predictions) parts into recovered data
                recovered_data <- rbind(complete_data, missing_part)
                
                # Overwriting the performance metrics
                recovered_data <- performance(recovered_data, moderator)
        }
        
        if (method=="MI"){
                
                # Fitting a multiple imputation model on the data
                imputation <- mice(missing_data, m=m, method="polyreg", printFlag=F)
                
                # Getting the imputation table
                imputation_table <- t(imputation$imp$X1)
                
                # Getting the indexes of missing values
                missing_value_indexes <- as.integer(colnames(imputation_table))
                
                # Defining empty list for imputed datasets
                imputed_datasets <- list()
                
                # Replacing missing values with imputed values over a loop
                for(i in 1:m){
                        missing_data$X1[missing_value_indexes] <- imputation$imp$X1[,i]
                        imputed_datasets[[i]] <- missing_data
                }
                
                # Creating an empty dataframe for the performances of imputed data
                imputation_performance_table <- data.frame( 'MSE_recovered' = rep(0,m),
                                                            'coef_bias_recovered' = rep(0,m),
                                                            'coef_var_recovered' = rep(0,m),
                                                            'heterogeneity.p_recovered' = rep(0,m),
                                                            'coef.p_recovered' = rep(0,m),
                                                            'meta.regression.p_recovered' = rep(0,m),
                                                            "deviance_recovered"= rep(0,m))
                
                # Obtaining performance values for the imputed datasets over a loop
                for (i in 1:m){
                        imputation_performance_table[i,] <- performance(imputed_datasets[[i]], moderator)
                }
                
                # Selecting median values as the ultimate performance values
                recovered_data <- colMedians(as.matrix(imputation_performance_table), hasNA=F)
        }

        if (method=="JOMO"){

                # Separating data into Y (effect sizes) and X (moderators)
                Y <- data.frame(missing_data[,2])
                X <- data.frame(missing_data[,-2])
                
                # Fitting imputation model on the data with missing values
                latent_imputation <- jomo1cat(Y.cat=Y, Y.numcat=3, X=X, nburn=100, nbetween=100, nimp=m, output=0)
                
                # Changing the data structure for easier analysis
                latent_imputation <- spread(data=latent_imputation, key=Imputation, value=names(latent_imputation)[1])
                latent_imputation <- latent_imputation[,-(1:(ncol(latent_imputation)-m))]
                
                # Defining empty list for imputed datasets
                imputed_datasets <- list()
                
                # Replacing missing values with imputed values over a loop
                for(i in 1:m){
                        missing_data$X1 <- latent_imputation[,i]
                        imputed_datasets[[i]] <- missing_data
                }
                
                # Creating an empty dataframe for the performances of imputed data
                imputation_performance_table <- data.frame( 'MSE_recovered' = rep(0,m),
                                                            'coef_bias_recovered' = rep(0,m),
                                                            'coef_var_recovered' = rep(0,m),
                                                            'heterogeneity.p_recovered' = rep(0,m),
                                                            'coef.p_recovered' = rep(0,m),
                                                            'meta.regression.p_recovered' = rep(0,m),
                                                            "deviance_recovered"= rep(0,m))
                
                # Obtaining performance values for the imputed datasets over a loop
                for (i in 1:m){
                        imputation_performance_table[i,] <- performance(imputed_datasets[[i]], moderator)
                }
                
                # Selecting median values as the ultimate performance values
                recovered_data <- colMedians(as.matrix(imputation_performance_table), hasNA=F)
        }
        
        # Returning the final performance metrics based on the missing data handling method
        return(recovered_data)
}