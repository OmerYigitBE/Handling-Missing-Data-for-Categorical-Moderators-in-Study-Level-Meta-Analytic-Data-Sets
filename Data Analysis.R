# Reading simulation results
simresults <- read.csv("SimulationResultsFULL.csv")



################################
### CREATING AGGREGATED DATA ###
################################

# Adding missing data handling methods to the simulation results dataset
method_vector <- rep(c("CC","MLR","MICE","JOMO"), each=81)
conditions$method <- method_vector


## POWER (COVERAGE) CALCULATIONS ##
# 1 if p-value is smaller than alpha (0.05)
simresults$qp_full<-1*(simresults$heterogeneity.p_full<alpha)
simresults$coefp_full <- 1*(simresults$coef.p_full<alpha)
simresults$rp_full<-1*(simresults$meta.regression.p_full<alpha)
simresults$qp_recovered<-1*(simresults$heterogeneity.p_recovered<alpha)
simresults$coefp_recovered <- 1*(simresults$coef.p_recovered<alpha)
simresults$rp_recovered<-1*(simresults$meta.regression.p_recovered<alpha)


# Aggregating power measures on full datasets
conditions$qp_full_power <-(aggregate(qp_full~id, full_results, mean)$qp_full)
conditions$rp_full_power <-(aggregate(rp_full~id, full_results, mean)$rp_full)
conditions$coefp_full_power <- (aggregate(coefp_full~id, full_results, mean)$coefp_full)


# Aggregating power measures of recovered datasets (for each missing data handling method)
qp_recovered_power_cc <-(aggregate(qp_recovered~id, simresults[simresults$method=="CC",], mean)$qp_recovered)               # CC
rp_recovered_power_cc <-(aggregate(rp_recovered~id, simresults[simresults$method=="CC",], mean)$rp_recovered)
coefp_recovered_power_cc <- (aggregate(coefp_recovered~id, simresults[simresults$method=="CC",], mean)$coefp_recovered)
qp_recovered_power_mlr <-(aggregate(qp_recovered~id, simresults[simresults$method=="MLR",], mean)$qp_recovered)             # MLR
rp_recovered_power_mlr <-(aggregate(rp_recovered~id, simresults[simresults$method=="MLR",], mean)$rp_recovered)
coefp_recovered_power_mlr <- (aggregate(coefp_recovered~id, simresults[simresults$method=="MLR",], mean)$coefp_recovered)
qp_recovered_power_mice <-(aggregate(qp_recovered~id, simresults[simresults$method=="MICE",], mean)$qp_recovered)           # MICE
rp_recovered_power_mice <-(aggregate(rp_recovered~id, simresults[simresults$method=="MICE",], mean)$rp_recovered)
coefp_recovered_power_mice <- (aggregate(coefp_recovered~id, simresults[simresults$method=="MICE",], mean)$coefp_recovered)
qp_recovered_power_jomo <-(aggregate(qp_recovered~id, simresults[simresults$method=="JOMO",], mean)$qp_recovered)           # JOMO
rp_recovered_power_jomo <-(aggregate(rp_recovered~id, simresults[simresults$method=="JOMO",], mean)$rp_recovered)
coefp_recovered_power_jomo <- (aggregate(coefp_recovered~id, simresults[simresults$method=="JOMO",], mean)$coefp_recovered)


# Creating power vectors for p-values
qp_recovered_power <- c(qp_recovered_power_cc,qp_recovered_power_mlr,qp_recovered_power_mice,qp_recovered_power_jomo)
rp_recovered_power <- c(rp_recovered_power_cc,rp_recovered_power_mlr,rp_recovered_power_mice,rp_recovered_power_jomo)
coefp_recovered_power <- c(coefp_recovered_power_cc,coefp_recovered_power_mlr,coefp_recovered_power_mice,coefp_recovered_power_jomo)


# Adding power values to aggregated data
conditions$qp_recovered_power <- qp_recovered_power
conditions$rp_recovered_power <- rp_recovered_power
conditions$coefp_recovered_power <- coefp_recovered_power


# Aggregating performance metrics for full datasets
conditions$MSE_full <- (aggregate(MSE_full~id, simresults, mean)$MSE_full)
conditions$coef_bias_full <- (aggregate(coef_bias_full~id, simresults, mean)$coef_bias_full)
conditions$coef_var_full <- (aggregate(coef_var_full~id, simresults, mean)$coef_var_full)


# Aggregating performance metrics for recovered datasets
conditions$MSE_recovered <- (aggregate(MSE_recovered~id+method, simresults, mean)$MSE_recovered)
conditions$coef_bias_recovered <- (aggregate(coef_bias_recovered~id+method, simresults, mean)$coef_bias_recovered)
conditions$coef_var_recovered <- (aggregate(coef_var_recovered~id+method, simresults, mean)$coef_var_recovered)


# Writing raw results and aggregated results (checkpoint)
write.csv(conditions, "SimulationConditions_LAST.csv", row.names = F)
write.csv(simresults, "SimulationResults_LAST.csv", row.names=F)



#############################
### DATASETS FOR ANALYSES ###
#############################

# Raw results
simresults <- read.csv("SimulationResults_LAST.csv", header=T)

# Aggregated results
conditions <- read.csv("SimulationConditions_LAST.csv")



####################################
### ANALYZING SIMULATION RESULTS ###
####################################

# Full vs. Recovered comparison (Paired t-test)
t.test(simresults$MSE_full, simresults$MSE_recovered, paired = TRUE, alternative = "two.sided")
t.test(simresults$coef_bias_full, simresults$coef_bias_recovered, paired = TRUE, alternative = "two.sided")
t.test(simresults$coef_var_full, simresults$coef_var_recovered, paired = TRUE, alternative = "two.sided")


# Overall method performances
overall_methods_full <- cbind(conditions[,c(6,13:15)], data.frame(dataset="full"))
overall_methods_recovered <- cbind(conditions[,c(6,17:19)], data.frame(dataset="recovered"))
names(overall_methods_full)[2:4] <- c("MSE","bias","variance"); names(overall_methods_recovered)[2:4] <- c("MSE","bias","variance")
overall_methods <- rbind(overall_methods_full,overall_methods_recovered)
overall_methods <- overall_methods[244:648,-5]
overall_methods$method <- rep(c("Full","CC","MLR","MICE","JOMO"), each=81)
cond <- conditions[1:81,2:5]
overall_methods <- cbind(cond, overall_methods)
overall_methods$method <- factor(overall_methods$method, levels=c("Full","CC","MLR","MICE","JOMO"))
overall_methods$percentage <- factor(overall_methods$percentage, levels=c("0.1","0.25","0.4"))
overall_methods$n <- factor(overall_methods$n, levels=c("10","25","40"))


## SUMMARY STATISTICS TABLES FOR PERFORMANCE METRICS ##
# MSE
MSE_table <- data.frame(method=c("CC","MLR","MICE", "JOMO","Full data"),
                        mean=c(mean(conditions[conditions$method=="CC",]$MSE_recovered), mean(conditions[conditions$method=="MLR",]$MSE_recovered), mean(conditions[conditions$method=="MICE",]$MSE_recovered), mean(conditions[conditions$method=="JOMO",]$MSE_recovered), mean(conditions$MSE_full)),
                        median=c(median(conditions[conditions$method=="CC",]$MSE_recovered), median(conditions[conditions$method=="MLR",]$MSE_recovered), median(conditions[conditions$method=="MICE",]$MSE_recovered), median(conditions[conditions$method=="JOMO",]$MSE_recovered), median(conditions[conditions$method=="JOMO",]$MSE_full)),
                        sd=c(sd(conditions[conditions$method=="CC",]$MSE_recovered), sd(conditions[conditions$method=="MLR",]$MSE_recovered), sd(conditions[conditions$method=="MICE",]$MSE_recovered), sd(conditions[conditions$method=="JOMO",]$MSE_recovered), sd(conditions[conditions$method=="JOMO",]$MSE_full)))
MSE_table[,-1] <- round(MSE_table[,-1],4)

# Bias
bias_table <- data.frame(method=c("CC","MLR","MICE", "JOMO","Full data"),
                         mean=c(mean(conditions[conditions$method=="CC",]$coef_bias_recovered), mean(conditions[conditions$method=="MLR",]$coef_bias_recovered), mean(conditions[conditions$method=="MICE",]$coef_bias_recovered), mean(conditions[conditions$method=="JOMO",]$coef_bias_recovered), mean(conditions$coef_bias_full)),
                         median=c(median(conditions[conditions$method=="CC",]$coef_bias_recovered), median(conditions[conditions$method=="MLR",]$coef_bias_recovered), median(conditions[conditions$method=="MICE",]$coef_bias_recovered), median(conditions[conditions$method=="JOMO",]$coef_bias_recovered), median(conditions[conditions$method=="JOMO",]$coef_bias_full)),
                         sd=c(sd(conditions[conditions$method=="CC",]$coef_bias_recovered), sd(conditions[conditions$method=="MLR",]$coef_bias_recovered), sd(conditions[conditions$method=="MICE",]$coef_bias_recovered), sd(conditions[conditions$method=="JOMO",]$coef_bias_recovered), sd(conditions[conditions$method=="JOMO",]$coef_bias_full)))
bias_table[,-1] <- round(bias_table[,-1],4)

# Variance
variance_table <- data.frame(method=c("CC","MLR","MICE", "JOMO","Full data"),
                             mean=c(mean(conditions[conditions$method=="CC",]$coef_var_recovered), mean(conditions[conditions$method=="MLR",]$coef_var_recovered), mean(conditions[conditions$method=="MICE",]$coef_var_recovered), mean(conditions[conditions$method=="JOMO",]$coef_var_recovered), mean(conditions$coef_var_full)),
                             median=c(median(conditions[conditions$method=="CC",]$coef_var_recovered), median(conditions[conditions$method=="MLR",]$coef_var_recovered), median(conditions[conditions$method=="MICE",]$coef_var_recovered), median(conditions[conditions$method=="JOMO",]$coef_var_recovered), median(conditions[conditions$method=="JOMO",]$coef_var_full)),
                             sd=c(sd(conditions[conditions$method=="CC",]$coef_var_recovered), sd(conditions[conditions$method=="MLR",]$coef_var_recovered), sd(conditions[conditions$method=="MICE",]$coef_var_recovered), sd(conditions[conditions$method=="JOMO",]$coef_var_recovered), sd(conditions[conditions$method=="JOMO",]$coef_var_full)))
variance_table[,-1] <- round(variance_table[,-1],4)


## BOXPLOTS ##
ggplot(overall_methods, aes(x=method, y=MSE, fill=1)) + stat_boxplot(geom ='errorbar', width=0.5) + geom_boxplot() + theme(legend.position = "none")
ggplot(overall_methods, aes(x=method, y=bias, fill=1)) + stat_boxplot(geom ='errorbar', width=0.5) + geom_boxplot() + theme(legend.position = "none")
ggplot(overall_methods, aes(x=method, y=variance, fill=1)) + stat_boxplot(geom ='errorbar', width=0.5) + geom_boxplot() + theme(legend.position = "none")


## T-TESTS BY CONTRASTS ##

# mean-squared-error (full vs. methods)
t.test(overall_methods[overall_methods$method=="CC",]$MSE, overall_methods[overall_methods$method=="Full",]$MSE)
t.test(overall_methods[overall_methods$method=="MLR",]$MSE, overall_methods[overall_methods$method=="Full",]$MSE)
t.test(overall_methods[overall_methods$method=="MICE",]$MSE, overall_methods[overall_methods$method=="Full",]$MSE)
t.test(overall_methods[overall_methods$method=="JOMO",]$MSE, overall_methods[overall_methods$method=="Full",]$MSE)

# mean-squared-error (CC vs. other methods)
t.test(overall_methods[overall_methods$method=="MLR",]$MSE, overall_methods[overall_methods$method=="CC",]$MSE)
t.test(overall_methods[overall_methods$method=="MICE",]$MSE, overall_methods[overall_methods$method=="CC",]$MSE)
t.test(overall_methods[overall_methods$method=="JOMO",]$MSE, overall_methods[overall_methods$method=="CC",]$MSE)


# bias (full vs. methods)
t.test(overall_methods[overall_methods$method=="CC",]$bias, overall_methods[overall_methods$method=="Full",]$bias)
t.test(overall_methods[overall_methods$method=="MLR",]$bias, overall_methods[overall_methods$method=="Full",]$bias)
t.test(overall_methods[overall_methods$method=="MICE",]$bias, overall_methods[overall_methods$method=="Full",]$bias)
t.test(overall_methods[overall_methods$method=="JOMO",]$bias, overall_methods[overall_methods$method=="Full",]$bias)

# bias (CC vs. other methods)
t.test(overall_methods[overall_methods$method=="MLR",]$bias, overall_methods[overall_methods$method=="CC",]$bias)
t.test(overall_methods[overall_methods$method=="MICE",]$bias, overall_methods[overall_methods$method=="CC",]$bias)
t.test(overall_methods[overall_methods$method=="JOMO",]$bias, overall_methods[overall_methods$method=="CC",]$bias)


# variance (full vs. methods)
t.test(overall_methods[overall_methods$method=="CC",]$variance, overall_methods[overall_methods$method=="Full",]$variance)
t.test(overall_methods[overall_methods$method=="MLR",]$variance, overall_methods[overall_methods$method=="Full",]$variance)
t.test(overall_methods[overall_methods$method=="MICE",]$variance, overall_methods[overall_methods$method=="Full",]$variance)
t.test(overall_methods[overall_methods$method=="JOMO",]$variance, overall_methods[overall_methods$method=="Full",]$variance)

# variance (CC vs. other methods)
t.test(overall_methods[overall_methods$method=="MLR",]$variance, overall_methods[overall_methods$method=="CC",]$variance)
t.test(overall_methods[overall_methods$method=="MICE",]$variance, overall_methods[overall_methods$method=="CC",]$variance)
t.test(overall_methods[overall_methods$method=="JOMO",]$variance, overall_methods[overall_methods$method=="CC",]$variance)


## F-TESTS FOR PERFORMANCE VARIATION COMPARISON ##

# mean-squared-error (full vs. methods)
var.test(overall_methods[overall_methods$method=="CC",]$MSE, overall_methods[overall_methods$method=="Full",]$MSE)
var.test(overall_methods[overall_methods$method=="MLR",]$MSE, overall_methods[overall_methods$method=="Full",]$MSE)
var.test(overall_methods[overall_methods$method=="MICE",]$MSE, overall_methods[overall_methods$method=="Full",]$MSE)
var.test(overall_methods[overall_methods$method=="JOMO",]$MSE, overall_methods[overall_methods$method=="Full",]$MSE)

# mean-squared-error (methods)
var.test(overall_methods[overall_methods$method=="MLR",]$MSE, overall_methods[overall_methods$method=="CC",]$MSE)
var.test(overall_methods[overall_methods$method=="MICE",]$MSE, overall_methods[overall_methods$method=="CC",]$MSE)
var.test(overall_methods[overall_methods$method=="JOMO",]$MSE, overall_methods[overall_methods$method=="CC",]$MSE)
var.test(overall_methods[overall_methods$method=="MLR",]$MSE, overall_methods[overall_methods$method=="MICE",]$MSE)
var.test(overall_methods[overall_methods$method=="MLR",]$MSE, overall_methods[overall_methods$method=="JOMO",]$MSE)
var.test(overall_methods[overall_methods$method=="MICE",]$MSE, overall_methods[overall_methods$method=="JOMO",]$MSE)


# bias (full vs. methods)
var.test(overall_methods[overall_methods$method=="CC",]$bias, overall_methods[overall_methods$method=="Full",]$bias)
var.test(overall_methods[overall_methods$method=="MLR",]$bias, overall_methods[overall_methods$method=="Full",]$bias)
var.test(overall_methods[overall_methods$method=="MICE",]$bias, overall_methods[overall_methods$method=="Full",]$bias)
var.test(overall_methods[overall_methods$method=="JOMO",]$bias, overall_methods[overall_methods$method=="Full",]$bias)

# bias (methods)
var.test(overall_methods[overall_methods$method=="MLR",]$bias, overall_methods[overall_methods$method=="CC",]$bias)
var.test(overall_methods[overall_methods$method=="MICE",]$bias, overall_methods[overall_methods$method=="CC",]$bias)
var.test(overall_methods[overall_methods$method=="JOMO",]$bias, overall_methods[overall_methods$method=="CC",]$bias)
var.test(overall_methods[overall_methods$method=="MLR",]$bias, overall_methods[overall_methods$method=="MICE",]$bias)
var.test(overall_methods[overall_methods$method=="MLR",]$bias, overall_methods[overall_methods$method=="JOMO",]$bias)
var.test(overall_methods[overall_methods$method=="MICE",]$bias, overall_methods[overall_methods$method=="JOMO",]$bias)


# variance (full vs. methods)
var.test(overall_methods[overall_methods$method=="CC",]$variance, overall_methods[overall_methods$method=="Full",]$variance)
var.test(overall_methods[overall_methods$method=="MLR",]$variance, overall_methods[overall_methods$method=="Full",]$variance)
var.test(overall_methods[overall_methods$method=="MICE",]$variance, overall_methods[overall_methods$method=="Full",]$variance)
var.test(overall_methods[overall_methods$method=="JOMO",]$variance, overall_methods[overall_methods$method=="Full",]$variance)

# variance (methods)
var.test(overall_methods[overall_methods$method=="MLR",]$variance, overall_methods[overall_methods$method=="CC",]$variance)
var.test(overall_methods[overall_methods$method=="MICE",]$variance, overall_methods[overall_methods$method=="CC",]$variance)
var.test(overall_methods[overall_methods$method=="JOMO",]$variance, overall_methods[overall_methods$method=="CC",]$variance)
var.test(overall_methods[overall_methods$method=="MLR",]$variance, overall_methods[overall_methods$method=="MICE",]$variance)
var.test(overall_methods[overall_methods$method=="MLR",]$variance, overall_methods[overall_methods$method=="JOMO",]$variance)
var.test(overall_methods[overall_methods$method=="MICE",]$variance, overall_methods[overall_methods$method=="JOMO",]$variance)


## PROFILE PLOTS ##

# Data table for main effects
main_mechanism <- conditions %>% group_by(method, mechanism) %>% summarize(MSE = mean(MSE_recovered), bias = mean(coef_bias_recovered), variance = mean(coef_var_recovered))
main_percentage <- conditions %>% group_by(method, percentage) %>% summarize(MSE = mean(MSE_recovered), bias = mean(coef_bias_recovered), variance = mean(coef_var_recovered))
main_n <- conditions %>% group_by(method, n) %>% summarize(MSE = mean(MSE_recovered), bias = mean(coef_bias_recovered), variance = mean(coef_var_recovered))
main_moderator <- conditions %>% group_by(method, moderator) %>% summarize(MSE = mean(MSE_recovered), bias = mean(coef_bias_recovered), variance = mean(coef_var_recovered))
# Rounding the numbers
main_mechanism[,-c(1,2)] <- round(main_mechanism[,-c(1,2)],4)
main_percentage[,-c(1,2)] <- round(main_percentage[,-c(1,2)],4)
main_n[,-c(1,2)] <- round(main_n[,-c(1,2)],4)
main_moderator[,-c(1,2)] <- round(main_moderator[,-c(1,2)],4)
full_performance <- conditions[c(1:81),c(2:6,7:9,13:15)]

# Data tables for interaction effect
interaction_percentage_n <-  conditions %>% group_by(method, percentage, n) %>% summarize(MSE = mean(MSE_recovered), bias = mean(coef_bias_recovered), variance = mean(coef_var_recovered))
interaction_moderator_n <-  conditions %>% group_by(method, moderator, n) %>% summarize(MSE = mean(MSE_recovered), bias = mean(coef_bias_recovered), variance = mean(coef_var_recovered))
interaction_mechanism_percentage <-  conditions %>% group_by(method, mechanism, percentage) %>% summarize(bias = mean(coef_bias_recovered))
interaction_mechanism_moderator <-  conditions %>% group_by(method, mechanism, moderator) %>% summarize(bias = mean(coef_bias_recovered))
interaction_percentage_moderator <-  conditions %>% group_by(method, percentage, moderator) %>% summarize(bias = mean(coef_bias_recovered))
# Rounding the numbers
interaction_percentage_n[,-c(1:3)] <- round(interaction_percentage_n[,-c(1:3)],4)
interaction_moderator_n[,-c(1:3)] <- round(interaction_moderator_n[,-c(1:3)],4)
interaction_mechanism_percentage[,-c(1:3)] <- round(interaction_mechanism_percentage[,-c(1:3)],4)
interaction_mechanism_moderator[,-c(1:3)] <- round(interaction_mechanism_moderator[,-c(1:3)],4)
interaction_percentage_moderator[,-c(1:3)] <- round(interaction_percentage_moderator[,-c(1:3)],4)


## TESTING THE SIGNIFICANCES OF THE PERFORMANCE RESULTS ##

#TOTAL
mse_model <- lm(MSE_recovered~mechanism:method+percentage:method+n:method+moderator:method+mechanism+method+percentage+n+moderator+mechanism:percentage:method+mechanism:n:method+mechanism:moderator:method+percentage:n:method+percentage:moderator:method+n:moderator:method,data=conditions)
bias_model <- lm(coef_bias_recovered~mechanism+method+percentage+n+moderator+mechanism:method+percentage:method+n:method+moderator:method+mechanism:percentage:method+mechanism:n:method+mechanism:moderator:method+percentage:n:method+percentage:moderator:method+n:moderator:method,data=conditions)
variance_model <- lm(coef_var_recovered~mechanism+method+percentage+n+moderator+mechanism:method+percentage:method+n:method+moderator:method+mechanism:percentage:method+mechanism:n:method+mechanism:moderator:method+percentage:n:method+percentage:moderator:method+n:moderator:method,data=conditions)
anova(mse_model)
anova(bias_model)
anova(variance_model)

#MSE - one effect
anova(lm(MSE_formula_recovered~method+method:mechanism, data=conditions)) #Mechanism not significant
anova(lm(MSE_formula_recovered~method+method:percentage, data=conditions)) #Percentage significant
anova(lm(MSE_formula_recovered~method+method:n, data=conditions)) #Sample size significant
anova(lm(MSE_formula_recovered~method*moderator, data=conditions)) #Moderator not significant

eta_sq(lm(MSE_formula_recovered~method+method:mechanism, data=conditions)) #Mechanism not significant
eta_sq(lm(MSE_formula_recovered~method+method:percentage, data=conditions)) #Percentage significant
eta_sq(lm(MSE_formula_recovered~method+method:n, data=conditions)) #Sample size significant
eta_sq(lm(MSE_formula_recovered~method*moderator, data=conditions)) #Moderator not significant

#Bias - one effect
anova(lm(coef_bias_recovered~method*mechanism, data=conditions)) #Mechanism not significant
anova(lm(coef_bias_recovered~method*percentage, data=conditions)) #Percentage not significant
anova(lm(coef_bias_recovered~method*n, data=conditions)) #Sample size significant
anova(lm(coef_bias_recovered~method*moderator, data=conditions)) #Moderator significant

eta_sq(lm(coef_bias_recovered~method*mechanism, data=conditions)) #Mechanism not significant
eta_sq(lm(coef_bias_recovered~method*percentage, data=conditions)) #Percentage not significant
eta_sq(lm(coef_bias_recovered~method*n, data=conditions)) #Sample size significant
eta_sq(lm(coef_bias_recovered~method*moderator, data=conditions)) #Moderator significant


#Variance - one effect
anova(lm(coef_var_recovered~method*mechanism, data=conditions)) #Mechanism not significant
anova(lm(coef_var_recovered~method*percentage, data=conditions)) #Percentage significant
anova(lm(coef_var_recovered~method*n, data=conditions)) #Sample size significant
anova(lm(coef_var_recovered~method*moderator, data=conditions)) #Moderator not significant

eta_sq(lm(coef_var_recovered~method*mechanism, data=conditions)) #Mechanism not significant
eta_sq(lm(coef_var_recovered~method*percentage, data=conditions)) #Percentage significant
eta_sq(lm(coef_var_recovered~method*n, data=conditions)) #Sample size significant
eta_sq(lm(coef_var_recovered~method*moderator, data=conditions)) #Moderator not significant


#MSE - double effect
anova(lm(MSE_recovered~method*mechanism+method*percentage+method:mechanism:percentage, data=conditions)) #Mechanism & Percentage not significant
anova(lm(MSE_recovered~method*mechanism+method*n+method:mechanism:n, data=conditions)) #Mechanism & sample size not significant
anova(lm(MSE_recovered~method*mechanism+method*moderator+method:mechanism:moderator, data=conditions)) #Mechanism & moderator not significant
anova(lm(MSE_recovered~method*percentage+method*n+method:percentage:n, data=conditions)) #Percentage & sample size significant ####################
anova(lm(MSE_recovered~method*percentage+method*moderator+method:percentage:moderator, data=conditions)) #Percentage & moderator not significant
anova(lm(MSE_recovered~method*n+method*moderator+method:n:moderator, data=conditions)) #Sample size & moderator not significant

#Bias - double effect
anova(lm(coef_bias_recovered~method*mechanism+method*percentage+method:mechanism:percentage, data=conditions)) #Mechanism & Percentage not significant
anova(lm(coef_bias_recovered~method*mechanism+method*n+method:mechanism:n, data=conditions))
anova(lm(coef_bias_recovered~method*mechanism+method*moderator+method:mechanism:moderator, data=conditions))
anova(lm(coef_bias_recovered~method*percentage+method*n+method:percentage:n, data=conditions))
anova(lm(coef_bias_recovered~method*percentage+method*moderator+method:percentage:moderator, data=conditions))
anova(lm(coef_bias_recovered~method*n+method*moderator+method:n:moderator, data=conditions))

#Variance - double effect
anova(lm(coef_var_recovered~method*mechanism+method*percentage+method:mechanism:percentage, data=conditions)) #Mechanism & Percentage not significant
anova(lm(coef_var_recovered~method*mechanism+method*n+method:mechanism:n, data=conditions))
anova(lm(coef_var_recovered~method*mechanism+method*moderator+method:mechanism:moderator, data=conditions))
anova(lm(coef_var_recovered~method*percentage+method*n+method:percentage:n, data=conditions)) #Percentage & sample size significant
anova(lm(coef_var_recovered~method*percentage+method*moderator+method:percentage:moderator, data=conditions))
anova(lm(coef_var_recovered~method*n+method*moderator+method:n:moderator, data=conditions)) #sample size & moderator significant


#Sample size 10 vs. others
t.test(conditions[conditions$n==10,]$MSE_recovered, conditions[conditions$n!=10,]$MSE_recovered)
#Sample size 25 vs. 50
t.test(conditions[conditions$n==25,]$MSE_recovered, conditions[conditions$n==50,]$MSE_recovered)
#MLR bias 10% vs others
t.test(conditions[conditions$percentage==0.1 & conditions$method=="MLR",]$coef_bias_recovered, conditions[conditions$percentage!=0.1 & conditions$method=="MLR",]$coef_bias_recovered)
#MLR bias 1cat vs others
t.test(conditions[conditions$moderator=="1c" & conditions$method=="JOMO",]$coef_bias_recovered, conditions[conditions$moderator!="1c" & conditions$method=="JOMO",]$coef_bias_recovered)
#% CC vs. % others (variance)
t.test(conditions[conditions$percentage==0.10 & conditions$method=="CC",]$coef_var_recovered, conditions[conditions$percentage==0.10 & conditions$method!="CC",]$coef_var_recovered)
#n=10 vs. n=others (variance)
t.test(conditions[conditions$n==25,]$coef_var_recovered, conditions[conditions$n==50,]$coef_var_recovered)
#1c vs. others (variance)
t.test(conditions[conditions$moderator=="1c",]$coef_var_recovered, conditions[conditions$moderator!="1c",]$coef_var_recovered)
#CC 1c1c vs. 2c
t.test(conditions[conditions$moderator=="1c1c" & conditions$method=="CC",]$coef_var_recovered, conditions[conditions$moderator=="2c" & conditions$method=="CC",]$coef_var_recovered)
#Imputation 1c1c vs. 2c
t.test(conditions[conditions$moderator=="1c1c" & conditions$method!="CC",]$coef_var_recovered, conditions[conditions$moderator=="2c" & conditions$method!="CC",]$coef_var_recovered)
#MICE vs. MLR (variance)
t.test(conditions[conditions$method=="MI",]$coef_var_recovered, conditions[conditions$method=="MLR",]$coef_var_recovered)



## TESTING THE SIGNIFICANCES OF POWERS ##

# Simple t-test comparisons
t.test(conditions$qp_full_power, conditions$qp_recovered_power)
t.test(conditions$rp_full_power, conditions$rp_recovered_power)
t.test(conditions$coefp_full_power, conditions$coefp_recovered_power)
t.test(conditions$rp_recovered_power, conditions$coefp_recovered_power)

# Data tables
qp_table <- data.frame(method=c("CC","MLR","MICE", "JOMO","Full data"),
                       mean=c(mean(conditions[conditions$method=="CC",]$qp_recovered_power), mean(conditions[conditions$method=="MLR",]$qp_recovered_power), mean(conditions[conditions$method=="MICE",]$qp_recovered_power), mean(conditions[conditions$method=="JOMO",]$qp_recovered_power), mean(full_performance$qp_full_power)),
                       median=c(median(conditions[conditions$method=="CC",]$qp_recovered_power), median(conditions[conditions$method=="MLR",]$qp_recovered_power), median(conditions[conditions$method=="MICE",]$qp_recovered_power), median(conditions[conditions$method=="JOMO",]$qp_recovered_power), median(full_performance$qp_full_power)),
                       sd=c(sd(conditions[conditions$method=="CC",]$qp_recovered_power), sd(conditions[conditions$method=="MLR",]$qp_recovered_power), sd(conditions[conditions$method=="MICE",]$qp_recovered_power), sd(conditions[conditions$method=="JOMO",]$qp_recovered_power), sd(full_performance$qp_full_power)))

rp_table <- data.frame(method=c("CC","MLR","MICE", "JOMO","Full data"),
                       mean=c(mean(conditions[conditions$method=="CC",]$rp_recovered_power), mean(conditions[conditions$method=="MLR",]$rp_recovered_power), mean(conditions[conditions$method=="MICE",]$rp_recovered_power), mean(conditions[conditions$method=="JOMO",]$rp_recovered_power), mean(full_performance$rp_full_power)),
                       median=c(median(conditions[conditions$method=="CC",]$rp_recovered_power), median(conditions[conditions$method=="MLR",]$rp_recovered_power), median(conditions[conditions$method=="MICE",]$rp_recovered_power), median(conditions[conditions$method=="JOMO",]$rp_recovered_power), median(full_performance$rp_full_power)),
                       sd=c(sd(conditions[conditions$method=="CC",]$rp_recovered_power), sd(conditions[conditions$method=="MLR",]$rp_recovered_power), sd(conditions[conditions$method=="MICE",]$rp_recovered_power), sd(conditions[conditions$method=="JOMO",]$rp_recovered_power), sd(full_performance$rp_full_power)))

coefp_table <- data.frame(method=c("CC","MLR","MICE", "JOMO","Full data"),
                          mean=c(mean(conditions[conditions$method=="CC",]$coefp_recovered_power), mean(conditions[conditions$method=="MLR",]$coefp_recovered_power), mean(conditions[conditions$method=="MICE",]$coefp_recovered_power), mean(conditions[conditions$method=="JOMO",]$coefp_recovered_power), mean(full_performance$coefp_full_power)),
                          median=c(median(conditions[conditions$method=="CC",]$coefp_recovered_power), median(conditions[conditions$method=="MLR",]$coefp_recovered_power), median(conditions[conditions$method=="MICE",]$coefp_recovered_power), median(conditions[conditions$method=="JOMO",]$coefp_recovered_power), median(full_performance$coefp_full_power)),
                          sd=c(sd(conditions[conditions$method=="CC",]$coefp_recovered_power), sd(conditions[conditions$method=="MLR",]$coefp_recovered_power), sd(conditions[conditions$method=="MICE",]$coefp_recovered_power), sd(conditions[conditions$method=="JOMO",]$coefp_recovered_power), sd(full_performance$coefp_full_power)))

# Linear models
qp_model <- lm(qp_recovered_power~mechanism:method+percentage:method+n:method+moderator:method+mechanism+method+percentage+n+moderator,data=conditions)
rp_model <- lm(rp_recovered_power~mechanism:method+percentage:method+n:method+moderator:method+mechanism+method+percentage+n+moderator,data=conditions)
coefp_model <- lm(coefp_recovered_power~mechanism:method+percentage:method+n:method+moderator:method+mechanism+method+percentage+n+moderator,data=conditions)

# Anova results
anova(qp_model)
anova(rp_model)
anova(coefp_model)

# Eta-squared values
eta_sq(qp_model)
eta_sq(rp_model)
anova(coefp_model)


# Data tables for main effects
power_main_method <- conditions %>% group_by(method) %>% summarize (heterogeneity_power = mean(qp_recovered_power), model_power = mean(rp_recovered_power), coefficient_power = mean(coefp_recovered_power))
power_main_mechanism <- conditions %>% group_by(mechanism) %>% summarize (heterogeneity_power = mean(qp_recovered_power), model_power = mean(rp_recovered_power), coefficient_power = mean(coefp_recovered_power))
power_main_percentage <- conditions %>% group_by(percentage) %>% summarize (heterogeneity_power = mean(qp_recovered_power), model_power = mean(rp_recovered_power), coefficient_power = mean(coefp_recovered_power))
power_main_n <- conditions %>% group_by(n) %>% summarize (heterogeneity_power = mean(qp_recovered_power), model_power = mean(rp_recovered_power), coefficient_power = mean(coefp_recovered_power))
power_main_moderator <- conditions %>% group_by(moderator) %>% summarize (heterogeneity_power = mean(qp_recovered_power), model_power = mean(rp_recovered_power), coefficient_power = mean(coefp_recovered_power))
# Rounding the numbers
power_main_method[,-1] <- round(power_main_method[,-1],4)
power_main_mechanism[,-1] <- round(power_main_mechanism[,-1],4)
power_main_percentage[,-1] <- round(power_main_percentage[,-1],4)
power_main_n[,-1] <- round(power_main_n[,-1],4)
power_main_moderator[,-1] <- round(power_main_moderator[,-1],4)


# Data tables for interaction effects
interaction_n <-  conditions %>% group_by(method, n) %>% summarize (heterogeneity_power = mean(qp_recovered_power), model_power = mean(rp_recovered_power), coefficient_power = mean(coefp_recovered_power))
interaction_percentage <-  conditions %>% group_by(method, percentage) %>% summarize (heterogeneity_power = mean(qp_recovered_power), model_power = mean(rp_recovered_power), coefficient_power = mean(coefp_recovered_power))
interaction_moderator <-  conditions %>% group_by(method, moderator) %>% summarize (heterogeneity_power = mean(qp_recovered_power), model_power = mean(rp_recovered_power), coefficient_power = mean(coefp_recovered_power))
# Rounding the numbers
interaction_n[,-1] <- round(interaction_n[,-1], 4)
interaction_percentage[,-1] <- round(interaction_percentage[,-1], 4)
interaction_moderator[,-c(1:2)] <- round(interaction_moderator[,-c(1:2)], 4)


#Heterogeneity assumption - one effect
anova(lm(qp_recovered_power~method*mechanism, data=conditions)) #Mechanism not significant
anova(lm(qp_recovered_power~method*percentage, data=conditions)) #Percentage significant
anova(lm(qp_recovered_power~method*n, data=conditions)) #Sample size significant
anova(lm(qp_recovered_power~method*moderator, data=conditions)) #Moderator not significant

eta_sq(lm(qp_recovered_power~method*mechanism, data=conditions)) #Mechanism not significant
eta_sq(lm(qp_recovered_power~method*percentage, data=conditions)) #Percentage significant
eta_sq(lm(qp_recovered_power~n, data=conditions)) #Sample size significant
eta_sq(lm(qp_recovered_power~moderator, data=conditions)) #Moderator not significant


#Model assumption - one effect
anova(lm(rp_recovered_power~method*mechanism, data=conditions)) #Mechanism not significant
anova(lm(rp_recovered_power~method*percentage, data=conditions)) #Percentage not significant
anova(lm(rp_recovered_power~method*n, data=conditions)) #Sample size significant
anova(lm(rp_recovered_power~method*moderator, data=conditions)) #Moderator significant

eta_sq(lm(rp_recovered_power~method*mechanism, data=conditions)) #Mechanism not significant
eta_sq(lm(rp_recovered_power~percentage, data=conditions)) #Percentage not significant
eta_sq(lm(rp_recovered_power~n, data=conditions)) #Sample size significant
eta_sq(lm(rp_recovered_power~moderator, data=conditions)) #Moderator significant


#Coefficient assumption - one effect
anova(lm(coefp_recovered_power~method*mechanism, data=conditions)) #Mechanism not significant
anova(lm(coefp_recovered_power~method*percentage, data=conditions)) #Percentage significant
anova(lm(coefp_recovered_power~method*n, data=conditions)) #Sample size significant
anova(lm(coefp_recovered_power~method*moderator, data=conditions)) #Moderator not significant

eta_sq(lm(coefp_recovered_power~method*mechanism, data=conditions)) #Mechanism not significant
eta_sq(lm(coefp_recovered_power~percentage, data=conditions)) #Percentage not significant
eta_sq(lm(coefp_recovered_power~n, data=conditions)) #Sample size significant
eta_sq(lm(coefp_recovered_power~moderator, data=conditions)) #Moderator significant


## DASHED LINES FOR FULL MEASURES

# Mechanism
mechanism_dashed_full <- conditions %>% group_by(mechanism) %>% summarise(mse=mean(MSE_full), bias=mean(coef_bias_full), variance=mean(coef_var_full),
                                                                          heterogeneity_power = mean(qp_full_power),
                                                                          model_power = mean(rp_full_power),
                                                                          coefficient_power = mean(coefp_full_power))
mechanism_dashed_full[,-1] <- round(mechanism_dashed_full[,-1],4)

# n (sample size)
n_dashed_full <- conditions %>% group_by(n) %>% summarise(mse=mean(MSE_full), bias=mean(coef_bias_full), variance=mean(coef_var_full),
                                                          heterogeneity_power = mean(qp_full_power),
                                                          model_power = mean(rp_full_power),
                                                          coefficient_power = mean(coefp_full_power))
n_dashed_full[,-1] <- round(n_dashed_full[,-1],4)

# Percentage
percentage_dashed_full <- conditions %>% group_by(percentage) %>% summarise(mse=mean(MSE_full), bias=mean(coef_bias_full), variance=mean(coef_var_full),
                                                                            heterogeneity_power = mean(qp_full_power),
                                                                            model_power = mean(rp_full_power),
                                                                            coefficient_power = mean(coefp_full_power))
percentage_dashed_full[,-1] <- round(percentage_dashed_full[,-1],4)

# Moderator
moderator_dashed_full <- conditions %>% group_by(moderator) %>% summarise(mse=mean(MSE_full), bias=mean(coef_bias_full), variance=mean(coef_var_full),
                                                                          heterogeneity_power = mean(qp_full_power),
                                                                          model_power = mean(rp_full_power),
                                                                          coefficient_power = mean(coefp_full_power))
moderator_dashed_full[,-1] <- round(moderator_dashed_full[,-1],4)