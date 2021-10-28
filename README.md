# MASTER THESIS
TITLE: Handling Missing Data for Categorical Moderators in Study-Level Meta-Analytic Data Sets

This repository includes the codes and raw data for the simulation study of my master thesis.

FILE DESCRIPTION:
Codes:
  Experimental Setting.R -> It includes the functions created to set the levels and conditions of the simulation study.
                            
                            There are 4 factors with 3 levels each:
                            1) Moderator: 1c   - 1 categorical moderator
                                          1c1c - 1 categorical and 1 continuous moderator
                                          2c   - 2 categorical moderators
                            2) Sample size: Number of individual studies in a meta-analysis.
                                            n = 10, 25 or 50.
                            3) Missingness mechanism: MCAR - missingness completely at random
                                                      MAR - missingness at random
                                                      MNAR - missingness not at random
                            4) Missingness percentage: What % of values in a moderator is missing
                                                       p = 0.1, 0.25 or 0.4.
                            
                            In total, there are 3*3*3*3 = 81 conditions. For each condition, 1000 dataset are simulated.
  Simulation.R -> It includes the simulation steps, from creating experimental design table to obtain raw results.
  Data Analysis.R -> It includes the analyses done on the raw results.
Datasets:
  SimulationConditions_LAST.csv -> It shows the aggregated results for each condition.
  SimulationResults_LAST.csv -> It shows the results for all repeated simulation steps.
  
