##' With this script the simulations from Laabs et al. "Construction of 
##' artificial most representative trees by minimizing tree-based 
##' distance measures" can be reproduced. 
##' Please note, that the simulations in the paper were performed using 
##' batchtools on  a high throughout batch system. This script will implement 
##' the same calculations on your local system, which may lead to a high 
##' computation time. Comments will show you where you can save time or 
##' incorporate your own batch system. 

#---------------------------------------
## Define directories
## Please define your main directory here. 
## This should be the directory you cloned the git repository into.
main_dir <- getwd()
setwd(main_dir)

## Create and define registry directory
dir.create(file.path(main_dir, "registries"), showWarnings = FALSE)
reg_dir <- file.path(main_dir, "registries")
## Create and define functions directory
dir.create(file.path(main_dir, "functions"), showWarnings = FALSE)
fun_dir <- file.path(main_dir, "functions")
## Create and define proc directory
dir.create(file.path(main_dir, "proc"), showWarnings = FALSE)
proc_dir <- file.path(main_dir, "proc")

#---------------------------------------
## Load libraries
if (!"pacman" %in% installed.packages()){
  install.packages("pacman")
}

pacman::p_load(batchtools)
pacman::p_load(ranger)
pacman::p_load(devtools)
pacman::p_load(rpart)
pacman::p_load(dplyr)

if("timbR" %in% installed.packages()){
  library(timbR)
} else {
  devtools::install_github("imbs-hl/timbR", "master")
  library(timbR)
}


# --------------------------------------------------- #
#                  Data Simulation                    #
# --------------------------------------------------- #
## In this part the data will be simulated and saved 
## for later use

#---------------------------------------
## load functions needed for the simulation

## functions to simulate the data sets
source("functions/simulate_rf_setting_1.R")
source("functions/simulate_rf_setting_2.R")
source("functions/simulate_rf_setting_3.R")
source("functions/simulate_rf_setting_4.R")
source("functions/simulate_rf_setting_5.R")

## functions to generate the MRT or ART
source("functions/calculate_most_rep_tree.R")
source("functions/calculate_art_rep_tree.R")

#---------------------------------------
## set parameters for simulation

## parameter of data set
n         <- 1000    ## Number of samples in train data set
n_test    <- 100     ## Number of samples in test data set
n_val     <- 1000    ## Number of samples in validation data set
p         <- 100     ## Number of variables 
num_trees <- 500     ## Number of trees in random forest
eps       <- 1       ## Simulated noise in data set

## parameter of random forest
num.trees <- 500                               ## Number of trees in random forest
mtry     <- sqrt(p)                            ## Mtry for random forest
min_node_size <- 100                           ## Minimal node size for random forest

## parameter of MRT and ART
metric   <- c("weighted splitting variables")  ## Simularity / distance measure for selecting MRT or building ART

## parameter of ART
imp.num.var <- 5                               ## Number of variables to be pre selected for ART based on importance values
probs_quantiles <- list(c(0.25,0.5,0.75))      ## Use quantiles of split points instead of all split points for continuous variables when creating the ART to save time
epsilon <- c(0.05)                             ## Continue adding more nodes to the ART if the similarity remains the same but the prediction improves by 1 - epsilon


## Number of times each experiment is repeated. You can save time here
# (in publication 100 was used, for time reasons 3 is used here)
repls <- 3

#---------------------------------------
## Create registry 
reg_name <- "simulate_art_original"
reg <- batchtools::makeExperimentRegistry(
  file.dir = file.path(reg_dir, reg_name),
  work.dir = main_dir,
  conf.file = NA, ## If you have a batch system, please enter conf file here,
  packages = c("ranger", "timbR", "rpart", "dplyr") ## Define which packages to use in your simulations
)


#---------------------------------------
# Data Simulation                   

## Add problems ----
## There is a separate function for generating each of the settings from the paper.
## You can save time, excluding settings you are not interested in. 
batchtools::addProblem(name = "simulate_setting_1",
                       reg = reg, 
                       fun = simulate_rf_setting_1,
                       data = n,
                       seed = 12345)
# batchtools::addProblem(name = "simulate_setting_2",
#                        reg = reg,
#                        fun = simulate_rf_setting_2,
#                        data = n,
#                        seed = 12345)
# batchtools::addProblem(name = "simulate_setting_3",
#                        reg = reg,
#                        fun = simulate_rf_setting_3,
#                        data = n,
#                        seed = 12345)
# batchtools::addProblem(name = "simulate_setting_4",
#                        reg = reg,
#                        fun = simulate_rf_setting_4,
#                        data = n,
#                        seed = 12345)
# batchtools::addProblem(name = "simulate_setting_5",
#                        reg = reg,
#                        fun = simulate_rf_setting_5,
#                        data = n,
#                        seed = 12345)

## Add algorithms to solve the problem ----
batchtools::addAlgorithm(reg = reg,
                         name = "most_rep_tree",
                         fun = calculate_most_rep_tree
)
batchtools::addAlgorithm(reg = reg,
                         name = "artificial_rep_tree",
                         fun = calculate_art_rep_tree
)


## define problem and algorithm designs
prob.designs <- list(
  simulate_setting_1 = data.frame(p_eff       = 5, ## Number of true effect variables
                                  beta_eff    = 2, ## Effect size of true effect variables
                                  n_test      = n_test,
                                  n_val       = n_val, 
                                  p           = p, 
                                  num.trees   = num.trees,
                                  eps         = eps,
                                  mtry        = mtry,
                                  min_node_size = min_node_size,
                                  stringsAsFactors = FALSE
  )
  # simulate_setting_2 = data.frame(p_eff       = 50,
  #                                 beta_eff    = 0.2,
  #                                 n_test      = n_test,
  #                                 n_val       = n_val,
  #                                 p           = p,
  #                                 num.trees   = num.trees,
  #                                 eps         = eps,
  #                                 mtry        = mtry,
  #                                 min_node_size = min_node_size,
  #                                 stringsAsFactors = FALSE
  # ),
  # simulate_setting_3 = data.frame(p_eff       = 5,
  #                                 beta_eff    = 2,
  #                                 n_test      = n_test,
  #                                 n_val       = n_val,
  #                                 p           = p,
  #                                 p_corr      = 5,
  #                                 n_blocks    = 5,
  #                                 cor         = 0.3,
  #                                 num.trees   = num.trees,
  #                                 eps         = eps,
  #                                 mtry        = mtry,
  #                                 min_node_size = min_node_size,
  #                                 stringsAsFactors = FALSE
  # ),
  # simulate_setting_4 = data.frame(p_eff       = 5,
  #                                 beta_eff    = 2,
  #                                 n_test      = n_test,
  #                                 n_val       = n_val,
  #                                 p           = p,
  #                                 p_int       = 5,
  #                                 beta_int    = 2,
  #                                 num.trees   = num.trees,
  #                                 eps         = eps,
  #                                 mtry        = mtry,
  #                                 min_node_size = min_node_size,
  #                                 stringsAsFactors = FALSE
  # ),
  # simulate_setting_5 = data.frame(p_eff_bin   = 5,
  #                                 p_eff_con   = 5,
  #                                 beta_eff    = 2,
  #                                 n_test      = n_test,
  #                                 n_val       = n_val,
  #                                 p           = p,
  #                                 num.trees   = num.trees,
  #                                 eps         = eps,
  #                                 mtry        = mtry,
  #                                 min_node_size = min_node_size,
  #                                 stringsAsFactors = FALSE
  # )
)

algo.designs <- list(
  most_rep_tree = data.frame(metric = metric, stringsAsFactors = FALSE),
  artificial_rep_tree = expand.grid(imp.num.var = imp.num.var,
                                    metric = metric, 
                                    stringsAsFactors = FALSE,
                                    probs_quantiles = probs_quantiles,
                                    epsilon = epsilon)
)


## Add experiments ----
ids = batchtools::addExperiments(reg = reg,
                                 prob.designs = prob.designs,
                                 algo.designs = algo.designs,
                                 repls = repls 
)

summarizeExperiments(reg = reg)


## Test jobs before submission
# testJob(id = 1, reg = reg)

## Please change this if you have a batch system. 

submitJobs(ids = ids, reg = reg)

##' With pre selected parameters it will take around 10 min to complete.
##' Please note, the run times for the other settings could differ. 
##' Anyway simulating data for the figures in the paper will probably run for several days on you computer. 

# check job status
getStatus()

## Collect and save results ----
results <- reduceResultsList(reg = reg, missing.val = 0)
## Save results
saveRDS(results, file = file.path(proc_dir, "results.Rds"))



