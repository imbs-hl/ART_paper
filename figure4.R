##' With this script the figure 4 from Laabs et al. "Construction of 
##' artificial most representative trees by minimizing tree-based 
##' distance measures" can be reproduced. 

#---------------------------------------
## Define directories
## Please define your main directory here. 
## This should be the directory you cloned the git repository into.
main_dir <- getwd()
setwd(main_dir)

## Create and define proc directory
dir.create(file.path(main_dir, "proc"), showWarnings = FALSE)
proc_dir <- file.path(main_dir, "proc")
## Create and define output directory
dir.create(file.path(main_dir, "output"), showWarnings = FALSE)
out_dir <- file.path(main_dir, "output")

#---------------------------------------
## Load libraries
if (!"pacman" %in% installed.packages()){
  install.packages("pacman")
}

pacman::p_load(ggplot2)
pacman::p_load(gridExtra)
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

#---------------------------------------
## Simulate data 

## functions to simulate the data sets
source("functions/simulate_rf_setting_1.R")

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
imp.num.var <- 5                             ## Number of variables to be pre selected for ART based on importance values
probs_quantiles <- list(c(0.25,0.5,0.75))      ## Use quantiles of split points instead of all split points for continuous variables when creating the ART to save time
epsilon <- c(0.05)                             ## Continue adding more nodes to the ART if the similarity remains the same but the prediction improves by 1 - epsilon


color_node <- "oceangreen_uzl!40"
color_node_ART <- "imbs_orange!40"
color_others <- "white"

set.seed(123)
# generate data
data_scenario1 <- simulate_rf_setting_1(data = n, 
                                        n_test = n_test, 
                                        n_val = n_val, 
                                        p = p,
                                        p_eff = 5, 
                                        beta_eff = 2, 
                                        eps = eps, 
                                        num.trees = num.trees,
                                        mtry = mtry, 
                                        min_node_size = min_node_size, 
                                        keep.inbag = TRUE)


## Exctract data from instance
test_dat       <- data_scenario1[[1]]
rf             <- data_scenario1[[2]]
params         <- data_scenario1[[3]]
val_dat        <- data_scenario1[[4]]
effect_var_ids <- data_scenario1[[5]]
noise_var_ids  <- data_scenario1[[6]]
train_dat      <- data_scenario1[[7]]

#---------------------------------------
## build MRT 
## the following code to get rf_red is extracted from the select_trees() 
## function from the timbR package 
set.seed(123)

## Calculate distances
start <- proc.time()
d  <- measure_distances(rf = rf, metric = metric, test_data = test_dat)
end <- proc.time()
time <- as.numeric((end - start)[1])

## Distance score for each tree
d_score <- rowSums(d)

## Select most representative tree
rf_red <- select_trees(rf, num.trees = as.numeric(1), distance.matrix = d)

# display treeInfo, yound prediction and plot tree
MRT_scenario1_treeInfo <- treeInfo(rf_red) %>% 
  mutate(prediction = round(prediction, 2),
         colors = ifelse(splitvarName %in% effect_var_ids, color_node, color_others))

colors_MRT_scenario1 <- MRT_scenario1_treeInfo$colors

MRT_scenario1_treeInfo$colors <- NULL

#---------------------------------------
## plot MRT
plot_tree(tree_info_df = MRT_scenario1_treeInfo, 
          train_data_df = train_dat, 
          rf_list = rf_red, 
          tree_number = 1,
          dependent_var = "y", 
          work_dir = out_dir, 
          plot_name = "MRT_scenario1", 
          hor_sep = 12, vert_sep = 8,
          show_sample_size = FALSE, 
          colors = colors_MRT_scenario1)

#---------------------------------------
## build ART 
set.seed(123)

# this step takes about 2 to 5 minutes 
rf_rep <- generate_tree(rf = rf, metric = metric, test_data = test_dat, train_data = train_dat, 
                        importance.mode = TRUE, imp.num.var = imp.num.var, dependent_varname = "y",
                        probs_quantiles = NULL, epsilon = epsilon)

# set colors
ART_scenario1_treeInfo <- treeInfo(rf_rep) %>% 
  mutate(prediction = round(prediction, 2),
         colors = ifelse(splitvarName %in% effect_var_ids, color_node_ART, color_others))

colors_ART_scenario1 <- ART_scenario1_treeInfo$colors

ART_scenario1_treeInfo$colors <- NULL

## plot ART
plot_tree(tree_info_df = ART_scenario1_treeInfo, 
          train_data_df = train_dat, 
          rf_list = rf_rep, 
          tree_number = 1,
          dependent_var = "y", 
          work_dir = out_dir, 
          plot_name = "ART_scenario1", 
          hor_sep = 12, vert_sep = 8,
          show_sample_size = FALSE, 
          colors = colors_ART_scenario1)








