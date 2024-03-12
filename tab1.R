##' With this script the table 1 from Laabs et al. "Construction of 
##' artificial most representative trees by minimizing tree-based 
##' distance measures" can be reproduced. Given a simulated data set. 
##' Run simulations.R to get such a data set. 
##' With the standard parameters in simulations.R, only the part for 
##' data scenario 1 is reproduced, as the runtime without a computing cluster 
##' would otherwise be too high.

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

pacman::p_load(ranger)
pacman::p_load(devtools)
pacman::p_load(rpart)
pacman::p_load(dplyr)
pacman::p_load(reshape2)
pacman::p_load(xtable)
pacman::p_load(tidyr)

#---------------------------------------
## Load and prepare data 
# data from publication
results <- read.csv2(file.path(proc_dir, "simulation_shiny_data.csv"))

# data produced by simulations.R
results <- readRDS(file.path(proc_dir, "results.Rds"))

## Transform results from list to data.frame
plot_data <- bind_rows(results) %>% 
  mutate(
    # seconds to minutes
    time = time/60,
    # add abbreviations and change order
    method_short = case_when(method == "selected tree" ~ "MRT", method == "artificial tree" ~ "ART"),
    method_short = factor(method_short, levels = c("MRT", "ART"), labels = c("MRT", "ART")),
    # change name
    scenario = gsub("Setting", "Scenario", setting)) 


#---------------------------------------
# table data and save table
table1 <- plot_data %>% 
  group_by(method_short, scenario) %>% 
  summarise_at(., 'time', mean) %>% 
  pivot_wider(., names_from = scenario, values_from = time) 

table1

# save table
write.csv2(table1, file.path(out_dir, "table1_runtime.csv"), row.names = FALSE)




