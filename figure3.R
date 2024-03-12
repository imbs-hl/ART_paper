##' With this script the figure 3 from Laabs et al. "Construction of 
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
pacman::p_load(reshape2)

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
    scenario = gsub("Setting", "Scenario", setting)) %>% 
  select(scenario, method_short, effect_var_fdr, covered_effect_vars, covered_noise_vars)

# transform data from wide to long format
plot_data_long <- melt(plot_data) %>% 
  mutate(variable = factor(variable, 
                           labels = c("Covered effect variables", "Covered noise variables", "FDR"), 
                           levels = c("covered_effect_vars", "covered_noise_vars", "effect_var_fdr")
  ))


#---------------------------------------
# plot data and save plot
ggplot(plot_data_long, 
       aes(x = scenario, 
           y = value,
           col = method_short)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "Data simulation scenarios",
       y = "",
       col = "Method") +
  theme(text = element_text(size = 15), legend.position = "bottom") +
  facet_grid(variable~.) +
  scale_color_manual(values = c(rgb(0,75,90, maxColorValue = 255),
                                rgb(203,81,25, maxColorValue = 255))) 


# save plot
ggsave(file.path(out_dir, "fig3_used_variables_fdr.png"), units = "cm", width = 20, height = 8)





