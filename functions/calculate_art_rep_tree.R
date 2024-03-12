calculate_art_rep_tree <- function(data, instance, metric, imp.num.var, probs_quantiles, epsilon, ...){
 
  ## Exctract data from instance 
  test_dat       <- instance[[1]]
  rf             <- instance[[2]]
  params         <- instance[[3]]
  val_dat        <- instance[[4]]
  effect_var_ids <- instance[[5]]
  noise_var_ids  <- instance[[6]]
  train_dat      <- instance[[7]]
  dependent_varname <- instance[[8]]
  
  library(timbR)
  probs_quantiles <- unlist(probs_quantiles)
  start <- proc.time()
  ## Generate artificial rep tree
  rf_rep <- generate_tree(rf = rf, metric = metric, test_data = test_dat, train_data = train_dat, 
                          importance.mode = TRUE, imp.num.var = imp.num.var, dependent_varname = dependent_varname,
                          probs_quantiles = probs_quantiles, epsilon = epsilon)
  end <- proc.time()
  time <- as.numeric((end - start)[1])
  
  covered_effect_vars <- sum(effect_var_ids %in% treeInfo(rf_rep)$splitvarName)/length(effect_var_ids)
  covered_noise_vars  <- sum(noise_var_ids %in% treeInfo(rf_rep)$splitvarName)/length(noise_var_ids)
  effect_var_fdr      <- 1 - sum(treeInfo(rf_rep)$splitvarName %in% effect_var_ids)/(nrow(treeInfo(rf_rep)) - sum(is.na(treeInfo(rf_rep)$splitvarName)))
  
  ## Prediction accuracy on validation data set
  mse_val_dat_rf <- 1/nrow(val_dat) * sum((val_dat$y - predict(rf, data = val_dat[,-1])$predictions)^2)
  mse_val_dat_rep_tree <- 1/nrow(val_dat) * sum((val_dat$y - predict(rf_rep, data = val_dat[,-1])$predictions)^2)
  
  ## Prediction accuracy on forest prediction
  mse_rf_pred <- 1/nrow(val_dat) * sum((predict(rf, data = val_dat[,-1])$predictions - predict(rf_rep, data = val_dat[,-1])$predictions)^2)
  
  # number of splits
  tree_info <- treeInfo(rf_rep)
  number_splits <- nrow(tree_info %>% filter(!terminal))
  
  return(data.frame(metric               = metric, 
                    method               = "artificial tree",
                    mean_dist            = NA, 
                    sd_dist              = NA, 
                    min_dist             = NA, 
                    max_dist             = NA,
                    setting              = params$setting,
                    min_node_size        = params$min_node_size,
                    covered_effect_vars  = covered_effect_vars,
                    covered_noise_vars   = covered_noise_vars,
                    effect_var_fdr       = effect_var_fdr,
                    mse_val_dat_rf       = mse_val_dat_rf,
                    mse_val_dat_tree     = mse_val_dat_rep_tree,
                    mse_rf_pred          = mse_rf_pred,
                    imp.num.var          = imp.num.var,
                    time                 = time,
                    probs_quantiles      = paste0(probs_quantiles, collapse = ","),
                    number_splits        = number_splits,
                    epsilon              = epsilon
  )
  )
}