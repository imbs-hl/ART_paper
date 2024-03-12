calculate_most_rep_tree <- function(data, instance, metric, ...){

  ## Exctract data from instance
  test_dat       <- instance[[1]]
  rf             <- instance[[2]]
  params         <- instance[[3]]
  val_dat        <- instance[[4]]
  effect_var_ids <- instance[[5]]
  noise_var_ids  <- instance[[6]]
  train_dat      <- instance[[7]]

  ## Calculate distances
  start <- proc.time()
  d  <- measure_distances(rf = rf, metric = metric, test_data = test_dat)
  end <- proc.time()
  time <- as.numeric((end - start)[1])

  ## Distance score for each tree
  d_score <- rowSums(d)

  ## Select most representative tree
  rf_red <- select_trees(rf, num.trees = as.numeric(1), distance.matrix = d)

  covered_effect_vars <- sum(effect_var_ids %in% treeInfo(rf_red)$splitvarName)/length(effect_var_ids)
  covered_noise_vars  <- sum(noise_var_ids %in% treeInfo(rf_red)$splitvarName)/length(noise_var_ids)
  effect_var_fdr      <- 1 - sum(treeInfo(rf_red)$splitvarName %in% effect_var_ids)/(nrow(treeInfo(rf_red)) - sum(is.na(treeInfo(rf_red)$splitvarName)))

  ## Prediction accuracy on validation data set
  mse_val_dat_rf <- 1/nrow(val_dat) * sum((val_dat$y - predict(rf, data = val_dat[,-1])$predictions)^2)
  mse_val_dat_rep_tree <- 1/nrow(val_dat) * sum((val_dat$y - predict(rf_red, data = val_dat[,-1])$predictions)^2)

  ## Prediction accuracy on forest prediction
  mse_rf_pred <- 1/nrow(val_dat) * sum((predict(rf, data = val_dat[,-1])$predictions - predict(rf_red, data = val_dat[,-1])$predictions)^2)

  # number of splits
  tree_info <- treeInfo(rf_red)
  number_splits <- nrow(tree_info %>% filter(!terminal))
  
  return(data.frame(metric               = metric,
                    method               = "selected tree",
                    mean_dist            = mean(d_score),
                    sd_dist              = sd(d_score),
                    min_dist             = min(d_score),
                    max_dist             = max(d_score),
                    setting              = params$setting,
                    min_node_size        = params$min_node_size,
                    covered_effect_vars  = covered_effect_vars,
                    covered_noise_vars   = covered_noise_vars,
                    effect_var_fdr       = effect_var_fdr,
                    time                 = time,
                    mse_val_dat_rf       = mse_val_dat_rf,
                    mse_val_dat_tree = mse_val_dat_rep_tree,
                    mse_rf_pred          = mse_rf_pred,
                    probs_quantiles = "",
                    number_splits        = number_splits
                    )
         )
}
