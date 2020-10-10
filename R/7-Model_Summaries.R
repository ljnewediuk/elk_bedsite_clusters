
### Load packages ===
libs <- c('data.table')
lapply(libs, require, character.only=TRUE)

### Load model results ===
### Set NN distance ===
nn_dist <- 32
# Define algorithms
alg_list <- c('rpart', 'knn', 'lda', 'nb', 'svmRadial')
# Load k-fold models
for(alg in alg_list){
  assign(paste(alg, 'results', sep='_'), readRDS(paste('results/', paste(nn_dist, paste('m_', paste(alg, 'results.rds', sep='_'), sep=''), sep=''), sep='')))
}
# Optionally load individually blocked models
for(alg in alg_list){
  assign(paste(alg, 'results', sep='_'), readRDS(paste('results/', paste(nn_dist, paste('m_', paste(alg, 'results_blocked.rds', sep='_'), sep=''), sep=''), sep='')))
}

### Compile accuracy ===
# Initiate data table
alg_accuracy <- data.table()
alg_specificity <- data.table()
alg_sensitivity <- data.table()
# Loop through
for(metric in c('Accuracy', 'Specificity', 'Sensitivity')){
  
  alg_metric <- data.table()

  for(alg in alg_list){
    alg_results <- get(paste(alg, 'results', sep='_'))
    # Get mean accuracy for each 
    for(cov_set in unique(alg_results$set)){
      # Subset out each covariate set
      cov_results <- alg_results[set==cov_set]
      # Calculate mean accuracy
      mean_metric <- mean(cov_results[measure==metric]$value, na.rm=T)
      # Calculate confidence intervals
      lower_95 <- mean_metric-(sd(cov_results[measure==metric]$value, na.rm=T)/sqrt(length(cov_results[measure==metric]$value)))*1.96
      upper_95 <- mean_metric+(sd(cov_results[measure==metric]$value, na.rm=T)/sqrt(length(cov_results[measure==metric]$value)))*1.96
      metric_row <- data.table(algorithm=alg, set=cov_set, metric=mean_metric, lower=lower_95, upper=upper_95)
      # Compile in data.table
      alg_metric <- rbind(alg_metric, metric_row)
    }
  }

  assign(paste('alg', metric, sep='_'), alg_metric)
  
}


