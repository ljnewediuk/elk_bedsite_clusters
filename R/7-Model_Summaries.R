#######################################################
#### 7 - Summarize performance metrics for all     ####
####     models and classifiers (accuracy,         ####
####     sensitivity, specificity)                 #### 
#######################################################

### Load packages ====
library('data.table')

### Load model results ====
# Set NN distance
nn_dist <- 32

# Define algorithms
alg_list <- c('nb', 'rpart', 'knn', 'lda', 'svmRadial')

# Load k-fold models
for(alg in alg_list){
  assign(paste(alg, 'results', sep='_'), 
         readRDS(paste('results/model_outputs/', paste(nn_dist, paste('m_', 
         paste(alg, 'results.rds', sep='_'), sep=''), sep=''), sep='')))
}

# Optionally load individually blocked, rarefied, or hair/pellet only models
mod_type <- 'blocked'
mod_type <- 'rarefied'
mod_type <- 'hair_only'
mod_type <- 'pellets_only'
for(alg in alg_list){
  assign(paste(alg, 'results', sep='_'),
      readRDS(paste('results/model_outputs/', paste(nn_dist, paste('m_',
      paste(alg, paste0('results_', mod_type, '.rds', sep=''), sep='_'), sep=''), 
      sep=''), sep='')))
}

### Compile accuracy ====
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
      lower_95 <- mean_metric-(sd(cov_results[measure==metric]$value, na.rm=T)
                        /sqrt(length(cov_results[measure==metric]$value)))*1.96
      upper_95 <- mean_metric+(sd(cov_results[measure==metric]$value, na.rm=T)
                        /sqrt(length(cov_results[measure==metric]$value)))*1.96
      metric_row <- data.table(algorithm=alg, set=cov_set, metric=mean_metric, 
                        lower=lower_95, upper=upper_95)
      # Compile in data.table
      alg_metric <- rbind(alg_metric, metric_row)
    }
  }

  # Sort by metric and return output
  alg_metric <- alg_metric[order(-metric)]
  assign(paste('alg', metric, sep='_'), alg_metric)
  
}



### Save output as csv ====
if(exists('mod_type')){
  fwrite(alg_Accuracy, paste('results/model_metrics/', mod_type, '_accuracy.csv', sep=''))
  fwrite(alg_Sensitivity, paste('results/model_metrics/', mod_type, '_sensitivity.csv', sep=''))
  fwrite(alg_Specificity, paste('results/model_metrics/', mod_type, '_specificity.csv', sep=''))
} else {
  fwrite(alg_Accuracy, paste('results/model_metrics/results_accuracy.csv', sep=''))
  fwrite(alg_Sensitivity, paste('results/model_metrics/results_sensitivity.csv', sep=''))
  fwrite(alg_Specificity, paste('results/model_metrics/results_specificity.csv', sep=''))
}


