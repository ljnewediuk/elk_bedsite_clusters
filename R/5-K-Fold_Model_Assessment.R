#######################################################
#### 5 - Run machine learning models with          ####
####     10-fold cross-validation using all        ####
####     combinations of predictors and full       ####
####     range of tuning parameters, then save     ####
####     outputs by classifier                     ####
#######################################################

### *** WARNING: This script takes several hours to run ***

### Load Packages ====
libs <- c('data.table', 'rsample', 'tune', 'caret', 'recipes')
lapply(libs, require, character.only=TRUE)

### Set NN distance ====
nn_dist <- 32

### Optionally rarefy data to 1 hour relocation frequency ====
# rarefy <- 'yes'
rarefy <- 'no'

### Load Data ====
if(rarefy=='no'){
  model_data <- readRDS(paste('input/model_data', 
                              paste(nn_dist, 'm.rds', sep=''), sep=''))
} else {
  model_data <- readRDS(paste('input/model_data', 
          paste(nn_dist, paste('m', 'rarefied.rds', sep='_'), sep=''), sep=''))
}

# Factor "correct" for classification
model_data$correct <- factor(model_data$correct)

# Optionally make training set = all data (if testing on independent data)
train_set <- model_data

###  Optionally subset data to only hair or pellets ====
# Hair only
# model_data <- model_data[sample_type=='hair']
# Pellets only
# model_data <- model_data[sample_type=='pellets']

### Create data_table for covariate sets ====
# Specify covariates
global_covs <- c('n_within_dist', 'beds_in_20_m', 'dist_nearest_pt', 'nn_avg')
# Create table
cov_list <- data.table()
for(h in 1:length(global_covs)){
  cov_combo <- combn(global_covs, h)
  for(l in 1:ncol(cov_combo)){
    model_covs <- data.table(combo=paste(cov_combo[,l], collapse='.'), 
                             covs=I(cov_combo[,l]))
    cov_list <- rbind(cov_list, model_covs)
  }
}

### List algorithms to use ====
#     nb = Naive Bayes (Bayesian technique); 
#     rpart = Classification and regression trees (decision tree technique)
#     knn = K nearest neighbors (clustering technique)
#     lda = Linear discriminant analysis (linear regression technique)
#     rf = Random forests (decision tree technique)
#     svmLinear = Linear support vector classifier (linear hyperplane technique)
#     svmRPoly = Polynomial support vector machines (nonlinear hyperplane technique)
#     svmRadial = Radial support vector machines (nonlinear hyperplane technique)
alg_list <- c('nb', 'rpart', 'knn', 'lda', 'rf', 'svmLinear', 'svmRadial', 'svmPoly')

### Initialize tables ====
for(alg in alg_list){
  assign(paste(alg, 'results', sep='_'), data.table())
}

### Set 'k' folds ====
k <- 10

### Set control parameters for tuning parameter selection ====
tctrl <- trainControl(method='repeatedcv', number=10, repeats=5)

### Load tuning parameters into tuning grids ====
for(alg in alg_list){
  assign(paste(alg, 'tg', sep='_'), readRDS(paste('output/', 
                                        paste(alg, 'tp.rds', sep='_'), sep='')))
}

### Loop over each unique covariate set ====
for(g in unique(cov_list$combo)){
  
  ### Repeat x5 for each covariate set ====
  # Set initial iteration to zero
  repeats <- 0
  # Define covariate set
  cov_set <- as.character(cov_list[combo==g]$covs)
  
  ### Start repeat loop ====
  repeat{
    # Set next iteration
    repeats <- repeats + 1
    print(repeats)
    
    ### Create 10 random folds ====
    randomDraw <- rnorm(nrow(train_set))
    kQuantiles <- quantile(randomDraw, 0:k/k)
    whichK <- cut(randomDraw, kQuantiles, include.lowest = TRUE)
    levels(whichK) <- LETTERS[1:k]
    # Add as column to clust_train
    train_set$folds <- whichK
    
    ### Perform model validation on all folds in turn ====
    for(i in LETTERS[seq(1, k, 1)]){
      # Define testing (letter "i") and training (all else letter "i") sets
      kfold_train <- train_set[!folds==i]
      kfold_test <- train_set[folds==i]
      
      # Down-sample training data ====
      # Count correctly and incorrectly identified samples in training set
      kfold_train[, .N, by='correct']
      # Perform down-sampling
      dsample_recipe <- recipe(correct~., data=kfold_train) %>% 
        step_downsample(correct)
      # Prep
      dsample_prep <- prep(dsample_recipe)
      # Juice (return pre-processed data)
      dsample_data <- as.data.table(juice(dsample_prep))
      # Check that correct/incorrect number of points are now equal
      dsample_data[, .N, by='correct']
      
      ### Organize data ====
      # Subset only relevant columns for modeling
      kfold_train <- data.table(correct=dsample_data$correct, 
               beds_in_20_m=dsample_data$beds_in_20_m, 
               n_within_dist=dsample_data$n_within_dist, 
               dist_nearest_pt=dsample_data$dist_nearest_pt, 
               nn_avg=dsample_data$nn_avg)
      
      ### Select models by adjusting tuning parameters ====
      for(alg in alg_list){
        # Define algorithm specific tuning parameters
        alg_tg <- get(paste(alg, 'tg', sep='_'))
        # If algorithm has tuning parameters, run models with tuning grid,
        # otherwise without (i.e. LDA), 
        # or define parameter subset based on N parameters (i.e. random forest)
        if(alg=='knn' | alg=='nb' | alg=='svmLinear' | alg=='svmPoly' | 
           alg=='svmRadial' | alg=='rpart'){
          assign(paste(alg, 'train', sep='_'), caret::train(reformulate(c(cov_set), 
                   response='correct'), tuneGrid=alg_tg, trControl=tctrl, 
                   data=kfold_train, method=alg))
        }
        # Run model without tuning params for LDA
        if(alg=='lda'){
          assign(paste(alg, 'train', sep='_'), caret::train(reformulate(c(cov_set), 
                 response='correct'), trControl=tctrl, data=kfold_train, method=alg))
        }
        # Run model with tuning parameters based on number of covariates in set
        if(alg=='rf'){
          alg_tg <- data.frame(.mtry=seq(1, length(cov_set), 1))
          assign(paste(alg, 'train', sep='_'), caret::train(reformulate(c(cov_set), 
                response='correct'), trControl=tctrl, data=kfold_train, method=alg))
        }  
      }
      
      ### Predict using model on withheld testing data ====
      for(alg in alg_list){
        # Get training results
        train_results <- get(paste(alg, 'train', sep='_'))
        # Make prediction
        mod_predictions <- predict(train_results, kfold_test)
        # Confusion matrix showing how many lines were predicted in each category
        con_mat <- confusionMatrix(mod_predictions, kfold_test$correct)
        # Compile results
        mod_accuracy <- as.data.table(con_mat$overall[1], keep.rownames = TRUE)
        mod_specificity <- as.data.table(con_mat$byClass[2], keep.rownames = TRUE)
        mod_sensitivity <- as.data.table(con_mat$byClass[1], keep.rownames = TRUE)
        mod_output <- data.table(measure=c(mod_accuracy$V1, mod_specificity$V1, 
                            mod_sensitivity$V1), value=c(mod_accuracy$V2, 
                            mod_specificity$V2, mod_sensitivity$V2), 
                            iteration=paste(i, repeats, sep='_'), set=g)
        # Move on to next if any of the models didn't work, or assign to data.table
        # if(any(is.nan(mod_output$value))){
        #   next
        # } else {
          assign(paste(alg, 'output', sep='_'), mod_output)
        # }
      }
      
      ### Compile results from iteration ====
      nb_results <- rbind(nb_results, nb_output)
      rpart_results <- rbind(rpart_results, rpart_output)
      knn_results <- rbind(knn_results, knn_output)
      lda_results <- rbind(lda_results, lda_output)
      svmRadial_results <- rbind(svmRadial_results, svmRadial_output)
      rf_results <- rbind(rf_results, rf_output)
      
    }
    
    # Break loop once it gets to 5 repeats
    if(repeats==5){
      break
    }
    
  }
}

### Save outputs ====
for(alg in alg_list){
  if(rarefy=='no'){
    if('hair' %in% model_data$sample_type & 'pellets' %in% model_data$sample_type){
      saveRDS(get(paste(alg, 'results', sep='_')), 
              paste('results/model_outputs/', paste(nn_dist, paste('m_', 
              paste(alg, 'results.rds', sep='_'), sep=''), sep=''), sep=''))
    } else {
      saveRDS(get(paste(alg, 'results', sep='_')),
              paste('results/model_outputs/', paste(nn_dist, paste('m_', 
              paste(alg, paste('results', paste(unique(model_data$sample_type), 
              'only.rds', sep='_'), sep='_'), sep='_'), sep=''), sep=''), sep=''))
    }
  }  else {
    if('hair' %in% model_data$sample_type & 'pellets' %in% model_data$sample_type){
      saveRDS(get(paste(alg, 'results', sep='_')), 
              paste('results/model_outputs/', paste(nn_dist, paste('m_', 
              paste(alg, 'results_rarefied.rds', sep='_'), sep=''), sep=''), sep=''))
    } else {
      saveRDS(get(paste(alg, 'results', sep='_')), 
              paste('results/model_outputs/', paste(nn_dist, paste('m_', 
              paste(alg, paste('results_rarefied', paste(unique(model_data$sample_type), 
              'only.rds', sep='_'), sep='_'), sep='_'), sep=''), sep=''), sep=''))
    }
  }
}



