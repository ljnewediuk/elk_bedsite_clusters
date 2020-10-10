

####### THIS NEEDS TO BE BLOCKED BY INDIVIDUAL (see Roberts et al. 2017 Box 2) ########


### Load Packages ===
libs <- c('data.table', 'rsample', 'tune', 'caret', 'recipes')
lapply(libs, require, character.only=TRUE)

### Set NN distance ===
nn_dist <- 32

### Load Data ===
model_data <- readRDS(paste('input/model_data', paste(nn_dist, 'm.rds', sep=''), sep=''))

# Factor "correct" for classification
model_data$correct <- factor(model_data$correct)

# Optionally make training set = all data
train_set <- model_data

### Create data_table for covariate sets ===
# Specify covariates
global_covs <- c('n_within_dist', 'beds_in_20_m', 'dist_nearest_pt', 'nn_avg')
# Create table
cov_list <- data.table()
for(h in 1:length(global_covs)){
  cov_combo <- combn(global_covs, h)
  for(l in 1:ncol(cov_combo)){
    model_covs <- data.table(combo=paste(cov_combo[,l], collapse='.'), covs=I(cov_combo[,l]))
    cov_list <- rbind(cov_list, model_covs)
  }
}

### List algorithms to use ===
#     nb = Naive Bayes (Bayesian technique); 
#     rpart = Classification and regression trees (decision tree technique)
#     knn = K nearest neighbors (clustering technique)
#     lda = Linear discriminant analysis (linear regression technique)
#     svmLinear = Linear support vector classifier (linear hyperplane technique)
#     svmRPoly = Polynomial support vector machines (nonlinear hyperplane technique)
#     svmRadial = Radial support vector machines (nonlinear hyperplane technique)
alg_list <- c('nb', 'rpart', 'knn', 'lda', 'svmLinear', 'svmRadial', 'svmPoly')

### Initialize tables ===
for(alg in alg_list){
  assign(paste(alg, 'results', sep='_'), data.table())
}

### Set 'k' folds ===
k <- 10

### Set control parameters for tuning parameter selection ===
tctrl <- trainControl(method='repeatedcv', number=10, repeats=5)

### Load tuning parameters into tuning grids ===
for(alg in alg_list){
  assign(paste(alg, 'tg', sep='_'), readRDS(paste('output/', paste(alg, 'tp.rds', sep='_'), sep='')))
}

### Loop over each unique covariate set ===
for(g in unique(cov_list$combo)){
  
  ### Repeat x5 for each covariate set ===
  # Set initial iteration to zero
  repeats <- 0
  # Define covariate set
  cov_set <- as.character(cov_list[combo==g]$covs)
  
  ### Start repeat loop ===
  repeat{
    # Set next iteration
    repeats <- repeats + 1
    print(repeats)
    
    ### Create 10 random folds ===
    randomDraw <- rnorm(nrow(train_set))
    kQuantiles <- quantile(randomDraw, 0:k/k)
    whichK <- cut(randomDraw, kQuantiles, include.lowest = TRUE)  # Divide randomDraw into kk equally-sized groups
    levels(whichK) <- LETTERS[1:k]
    # Add as column to clust_train
    train_set$folds <- whichK
    
    ### Perform model validation on all folds in turn ===
    for(i in LETTERS[seq(1, k, 1)]){
      # Define testing (letter "i") and training (all else letter "i") sets
      kfold_train <- train_set[!folds==i]
      kfold_test <- train_set[folds==i]
      
      # Down-sample training data ===
      # Count correctly and incorrectly identified samples in training set
      kfold_train[, .N, by='correct']
      # Perform down-sampling
      dsample_recipe <- recipe(correct~., data=kfold_train) %>% step_downsample(correct)
      # Prep
      dsample_prep <- prep(dsample_recipe)
      # Juice (return pre-processed data)
      dsample_data <- as.data.table(juice(dsample_prep))
      # Check that correct/incorrect are now equivalent
      dsample_data[, .N, by='correct']
      
      ### Organize data ===
      # Subset only relevant columns for modeling
      kfold_train <- data.table(correct=dsample_data$correct, beds_in_20_m=dsample_data$beds_in_20_m, n_within_dist=dsample_data$n_within_dist, dist_nearest_pt=dsample_data$dist_nearest_pt, nn_avg=dsample_data$nn_avg)
      
      ### Select models by adjusting tuning parameters ===
      for(alg in alg_list){
        # Define algorithm specific tuning parameters
        alg_tg <- get(paste(alg, 'tg', sep='_'))
        # If algorithm has tuning parameters, run models with tuning grid, otherwise without (i.e. LDA)
        if(alg=='knn' | alg=='nb' | alg=='svmLinear' | alg=='svmPoly' | alg=='svmRadial' | alg=='rpart'){
          assign(paste(alg, 'train', sep='_'), caret::train(reformulate(c(cov_set), response='correct'), tuneGrid=alg_tg, trControl=tctrl, data=kfold_train, method=alg))
        } else {
          assign(paste(alg, 'train', sep='_'), caret::train(reformulate(c(cov_set), response='correct'), trControl=tctrl, data=kfold_train, method=alg))
        }
      }
      
      ### Predict using model on withheld testing data ===
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
        mod_output <- data.table(measure=c(mod_accuracy$V1, mod_specificity$V1, mod_sensitivity$V1), value=c(mod_accuracy$V2, mod_specificity$V2, mod_sensitivity$V2), iteration=paste(i, repeats, sep='_'), set=g)
        # Move on to next if any of the models didn't work, or assign to data.table
        # if(any(is.nan(mod_output$value))){
        #   next
        # } else {
          assign(paste(alg, 'output', sep='_'), mod_output)
        # }
      }
      
      ### Compile results from iteration ===
      nb_results <- rbind(nb_results, nb_output)
      rpart_results <- rbind(rpart_results, rpart_output)
      knn_results <- rbind(knn_results, knn_output)
      lda_results <- rbind(lda_results, lda_output)
      svmRadial_results <- rbind(svmRadial_results, svmRadial_output)
      
    }
    
    # Break loop once it gets to 5 repeats
    if(repeats==5){
      break
    }
    
  }
}

### Save outputs ===
for(alg in alg_list){
  saveRDS(get(paste(alg, 'results', sep='_')), paste('results/', paste(nn_dist, paste('m_', paste(alg, 'results.rds', sep='_'), sep=''), sep=''), sep=''))
}



