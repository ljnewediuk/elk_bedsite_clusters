#######################################################
#### 4 - Define a range of possible tuning         ####
####     parameters for each classifier to try     ####
####     in cross validations                      ####
#######################################################

### Load Packages ====
require('data.table')

### Set NN distance ====
nn_dist <- 32

### Load Data ====
dsample_data <- readRDS(paste('input/model_data', 
                              paste(nn_dist, 'm.rds', sep=''), sep=''))

### Define tuning parameters ====
# K nearest-neighbour: try k from 1:n_per_class-2
knn_tp <- data.frame(.k = seq(1,nrow(dsample_data[correct==0])-2,1))
# Naive Bayes: try with and without kernel estimator
nb_tp <- data.frame(.usekernel = c(TRUE, FALSE), .fL=0, .adjust=1)
# Linear svm: try small C values up to C = all observations in data
svmLinear_tp <- data.frame(.C = c(seq(0.1,2,0.1), 10, nrow(dsample_data)))
# Polynomial svm: up to three degrees, 
#                 try small C values up to C = all observations in data, 
#                 scale fixed at 0.1
svmPoly_tp <- data.frame(.degree = c(1,2,3), .C = c(seq(0.1,2,0.1), 
                                                nrow(dsample_data)), .scale=0.1)
# Radial svm: try small C values up to C = all observations in data, 
#             sigma fixed at 1.438348
svmRadial_tp <- data.frame(.sigma = 1.438348, .C = c(seq(0.1,2,0.1), 
                                                     nrow(dsample_data)))
# rpart: try complexity parameters up to 0.3
rpart_tp <- data.frame(.cp=seq(0,0.3,0.01))
# rf: set a null data frame (tuning parameters determined by N predictors)
rf_tp <- data.frame(.mtry=NA)
# lda: set a null data frame (no tuning parameters defined)
lda_tp <- data.frame(.x=NA)

### Save data frames ===
# List of algorithms
alg_list <- c('knn', 'nb', 'svmLinear', 
              'svmPoly', 'svmRadial', 'rpart', 'rf', 'lda')
# Save
for(alg in alg_list){
  saveRDS(get(paste(alg, 'tp', sep='_')), paste('output/', 
                                        paste(alg, 'tp.rds', sep='_'), sep=''))
}



