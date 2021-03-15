#######################################################
#### 3 - Prepare the n_within_dist predictor       ####
####     for machine learning models by            ####
####     specifying a buffer distance and          ####
####     counting points within each               ####
#######################################################

### *** WARNING: Sourcing the "Cluster Function" function loads the packages
###     "Imap", "gdistance" and "geosphere" ***

### Load Packages ====
require(data.table)

### Source cluster function ====
source('functions/Cluster_Function.R')

### Load Data ===
dat <- fread('input/loc_dat.csv')
clust_info <- fread('input/clust_dat.csv')
nn_dat <- readRDS('input/avg_nn_dat.rds')

### Source cluster function ====
source('functions/Cluster_Function.R')

### Set m distance within which to calculate NN ====
nn_dist <- 32

### Optionally rarefy data to 1 hour frequency ====
# rarefy <- 'yes'
rarefy <- 'no'

### Calculate NN distance within X m (specified by nn_dist) ====
# For each point in the cluster, calculate the number of 
# other points that fall within 25 m of that focal point

clust_dat <- data.table()
for(i in unique(clust_info$uid)){
  dat_sub <- dat[uid==i]
  # Rarefy to 1 hr relocations if rarify=T
  if(rarefy=='yes'){
    dat_sub <- dat_sub[c(1,3,5,7,9),]
  }
  clust_id <- as.data.table(cluster_distance(dat_sub,nn_dist))
  clust_row <- data.table(uid=i, n_within_dist=nrow(clust_id))
  clust_dat <- rbind(clust_dat, clust_row)
}

model_data <- merge(clust_dat, clust_info, by='uid')
model_data <- merge(model_data, nn_dat, by='uid')

### Save model data ===
if(rarefy=='no'){
  saveRDS(model_data, paste('input/model_data', 
                            paste(nn_dist, 'm.rds', sep=''), sep=''))
} else  {
  saveRDS(model_data, paste('input/model_data', 
          paste(nn_dist, paste('m', 'rarefied.rds', sep='_'), sep=''), sep=''))
}

