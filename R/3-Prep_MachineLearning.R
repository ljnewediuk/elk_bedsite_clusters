
### Load Packages ===
require(gdistance)
require(data.table)
require(Imap)
require(caret)
require(geosphere)

### Load Data ===
dat <- fread('input/loc_dat.csv')
clust_info <- fread('input/clust_dat.csv')
nn_dat <- readRDS('input/avg_nn_dat.rds')

### Set m distance within which to calculate NN ===
nn_dist <- 32

### Calculate NN distance within X m ===
# For each point in the cluster, calculate the number of 
# other points that fall within 25 m of that focal point

clust_dat <- data.table()
for(i in unique(clust_info$uid)){
  dat_sub <- dat[uid==i]
  clust_id <- as.data.table(cluster_distance(dat_sub,nn_dist))
  clust_row <- data.table(uid=i, n_within_dist=nrow(clust_id))
  clust_dat <- rbind(clust_dat, clust_row)
}

model_data <- merge(clust_dat, clust_info, by='uid')
model_data <- merge(model_data, nn_dat, by='uid')

### Save model data ===
saveRDS(model_data, paste('input/model_data', paste(nn_dist, 'm.rds', sep=''), sep=''))

