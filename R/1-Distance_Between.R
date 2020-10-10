
### *** NOTE: You must load the "Cluster Function" function in functions/ before
### this script will work properly ***

### Load Packages ===
require(gdistance)
require(data.table)
require(Imap)
require(caret)
require(geosphere)

### Load Data ===
dat <- fread('input/loc_dat.csv')
clust_info <- fread('input/clust_dat.csv')

### Number of points within buffer of size j ===
dat_sens <- data.table()

for(j in seq(1,100,1)){
  
  clust_dat <- data.table()
  
  for(i in unique(clust_info$uid)){
    dat_sub <- dat[uid==i]
    # dat_sub <- dat_sub[3:7,] # Optionally subset clusters to 2.5 hours time period
    clust_id <- as.data.table(cluster_distance(dat_sub,j))
    clust_row <- data.table(uid=i, n_within_dist=nrow(clust_id))
    clust_dat <- rbind(clust_dat, clust_row)
  }
  
  clust_test <- merge(clust_dat, clust_info, by='uid')
  
  no_mean <- mean(clust_test[correct==0]$n_within_dist)
  yes_mean <- mean(clust_test[correct==1]$n_within_dist)
  
  no_se <- sd(clust_test[correct==0]$n_within_dist)/sqrt(length(clust_test[correct==0]$n_within_dist))
  yes_se <- sd(clust_test[correct==1]$n_within_dist)/sqrt(length(clust_test[correct==1]$n_within_dist))
  
  sens_row <- data.table(distance=j, yes_mean, no_mean, yes_se, no_se)
  dat_sens <- rbind(dat_sens, sens_row)
  
}

### Average nearest neighbour distance between points in cluster ===
nn_dists <- data.table()
for(clust in unique(clust_info$uid)){
  # Subset out each uid
  dat_sub <- dat[uid==clust]
  sub_dist <- c()
  for(i in 1:nrow(dat_sub)){
    # Subset out all non-focal points
    not_pt <- dat_sub[!i,]
    pt_vec <- c()
    for(j in 1:nrow(not_pt)){
      # Compare distance of each non-focal point to the focal point
      pt_dist <- gdist(lat.1=dat_sub[i,]$lat, lon.1=dat_sub[i,]$lon, lat.2=not_pt[j,]$lat, lon.2=not_pt[j,]$lon, units="m")
      pt_vec <- c(pt_vec, pt_dist)
    }
    # Make vector of all nn dists between all points
    sub_dist <- c(sub_dist, pt_vec)
  }
  # Combine into data table
  avg_dist <- data.table(uid=clust, nn_avg=mean(sub_dist))
  nn_dists <- rbind(nn_dists, avg_dist)
}




### Save sensitivity, cluster data, and nn data ===
saveRDS(dat_sens, 'output/dat_sens.rds')
saveRDS(clust_test, 'output/clust_test.rds')
saveRDS(nn_dists, 'input/avg_nn_dat.rds')




