
### Load Packages ===
require(gdistance)
require(data.table)
require(Imap)
require(caret)

### Load Data ===
dat <- fread('input/test_dat.csv')


cluster_distance <- function(df, dist_btwn){
  
  pt_matrix <- data.table()
  
  for(i in 1:nrow(df)){
    not_pt <- df[!i,]
    pt_row <- data.table(name=df[i,]$name)
    for(j in 1:nrow(not_pt)){
      pt_dist <- gdist(lat.1=df[i,]$lat, lon.1=df[i,]$lon, lat.2=not_pt[j,]$lat, lon.2=not_pt[j,]$lon, units="m")
      pt_row <- cbind(pt_row, pt_dist)
    }
    pt_name <- as.character(lapply(seq(1,(ncol(pt_row)-1),1), function(x) paste(x, 'pt', sep='_')))
    colnames(pt_row)[2:ncol(pt_row)] <- pt_name
    pt_matrix <- rbind(pt_matrix, pt_row)
  }
  
  clust_matrix <- data.table()
  for(k in 1:nrow(pt_matrix)){
    clust_size <- length(which(pt_matrix[k,]<dist_btwn))
    if(clust_size>0){
      clust_row <- data.table(pt=k, n_within_d=clust_size)
      clust_matrix <- rbind(clust_matrix, clust_row)
    }
  }
  
  return(clust_matrix)
  
}

# - Clusters should have point at central date/time of suspected cluster,
#   and some number of points before and after, based on the 95% CI for 
#   time range of all clusters
# - For each cluster, calculate the avg. number of points within 
#   seq(5, 30, 1) m of cluster
# - Plot distributions of correct/incorrect clusters to visualize difference
# - Do t-tests to figure out at which distance the correct/incorrect clusters
#   have a significantly different number of points within
# - Plot the smoothed GAM for mean(difference)~distance

# - For actual predictions, go through rolling window of n rows at a time
#   and calculate the N within distance

frollapply(clust_matrix$n_within_d, 3, mean, align='center')



