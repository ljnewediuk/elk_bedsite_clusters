######################################################
#########       F1: CLUSTER FUNCTION          ########
######################################################
## 
## Returns the number of other points ('n_within_d')
## within a given distance of each point (row) within
## a cluster (df). 
## The following columns are required:
##      - 'lon': Longitude
##      - 'lat': Latitude
##      - 'name': ID column for each row
## The following needs to be specified:
##      - 'df': The data frame
##      - 'dist_btwn': The distance from the focal
##         point within which to count points

### Load Packages ===
library(gdistance)
library(data.table)
library(Imap)

### Function ===
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

