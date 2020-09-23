
### Load Packages ===
require(data.table)
require(geosphere)

### Load Data ===
dat <- fread('input/loc_dat.csv')
clust_test <- readRDS('output/clust_test.rds')

angle_dat <- data.table()

for(i in unique(dat$uid)){
  
  dat_sub <- dat[uid==i]
  
  sub_angle <- data.table()
  for(j in 1:nrow(dat_sub)){
    if(j == 1){
      angle_btwn <- NA
      }else{
      angle_btwn <- bearing(c(dat_sub$lon[j-1], dat_sub$lat[j-1]), c(dat_sub$lon[j], dat_sub$lat[j]))
    }
    angle_row <- data.table(uid=i, angle=angle_btwn)
    sub_angle <- rbind(sub_angle, angle_row)
  }
  
  angle_dat <- rbind(angle_dat, sub_angle)
}
