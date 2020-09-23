
### Load Packages ===
require(data.table)
require(ggplot2)

### Load Data ===
dat_sens <- readRDS('output/dat_sens.rds')
clust_test <- readRDS('output/model_data32m.rds')

### Separation between incorrect and correctly identified samples ===
# Calculate difference between means
dat_sens[, 'difference' := .(yes_mean-no_mean)]
# Plot histogram
ggplot(dat_sens, aes(x=distance, y=difference)) + geom_line()

### Points with 80% CI at range of NN distances ===
# Save plot
# tiff('figures/NN_sensitivity_plot.tiff', width = 6, height = 6, units = 'in', res = 300)
# Plot
ggplot(dat_sens) + 
  geom_point(aes(x=distance, y=yes_mean), col='red', size=2) + 
  geom_point(aes(x=distance, y=no_mean), col='blue', size=2) + 
  geom_ribbon(aes(x=distance, ymax=yes_mean+yes_se*1.96, ymin=yes_mean-yes_se*1.96), fill='red', alpha=0.5) + 
  geom_ribbon(aes(x=distance, ymax=no_mean+no_se*1.96, ymin=no_mean-no_se*1.96), fill='blue', alpha=0.5) +
  theme(panel.background = element_rect(fill='white'), panel.grid = element_blank(), panel.border = element_rect(colour='black', fill='NA'), 
        axis.text = element_text(colour='black', size=18), axis.title.y = element_text(colour='black', size=22, vjust=4), 
        axis.title.x = element_text(colour='black', size=22, vjust=-4), plot.margin = unit(c(0.5,0.5,1,1), 'cm')) +
  scale_y_continuous(breaks = c(0,2.25,4.5,6.75,9), labels = c('0.00', '0.25', '0.50', '0.75', '1.00')) +
  ylab('Proportion points within NN distance') + xlab('NN distance between points (m)')

### Histogram of distance to sampled point ===
# Save plot
# tiff('figures/dist_to_point_density_plot.tiff', width = 6, height = 6, units = 'in', res = 300)
# Plot
ggplot() +
  geom_density(dat=clust_test[correct==1], aes(x=dist_nearest_pt), col='red') +
  geom_density(dat=clust_test[correct==0], aes(x=dist_nearest_pt), col='blue') +
  theme(panel.background = element_rect(fill='white'), panel.grid = element_blank(), panel.border = element_rect(colour='black', fill='NA'), 
        axis.text = element_text(colour='black', size=18), axis.title.y = element_text(colour='black', size=22, vjust=4), 
        axis.title.x = element_text(colour='black', size=22, vjust=-4), plot.margin = unit(c(0.5,0.5,1,1), 'cm')) +
  ylab('Density') + xlab('Distance between point and sample (m)')

### Boxplot of distance to sampled point ===
# Save plot
# tiff('figures/bedsite_histograms.tiff', width = 6, height = 6, units = 'in', res = 300)
# Plot
ggplot(dat=clust_test, aes(y=dist_nearest_pt, x=factor(correct), group=factor(correct), col=factor(correct))) +
  scale_x_discrete(labels = c('Incorrect', 'Correct')) +
  scale_color_manual(values=c('blue', 'red'), labels=c('Incorrect', 'Correct')) +
  geom_boxplot(width=0.35) +
  theme(panel.background = element_rect(fill='white'), panel.grid = element_blank(), panel.border = element_rect(colour='black', fill='NA'), 
        axis.text = element_text(colour='black', size=18), axis.title.y = element_text(colour='black', size=22, vjust=4), 
        axis.title.x = element_blank(), plot.margin = unit(c(0.5,0.5,1,1), 'cm'),
        legend.position = 'none') +
  ylab('Distance between point and sample (m)')

### Boxplot of nearest neighbour points ===
# Save plot
# tiff('figures/bedsite_histograms.tiff', width = 6, height = 6, units = 'in', res = 300)
# Plot
ggplot(dat=clust_test, aes(y=n_within_dist, x=factor(correct), group=factor(correct), col=factor(correct))) +
  scale_x_discrete(labels = c('Incorrect', 'Correct')) +
  scale_color_manual(values=c('blue', 'red'), labels=c('Incorrect', 'Correct')) +
  geom_boxplot(width=0.35) +
  theme(panel.background = element_rect(fill='white'), panel.grid = element_blank(), panel.border = element_rect(colour='black', fill='NA'), 
        axis.text = element_text(colour='black', size=18), axis.title.y = element_text(colour='black', size=22, vjust=4), 
        axis.title.x = element_blank(), plot.margin = unit(c(0.5,0.5,1,1), 'cm'),
        legend.position = 'none') +
  ylab('N pts within 32 m of focal pt')

### Boxplot of number of beds in 30 m ===
# Save plot
# tiff('figures/bedsite_histograms.tiff', width = 6, height = 6, units = 'in', res = 300)
# Plot
ggplot(dat=clust_test, aes(y=beds_in_20_m, x=factor(correct), group=factor(correct), col=factor(correct))) +
  scale_x_discrete(labels = c('Incorrect', 'Correct')) +
  scale_color_manual(values=c('blue', 'red'), labels=c('Incorrect', 'Correct')) +
  geom_boxplot(width=0.35) +
  theme(panel.background = element_rect(fill='white'), panel.grid = element_blank(), panel.border = element_rect(colour='black', fill='NA'), 
        axis.text = element_text(colour='black', size=18), axis.title.y = element_text(colour='black', size=22, vjust=4), 
        axis.title.x = element_blank(), plot.margin = unit(c(0.5,0.5,1,1), 'cm'),
        legend.position = 'none') +
  ylab('Number of beds in 30 m')


