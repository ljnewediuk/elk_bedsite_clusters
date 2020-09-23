
### Load packages ===
libs <- c('data.table', 'ggplot2', 'cowplot', 'grid', 'gridExtra')
lapply(libs, require, character.only=TRUE)

### Load data ===
model_data <- readRDS('output/model_data32m.rds')
dat_sens <- readRDS('output/dat_sens.rds')

# Factor "correct"
model_data$correct <- factor(model_data$correct, labels=c('Incorrect', 'Correct'))

###################################
### Predictor variable boxplots ===
# Create table for predictor variables and y-axis titles
plot_atts <- data.table(predictor=c('beds_in_20_m', 'n_within_dist', 'dist_nearest_pt'), title=c('Beds within 20 m', 'Points within 32 m', 'Distance to nearest point (m)'))
# Loop to create plots in list
pred_plots <- list()
for(i in 1:nrow(plot_atts)){
   # Create base plot
   p1 <- eval(substitute(
     ggplot(model_data, aes(y=get(plot_atts[i,]$predictor), x=correct, group=correct, fill=correct)) + 
     geom_boxplot(width=0.4) + scale_fill_manual(values=c('#F00640', '#7AB9FF')) +
     theme(panel.background=element_rect(fill='white', colour='black'), panel.grid=element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_text(size=18, colour='black'), axis.title.y = element_text(size=18, colour='black', vjust=4),
        axis.title.x = element_blank(), axis.ticks.x = element_blank(), plot.margin = unit(c(0.5,0.5,0.5,1), 'cm'), legend.position = 'none') +
     ylab(as.character(plot_atts[i,]$title))
   ,list(i = i)))
  # Add x-axis labels to the third plot
  if(i==1|i==2){
    p1 <- p1 + theme(axis.text.x = element_blank())
  } else {
    p1 <- p1 + theme(axis.text.x = element_text(size=18, colour='black'))
  }
  # Add plots to list
  pred_plots[[i]] <- p1
  
}

# ANOVA for difference in variables between correct and incorrect samples
# Beds in 20 m
anova(lm(beds_in_20_m~correct, data=model_data))
# NN distance
anova(lm(n_within_dist~correct, data=model_data))
# Distance to nearest point
anova(lm(dist_nearest_pt~correct, data=model_data))

##########################################################################
### Plot separation between incorrect and correctly identified samples ===
# Calculate difference between means
dat_sens[, 'difference' := .(yes_mean-no_mean)]
# Plot means
mean_diff_plot <- ggplot(dat_sens, aes(x=distance, y=difference)) + 
  geom_line(colour='black') +
  geom_vline(xintercept = 32, colour='#00d42e', linetype='dashed') +
  theme(panel.background = element_rect(fill='white'), panel.grid = element_blank(), panel.border = element_rect(colour='black', fill='NA'), 
        axis.text = element_text(colour='black', size=18), axis.title.y = element_text(colour='black', size=22, vjust=4), 
        axis.title.x = element_blank(), plot.margin = unit(c(0.5,0.5,1,1), 'cm')) +
  ylab('Mean (correct-incorrect points)') + xlab('NN distance between points (m)')

# Points with 80% CI at range of NN distances
nn_dist_plot <- ggplot(dat_sens) + 
  geom_point(aes(x=distance, y=yes_mean), col='#7AB9FF', size=2) + 
  geom_point(aes(x=distance, y=no_mean), col='#F00640', size=2) + 
  geom_ribbon(aes(x=distance, ymax=yes_mean+yes_se*1.96, ymin=yes_mean-yes_se*1.96), fill='#7AB9FF', alpha=0.5) + 
  geom_ribbon(aes(x=distance, ymax=no_mean+no_se*1.96, ymin=no_mean-no_se*1.96), fill='#F00640', alpha=0.5) +
  theme(panel.background = element_rect(fill='white'), panel.grid = element_blank(), panel.border = element_rect(colour='black', fill='NA'), 
        axis.text = element_text(colour='black', size=18), axis.title.y = element_text(colour='black', size=22, vjust=4), 
        axis.title.x = element_blank(), plot.margin = unit(c(0.5,0.5,1,1), 'cm')) +
  scale_y_continuous(breaks = c(0,2.25,4.5,6.75,9), labels = c('0.00', '0.25', '0.50', '0.75', '1.00')) +
  ylab('Proportion points within NN distance')

##################
### Save plots ===
# Save plot TIFF of boxplots
# tiff('figures/predictor_boxplots.tiff', width = 5, height = 12, units = 'in', res = 300)
pred_grobs <- plot_grid(pred_plots[[1]], pred_plots[[2]], pred_plots[[3]], ncol=1, nrow=3, align='v',
                        labels=c('A','B','C'), label_x=c(0.2,0.2,0.2), label_y=c(0.9,0.9,0.9), label_size=25)

# Save plot TIFF of NN distance
separation_grobs <- plot_grid(nn_dist_plot, mean_diff_plot, ncol=2, nrow=1, align='v', labels=c('A','B'), 
                              label_x =c(0.3,0.3), label_y=c(0.9,0.9), label_size=29)
# tiff('figures/NN_distance.tiff', width = 12, height = 7, units = 'in', res = 300)
grid.arrange(arrangeGrob(separation_grobs, bottom = textGrob('NN distance between points (m)', gp=gpar(fontsize=23), vjust=0.3)))

