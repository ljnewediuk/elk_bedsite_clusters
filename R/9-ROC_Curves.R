#######################################################
#### 8 - Calculate AUC for the best models         ####
####     and plot ROC curves                       ####
#######################################################

### Load Packages ====
libs <- c('data.table', 'rsample', 'tune', 'caret', 'recipes', 'MLeval', 'ggplot2')
lapply(libs, require, character.only=TRUE)

### Set NN distance ====
nn_dist <- 32

### Load Data ====
model_data <- readRDS(paste('input/model_data', 
                            paste(nn_dist, 'm.rds', sep=''), sep=''))

# Factor "correct" for classification
model_data$correct <- factor(model_data$correct, labels=make.names(c('no', 'yes')))

# Load tuning parameters
nb_tg <- readRDS('output/nb_tp.rds')
svmRadial_tg <- readRDS('output/svmRadial_tp.rds')
lda_tg <- readRDS('output/lda_tp.rds')

### Set control parameters for tuning parameter selection ====
tctrl <- trainControl(method='repeatedcv', number=10, repeats=5, 
          classProbs=TRUE, summaryFunction = twoClassSummary, savePredictions = T)

### Create table of model specifications (based on table 1) ====
specs <- data.table(modno=seq(1,15,1),
                    alg=c('nb', 'lda', 'nb', 'nb', rep('lda',3), 'svmRadial', 
                          rep('nb',7)),
                    param1=c(rep('beds_in_20_m',8), rep('n_within_dist',4), 
                             'dist_nearest_pt', rep('nn_avg',2)),
                    param2=c('n_within_dist', NA, rep('n_within_dist',2), 
                             'dist_nearest_pt', 'nn_avg', 'n_within_dist', 
                             'nn_avg', NA, 'dist_nearest_pt', rep('nn_avg',2), 
                             rep(NA,2), 'dist_nearest_pt'),
                    param3=c(rep(NA,2), 'nn_avg', 'dist_nearest_pt', 
                             rep(NA,2), 'nn_avg', 'dist_nearest_pt', rep(NA,3), 
                             'dist_nearest_pt', rep(NA,3)),
                    param4=c(rep(NA,6), 'dist_nearest_pt', rep(NA,8)),
                    nparams=c(2,1,3,3,2,2,4,3,1,2,2,3,1,1,2))

### Run models ====
for(i in 1:nrow(specs)){
  # subset out specifications
  sub_specs <- specs[i,]
  # set tuning parameters
  alg_tg <- paste(sub_specs$alg, 'tg', sep='_')
  # set parameters
  param_list <- paste(sub_specs[,3:as.numeric(2+sub_specs$nparams)])
  # If algorithm has tuning parameters, run models with tuning grid, 
  # otherwise without (i.e. LDA)
  if(sub_specs$alg=='nb' | sub_specs$alg=='svmRadial'){
    assign(paste('auc_mod', sub_specs$modno, sep='_'), 
           caret::train(reformulate(c(param_list), response='correct'), 
                        tuneGrid=get(alg_tg), trControl=tctrl, data=model_data, 
                        method=sub_specs$alg, metric='ROC'))
  } else {
    assign(paste('auc_mod', sub_specs$modno, sep='_'), 
           caret::train(reformulate(c(param_list), response='correct'), 
                        trControl=tctrl, data=model_data, method=sub_specs$alg, 
                        metric='ROC'))
  }
  
}

### Summarize results ====
# Create table for sensitivity and false positives
mod_rocs <- data.table()
# Create table for AUCs
mod_aucs <- data.table()
# Loop
for(i in c(1:15)){
  # Compile ROC data
  mod <- get(paste('auc_mod', i, sep='_'))
  eval_mod <- as.data.table(evalm(mod)$roc$data)
  eval_mod[, 'Group' := .(i)]
  # Calculate AUC and 95% CI
  mean_auc <- mean(sample(eval_mod$SENS,1000,replace=T) 
                   > sample(eval_mod$FPR,1000,replace=T))
  btstrp_auc <- replicate(1000,mean(sample(eval_mod$SENS,1000,replace=T) 
                                    > sample(eval_mod$FPR,1000,replace=T)))
  lower <- mean_auc-(sd(btstrp_auc)/sqrt(length(btstrp_auc)))*1.96
  upper <- mean_auc+(sd(btstrp_auc)/sqrt(length(btstrp_auc)))*1.96
  aucs <- data.table(model=i, auc=mean_auc, lower, upper)
    
  mod_aucs <- rbind(mod_aucs, aucs)
  mod_rocs <- rbind(mod_rocs, eval_mod)
}

### Plot ROC curves ====
# tiff('figures/ROC_curve.tiff', width = 7, height = 7, units = 'in', res = 300)
ggplot(mod_rocs[Group==1 | Group==9], aes(x=FPR, y=SENS, group=Group, colour=Group)) + 
  geom_abline(intercept = 0, slope=1,  linetype='dashed') + geom_line(size=2) +
  scale_colour_manual(values=c('#7c7a77', '#cfd0d2'), labels=c('buffer + bed (AUC = 0.73)', 'buffer (AUC = 0.55)'), name='Highest:') +
  theme(plot.margin=unit(c(0.25,0.25,0.75,0.75), 'cm'), 
        panel.background=element_rect(fill='white', colour='black'), 
        panel.grid = element_blank(),
        axis.text=element_text(size=18, colour='black'), 
        axis.title.x=element_text(size=22, colour='black', vjust=-3), 
        axis.title.y=element_text(size=22, colour='black', vjust=5),
        legend.position=c(0.7,0.25), 
        legend.title=element_blank(), 
        legend.text=element_text(size=16, colour='black'), 
        legend.key.height=unit(1, 'cm'), legend.key=element_rect(fill='white')) +
  ylab('True positive rate') + xlab('False positive rate')


