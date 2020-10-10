
### Load Packages ===
libs <- c('data.table', 'rsample', 'tune', 'caret', 'recipes', 'MLeval', 'ggplot2')
lapply(libs, require, character.only=TRUE)

### Set NN distance ===
nn_dist <- 32

### Load Data ===
model_data <- readRDS(paste('input/model_data', paste(nn_dist, 'm.rds', sep=''), sep=''))

# Factor "correct" for classification
model_data$correct <- factor(model_data$correct, labels=make.names(c('no', 'yes')))



nb_tg <- readRDS('output/nb_tp.rds')
svm_tg <- readRDS('output/svmRadial_tp.rds')
lda_tg <- readRDS('output/lda_tp.rds')

### Set control parameters for tuning parameter selection ===
tctrl <- trainControl(method='repeatedcv', number=10, repeats=5, classProbs=TRUE, summaryFunction = twoClassSummary, savePredictions = T)

mod_clust_bed <- caret::train(correct ~ n_within_dist + beds_in_20_m, tuneGrid=nb_tg, trControl=tctrl, data=model_data, method='nb', metric='ROC')
mod_clust <- caret::train(correct ~ n_within_dist, tuneGrid=nb_tg, trControl=tctrl, data=model_data, method='nb', metric='ROC')
mod_specificity <- caret::train(correct ~ beds_in_20_m, trControl=tctrl, data=model_data, method='lda', metric='ROC')

mod_rocs <- data.table()
for(i in c('clust_bed', 'clust')){
  mod <- get(paste('mod', i, sep='_'))
  eval_mod <- as.data.table(evalm(mod)$roc$data)
  eval_mod[, 'Group' := .(i)]
  
  mod_rocs <- rbind(mod_rocs, eval_mod)
}

# tiff('figures/ROC_curve.tiff', width = 7, height = 7, units = 'in', res = 300)
ggplot(mod_rocs, aes(x=FPR, y=SENS, group=Group, colour=Group)) + 
  geom_abline(intercept = 0, slope=1,  linetype='dashed') + geom_line(size=2) +
  scale_colour_manual(values=c('#7c7a77', '#cfd0d2'), labels=c('clust (AUC = 0.59)', 'clust + bed (AUC = 0.78)'), name='Highest:') +
  theme(plot.margin=unit(c(0.25,0.25,0.75,0.75), 'cm'), panel.background=element_rect(fill='white', colour='black'), panel.grid = element_blank(),
        axis.text=element_text(size=18, colour='black'), axis.title.x=element_text(size=22, colour='black', vjust=-3), axis.title.y=element_text(size=22, colour='black', vjust=5),
        legend.position=c(0.7,0.25), legend.title=element_blank(), legend.text=element_text(size=16, colour='black'), legend.key.height=unit(1, 'cm'), legend.key=element_rect(fill='white')) +
  ylab('True positive rate') + xlab('False positive rate')

