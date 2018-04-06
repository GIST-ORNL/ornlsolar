
# install and/or load required packages

if (!require("caret")) { 
  install.packages('caret',
                   repos="https://cran.rstudio.com/",
                   quiet = TRUE)
  require('caret')
}
if (!require("rgdal")) { 
  install.packages('rgdal',
                   repos="https://cran.rstudio.com/",
                   quiet = TRUE)
  require('rgdal')
}
if (!require("reshape2")) { 
  install.packages('reshape2',
                   repos="https://cran.rstudio.com/",
                   quiet = TRUE)
  require('rgdal')
}
if (!require("dplyr")) { 
  install.packages('dplyr',
                   repos="https://cran.rstudio.com/",
                   quiet = TRUE)
  require('dplyr')
}
if (!require("e1071")) { 
  install.packages('e1071',
                   repos="https://cran.rstudio.com/",
                   quiet = TRUE)
  require('e1071')
}
if (!require("randomForest")) { 
  install.packages('randomForest',
                   repos="https://cran.rstudio.com/",
                   quiet = TRUE)
  require('randomForest')
}
if (!require("ada")) { 
  install.packages('ada',
                   repos="https://cran.rstudio.com/",
                   quiet = TRUE)
  require('ada')
}
if (!require("ggplot2")) { 
  install.packages('ggplot2',
                   repos="https://cran.rstudio.com/",
                   quiet = TRUE)
  require('ggplot2')
}
if (!require("nnet")) { 
  install.packages('nnet',
                   repos="https://cran.rstudio.com/",
                   quiet = TRUE)
  require('nnet')
}
if (!require("neuralnet")) { 
  install.packages('neuralnet',
                   repos="https://cran.rstudio.com/",
                   quiet = TRUE)
  require('neuralnet')
}
if (!require("GGally")) { 
  install.packages('GGally',
                   repos="https://cran.rstudio.com/",
                   quiet = TRUE)
  require('GGally')
}


# choose which dataset you're running algorithms
# on

data_set <- "aut_dc"
#data_set <- "man_dc"
#data_set <- "aut_bos"
#data_set <- "man_bos"

# if you want to train on DC and test on boston select the
# dataset below

#data_set <- "aut_combo"

# set data directory

dir <- "C:/Users/mqm/Desktop/ornl_data_code/data"

# determine if the algorithm will be dealing with boston
# or dc

area <- substring(data_set,5,6)
type <- substring(data_set,1,3)

if (area=="dc"){
  real_area <- "dc"
  image_name <- "DC"
  abbr_area <- "dc"
}else if (area=="bo"){
  real_area <- "boston"
  image_name <- "Boston"
  abbr_area <- "bos"
}else{
  real_area <- "dc"
  image_name <- "\nTrained on DC and Tested on Boston"
  abbr_area <- "dc"
}

# determine if the algorithm will be dealing with the
# automatically or manually detected parcels. Change 
# barchart colors and text accordingly


if (type=="aut"){
  image_class <- "Automatically" 
  fill_color <- "steelblue"
  word_color <- "white"
}else{
  image_class <- "Manually" 
  fill_color <- "#FF9999"
  word_color <- "black"
}

# read in data

feat_dir <- paste(dir,
                  "/parcels/not_classified/",
                  real_area,
                  "/parcels_",
                  type,
                  "_",
                  abbr_area,
                  sep="")
feat_name <- paste("parcels_",type,"_",abbr_area,sep="")
feat <- readOGR(feat_dir,feat_name,stringsAsFactors=FALSE)
feat_data <- feat@data

# if you are training on DC and testing on Boston
# also read in Boston file

if (image_name == "\nTrained on DC and Tested on Boston"){
  
  feat_dir <- paste(dir,
                    "/parcels/not_classified/",
                    "boston",
                    "/parcels_",
                    "aut_bos",
                    sep="")
  feat_name <- paste("parcels_","aut_bos",sep="")
  feat_bos <- readOGR(feat_dir,feat_name,stringsAsFactors=FALSE)
  feat_data_bos <- feat_bos@data
  
}

# store pre-scaled data

feat_data_pre <- feat_data

if (image_name == "\nTrained on DC and Tested on Boston"){
  
  feat_data_pre_bos <- feat_data_bos  
  
}

# scale feat_data 

feat_data[3:(dim(feat_data)[2]-1)] <- scale(feat_data[3:(dim(feat_data)[2]-1)])

if (image_name == "\nTrained on DC and Tested on Boston"){
  
  feat_data_bos[3:(dim(feat_data_bos)[2]-1)] <- scale(feat_data_bos[3:(dim(feat_data_bos)[2]-1)])
  
}

# get all combinations of relevant features 

combinations <- list((combn(4,1)),
                     (combn(4,2)),
                     (combn(4,3)),
                     (combn(4,4)))

new_combos <- list()

k <- 0

for (i in 1:length(combinations)){  
  len_combo <- dim(combinations[[i]])[2]
  
  for (j in 1:len_combo){
    k <- k+1
    new_combos[[k]] <- combinations[[i]][,j]+2
    
  }
}

# get number of combinations

new_combs_length <- length(new_combos)

# initialize vectors for overall accuracy, accuracy of positive class (commercial)
# and accuracy of negative class (residential) for each simulation

accuracynn <- NULL
accuracyrf <- NULL
accuracylr <- NULL

posnn <- NULL
posrf <- NULL
poslr <- NULL

negnn <- NULL
negrf <- NULL
neglr <- NULL

# calculate the mean and standard deviations of the accuracies
# for each set of simulations

meannn <- NULL
meanrf <- NULL
meanlr <- NULL

sdnn <- NULL
sdrf <- NULL
sdlr <- NULL

meanposrf <- NULL
meannegrf <- NULL
meanposlr <- NULL
meanneglr <- NULL
meanposnn <- NULL
meannegnn <- NULL

sdposrf <- NULL
sdnegrf <- NULL
sdposlr <- NULL
sdneglr <- NULL
sdposnn <- NULL
sdnegnn <- NULL

# initialize list to store trained models and to keep 
# best models

trainedrf <- list()
trainedlr <- list()
trainednn <- list()

chosenrf <- list()
chosenlr <- list()
chosennn <- list()

stored_accuracies <- list()

# define number of times you would like to train
# and test each model

num_simulations <- 625

# load random seeds generated for models (so that user can
# generate results consistent with paper)

load("random_seed.Rdata")
load("random_seed_k.Rdata")

# train and test algorithms multiple times on all feature combinations 

for (k in 1:new_combs_length){
  
  for (i in 1:num_simulations){
    
    # extract subset of features from data
    
    feat_data_sub <- feat_data[,c(1,2,new_combos[[k]],dim(feat_data)[2])]
    
    if (image_name == "\nTrained on DC and Tested on Boston"){
      
      feat_data_sub_bos <- feat_data_bos[,c(1,2,new_combos[[k]],dim(feat_data_bos)[2])]
      
    }
    
    # extract labeled data (stored in to_model) and non-labeled data to 
    # classify (stored in to_classify)
    
    to_model <- feat_data_sub[which(!is.na(feat_data_sub$class)),] 
    
    if (image_name == "\nTrained on DC and Tested on Boston"){
      
      to_model_bos <- feat_data_sub_bos[which(!is.na(feat_data_sub_bos$class)),] 
      to_classify <- feat_data_sub_bos[which(is.na(feat_data_sub_bos$class)),]
      to_classify$class <- NULL
      
    }else{
      to_classify <- feat_data_sub[which(is.na(feat_data_sub$class)),]
      to_classify$class <- NULL
    }
    
    # set the seed so that we can produce the same results consistently
    # (if you don't set the seed you will get slightly different results
    # each time). this will make it easier to reproduce results in paper
    
    set.seed(random_seed[i])
    
    # split the 'to_model' subset into training and testing datasets while making sure
    # the training dataset has more than one unique value
    
    length_mat  <- 1
    
    while (length_mat < 2){
      trainind <- createDataPartition(y = to_model$class, p = 0.75, list = FALSE)
      training <- to_model[trainind,]
      unique_features <- unique(training[,-c(1,2,dim(training)[2]),drop=FALSE])
      length_mat <- dim(unique_features)[1]
    }
    
    trainingy <- as.factor(training$class)
    training$class <- NULL
    
    if (image_name == "\nTrained on DC and Tested on Boston"){
      trainindbos <- createDataPartition(y = to_model_bos$class, p = 0.75, list = FALSE)
      testing <- to_model_bos[-trainindbos,]
      testingy <- as.factor(testing$class)
      testing$class <- NULL
    }else{
      testing <- to_model[-trainind,]
      testingy <- as.factor(testing$class)
      testing$class <- NULL
    }
    
    # train random forest model
    
    rFmodel <- randomForest(x=training[,c(-1,-2),drop=FALSE],
                            y=trainingy,
                            ntree=1000,
                            importance = TRUE)
    
    # make predictions on testing set with trained model
    
    predrF <- predict(rFmodel, testing[,c(-1,-2),drop=FALSE], type = "response")
    predrF <- c(as.character(predrF),"com","res")
    predrF <- as.factor(predrF)
    predrF <- predrF[-((length(predrF)-1):length(predrF))]
    
    # calculate accuracies on testing set (pos class = com, so pos class
    # accuracy represents the percentage of commercial predictions
    # that are actually commercial)
    
    confmat <- confusionMatrix(predrF,testingy)
    accuracyrf <- c(accuracyrf,confmat$overall['Accuracy'])
    posrf <- c(posrf,confmat$byClass["Pos Pred Value"])
    negrf <- c(negrf,confmat$byClass["Neg Pred Value"])
    
    # store trained model
    
    trainedrf[[i]] <- rFmodel
    
    # train logistic model
    
    LR <- glm(trainingy~.,
              data=training[,c(-1,-2),drop=FALSE],
              family=binomial("logit"))
    LRstep <- step(LR, direction = "both", trace = FALSE)
    
    # make predictions on testing set with trained model (pos class = com)
    
    predLR <- predict(LRstep, newdata = testing[,c(-1,-2),drop=FALSE], type = 'response')
    predLR <- ifelse(predLR > 0.5, "res", "com")
    predLR <- c(predLR,"com","res")
    predLR <- as.factor(predLR)
    predLR <- predLR[-((length(predLR)-1):length(predLR))]
    
    # calculate accuracies on testing set
    
    confmat <- confusionMatrix(predLR,testingy)
    accuracylr <- c(accuracylr,confmat$overall['Accuracy'])
    poslr <- c(poslr,confmat$byClass["Pos Pred Value"])
    neglr <- c(neglr,confmat$byClass["Neg Pred Value"])
    
    # store trained model 
    
    trainedlr[[i]] <- LRstep
    
    # train neural network model 
    
    numtrainingy <- as.numeric(trainingy)
    num_col <- colnames(training[,sapply(training, is.numeric),drop=FALSE])
    num_col <- sapply(num_col, function(x) paste(x, "+"))
    num_col <- paste(num_col, collapse = " ")
    num_col <- substr(num_col, 1, nchar(num_col)-1)
    formula <- paste("numtrainingy","~",collapse = "")
    formula <- paste(formula, num_col, collapse = " ")
    NN <- neuralnet(formula,
                    data = training[,c(-1,-2),drop=FALSE],
                    hidden = 10,
                    threshold = 0.3,
                    stepmax = 1e+07,
                    startweights = NULL)
    
    # make predictions on testing set with trained model (pos class = com)
    
    predNN <- compute(NN, testing[,c(-1,-2),drop=FALSE])
    predNN <- predNN$net.result
    predNN <- ifelse(predNN > 1.5, "res", "com")
    predNN <- c(predNN,"com","res")
    predNN <- as.factor(predNN)
    predNN <- predNN[-((length(predNN)-1):length(predNN))]
    
    # calculate accuracies on testing set
    
    confmat <- confusionMatrix(predNN,testingy)
    accuracynn <- c(accuracynn,confmat$overall['Accuracy'])
    posnn <- c(posnn,confmat$byClass["Pos Pred Value"])
    negnn <- c(negnn,confmat$byClass["Neg Pred Value"])
    
    # store trained model
    
    trainednn[[i]] <- NN
  }
  
  # compute the mean and sd accuracies and reset vectors
  
  meannn <- c(meannn,round(mean(accuracynn), digits = 4))
  meanrf <- c(meanrf,round(mean(accuracyrf), digits = 4))
  meanlr <- c(meanlr,round(mean(accuracylr), digits = 4))
  
  sdlr <- c(sdlr,round(sd(accuracylr), digits = 4))
  sdnn <- c(sdnn,round(sd(accuracynn), digits = 4))
  sdrf <- c(sdrf,round(sd(accuracyrf), digits = 4))
  
  meanposrf <- c(meanposrf,round(mean(posrf), digits = 4))
  meanposlr <- c(meanposlr,round(mean(poslr), digits = 4))
  meanposnn <- c(meanposnn,round(mean(posnn), digits = 4))
  
  meannegrf <- c(meannegrf,round(mean(negrf), digits = 4))
  meanneglr <- c(meanneglr,round(mean(neglr), digits = 4))
  meannegnn <- c(meannegnn,round(mean(negnn), digits = 4))
  
  sdposrf <- c(sdposrf,round(sd(posrf), digits = 4))
  sdposlr <- c(sdposlr,round(sd(poslr), digits = 4))
  sdposnn <- c(sdposnn,round(sd(posnn), digits = 4))
  
  sdnegrf <- c(sdnegrf,round(sd(negrf), digits = 4))
  sdneglr <- c(sdneglr,round(sd(neglr), digits = 4))
  sdnegnn <- c(sdnegnn,round(sd(negnn), digits = 4))
  
  # randomly save one of the algorithms from the set of simulations
  
  set.seed(random_seed_k[k])
  
  ranrf <- floor(runif(1, 1,length(accuracyrf)))
  ranlr <- floor(runif(1, 1,length(accuracylr)))
  rannn <- floor(runif(1, 1,length(accuracynn)))
  
  chosenrf[[k]] <- trainedrf[ranrf]
  chosenlr[[k]] <- trainedlr[ranlr]
  chosennn[[k]] <- trainednn[rannn]
  
  
  stored_accuracies[[k]] <- list(accuracyrf,
                                 accuracylr,
                                 accuracynn)
  
  # reset accuracy vectors to calculate accuracies of next rounds
  
  accuracynn <- NULL
  accuracyrf <- NULL
  accuracylr <- NULL
  
  posrf <- NULL
  negrf <- NULL
  poslr <- NULL
  neglr <- NULL
  posnn <- NULL
  negnn <- NULL
  
  print(k)
  print(proc.time())
}

# combine accuracies in matrix

accuracies <- cbind(meanrf, meanlr, meannn)

accuracies_2 <- cbind(meanrf, meanlr, meannn, sdrf, sdlr, sdnn,
                      meanposrf, meanposlr, meanposnn, sdposrf, sdposlr, sdposnn,
                      meannegrf, meanneglr, meannegnn, sdnegrf, sdneglr, sdnegnn)

# restore data so it's not scaled

feat_data <- feat_data_pre

if (image_name == "\nTrained on DC and Tested on Boston"){
  
  feat_data_bos <- feat_data_pre_bos
  
}

# get the predictions from the best feature combination/algorithm 'to classify' subset

finalalgo <- which(accuracies == max(accuracies), arr.ind = TRUE)[1,]
finalalgo <- t(as.matrix(finalalgo))


if (finalalgo[,2] == 1){
  pred <- predict(chosenrf[[finalalgo[,1]]][[1]], 
                  to_classify[,new_combos[[finalalgo[,1]]],drop=FALSE], 
                  type = "response")
}
if (finalalgo[,2] == 2){
  predLR <- predict(chosenlr[[finalalgo[,1]]][[1]], 
                    newdata = to_classify[,new_combos[[finalalgo[,1]]],drop=FALSE], 
                    type = 'response')
  pred <- ifelse(predLR > 0.5, "res", "com")
}
if (finalalgo[,2] == 3){
  pred <- compute(chosennn[[finalalgo[,1]]][[1]], 
                  to_classify[,new_combos[[finalalgo[,1]]],drop=FALSE])
  pred <- pred$net.result
  pred <- ifelse(pred > 1.5, "res", "com")
}

# fill in original data with predictions 

to_classify$class <- as.character(pred)


if (image_name == "\nTrained on DC and Tested on Boston"){
  
  out_shp_directory <- paste(dir,"/parcels/classified/boston/parcels_class_",data_set,sep="")
  
  feat_data_bos$class_ind <- 0
  feat_data_bos$class_ind[which(is.na(feat_data_bos$class))] <- 1
  feat_data_bos[match(to_classify$parcid,feat_data_bos$parcid),"class"] <- to_classify$class
  feat_bos@data <- feat_data_bos
  
  # export classified shapefile
  
  
  writeOGR(feat_bos, 
           dsn = out_shp_directory, 
           layer = paste("parcels_class_",data_set,sep=""),
           driver = "ESRI Shapefile", 
           check_exists=TRUE, 
           overwrite_layer = TRUE)
  
}else{
  
  out_shp_directory <- paste(dir,"/parcels/classified/",real_area,"/parcels_class_",data_set,sep="")
  
  feat_data$class_ind <- 0
  feat_data$class_ind[which(is.na(feat_data$class))] <- 1
  feat_data[match(to_classify$parcid,feat_data$parcid),"class"] <- to_classify$class
  feat@data <- feat_data
  
  # export classified shapefile
  
  writeOGR(feat, 
           dsn = out_shp_directory, 
           layer = paste("parcels_class_",data_set,sep=""),
           driver = "ESRI Shapefile", 
           check_exists=TRUE, 
           overwrite_layer = TRUE)
}

# reprocess data so it can be plotted with ggplot 

combo_names <- list()
paste_names <- as.character(1:length(new_combos))

# create dataframe with feature combinations and accuracies

for (i in 1:length(new_combos)){
  combo_names[[i]] <- colnames(feat_data)[new_combos[[i]]]
  paste_names[i] <- paste(combo_names[[i]],"\n",collapse="")
  
}

paste_names <- substring(paste_names,1,nchar(paste_names)-2)

for (i in 1:length(new_combos)){
    
  if (length(combo_names[[i]]) < 4) {
    
    paste_names[i] <- paste(paste_names[i],")",sep="")
    paste_names[i] <- paste(paste_names[i],"\n",sep="")
    
  }else{
    paste_names[i] <- paste(paste_names[i],")",sep="")
  }

}

results_df <- data.frame(cbind(paste_names,
                               meannn,sdnn,
                               meanrf,sdrf,
                               meanlr,sdlr), 
                         stringsAsFactors = FALSE)

accuracies_2 <- data.frame(cbind(paste_names,
                               accuracies_2), 
                         stringsAsFactors = FALSE)

tables_directory <- "C:/Users/mqm/Desktop/ornl_data_code_epsa/code/output_tables/"

write.csv(accuracies_2,file=paste(tables_directory,data_set,".csv",sep=""))

# reformat dataframe so it can be plotted by ggplot2 

results_melt <- melt(results_df, 
                     id.vars = "paste_names", 
                     measure.vars = colnames(results_df[,seq(2,(dim(results_df)[2]-1),2)]))
results_melt_sd <- melt(results_df, 
                        id.vars = "paste_names", 
                        measure.vars = colnames(results_df[,seq(3,dim(results_df)[2],2)]))

results_melt$algorithm <- substring(results_melt$variable,5,6)
results_melt$variable <- NULL
colnames(results_melt) <- c("features","mean","algorithm")

results_melt_sd$algorithm <- substring(results_melt_sd$variable,3,4)
results_melt_sd$variable <- NULL
colnames(results_melt_sd) <- c("features","sd","algorithm")

new_results <- left_join(results_melt,results_melt_sd,by=c("features","algorithm"))

# extract feature set with highest mean accuracy for each algorithm

results_melt_max <- new_results %>% group_by(algorithm) %>% filter(mean==max(mean))

# reformat title so it can be more easily plotted 

results_melt_max$title <- paste(results_melt_max$algorithm,"\n \n (",results_melt_max$features,sep="")

# make sure mean and sd are numeric

results_melt_max$mean <- as.numeric(results_melt_max$mean)
results_melt_max$sd <- as.numeric(results_melt_max$sd)

# plot box and whisker plots with jitter 

max_accuracies <- t(as.matrix(apply(accuracies,2,which.max)))

rf_max <- rbind("rf",stored_accuracies[[max_accuracies[,1]]][[1]])
lr_max <- rbind("lr",stored_accuracies[[max_accuracies[,2]]][[2]])
nn_max <- rbind("nn",stored_accuracies[[max_accuracies[,3]]][[3]])

dat <- data.frame(t(cbind(rf_max,lr_max,nn_max)),stringsAsFactors = FALSE)

colnames(dat)[1] <- c("algorithm")
colnames(dat)[2] <- c("accuracy")

dat$title <- results_melt_max$title[match(dat$algorithm,results_melt_max$algorithm)]
dat$mean <- results_melt_max$mean[match(dat$algorithm,results_melt_max$algorithm)]

dat$accuracy <- as.numeric(dat$accuracy)
dat$algorithm <- as.factor(dat$algorithm)
dat$title <- as.factor(dat$title)

axis_upper <- max(dat$accuracy)+0.01
axis_lower <- min(dat$accuracy)-0.06

boxplot <- ggplot(dat, aes(reorder(title,-mean), accuracy)) +
  labs(title="Box Plots of Best Performing Feature Combination \nfor each Algorithm",subtitle=paste("Dataset with ",image_class," Detected Solar Panels - " ,image_name, " Area",sep=""))+
  xlab("\n Algorithm (Feature Combination)") +
  ylab("Accuracy") +
  geom_boxplot() + geom_jitter(width = 0.2,height=.001) + 
  coord_cartesian(ylim = c(axis_lower,axis_upper))+
  theme(axis.text.x=element_text(size=17,face="bold"),
        axis.text.y=element_text(size=17,face="bold"),
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=17,face="bold"),
        plot.subtitle=element_text(size=15.5))

# save image

image_directory <- "C:/Users/mqm/Desktop/ornl_data_code/code/images/"

ggsave(paste(image_directory,data_set,"boxplot.png"), boxplot, width = 8, height = 6, unit = "in",
       dpi = 200) 

# plot bar charts 

auto_comp <- ggplot(data = results_melt_max, aes(x=reorder(title,-mean), y=mean))+
  geom_bar(stat="identity", fill=fill_color)+
  labs(title="Best Performing Feature Combination \nfor each Algorithm",subtitle=paste("Dataset with ",image_class," Detected Solar Panels - " ,image_name, " Area",sep=""))+
  xlab("\n Algorithm (Feature Combination)") +
  ylab("Mean Classification Accuracy") +
  coord_cartesian(ylim = c(axis_lower,axis_upper))+
  #scale_y_continuous(name="Accuracy", limits=c(0.80, 1))+
  geom_errorbar(aes(ymin=mean - sd, ymax = mean + sd), width=.3, color="red")+
  geom_text(aes(label=paste("\n Mean = ",round(mean,4),"\n SD = ",round(sd,4),sep="")), vjust=2, color=word_color, size=6)+
  theme(axis.text.x=element_text(size=17,face="bold"),
        axis.text.y=element_text(size=17,face="bold"),
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=17,face="bold"),
        plot.subtitle=element_text(size=15.5)) #+


# save image 

image_directory <- "C:/Users/mqm/Desktop/ornl_data_code/code/images/"

ggsave(paste(image_directory,data_set,".png"), auto_comp, width = 8, height = 6, unit = "in",
       dpi = 200) 

# save df in correct format

results_df$meanrf <- paste(results_df$meanrf," (",results_df$sdrf,")",sep="")
results_df$meanlr <- paste(results_df$meanlr," (",results_df$sdlr,")",sep="")
results_df$meannn <- paste(results_df$meannn," (",results_df$sdnn,")",sep="")

results_df <- results_df[,c("paste_names","meanrf","meanlr","meannn")]

maxnchar <- which.max(nchar(results_df$paste_names))
origmax <- results_df$paste_names[maxnchar]


results_df$paste_names <- substring(results_df$paste_names,1,(nchar(results_df$paste_names)-2))
results_df$paste_names[maxnchar] <- substring(origmax,1,(nchar(origmax)-1))

results_df$paste_names <- gsub(" \n", ", ",results_df$paste_names)

bestalgo <- results_melt_max$algorithm[which.max(results_melt_max$mean)]

results_df <- results_df[order(results_df[,paste("mean",bestalgo,sep="")],decreasing=TRUE),]

colnames(results_df)[which(colnames(results_df)==paste("mean",bestalgo,sep=""))] <- paste("mean",bestalgo,"*",sep="")

write.csv(results_df,file=paste(tables_directory,data_set,"_table_acc.csv",sep=""))

