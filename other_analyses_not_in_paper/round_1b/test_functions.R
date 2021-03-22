library(tidyverse)
library(reticulate)
library(challengescoring)

use_python("/usr/local/bin/python3")
# synapse <- import("synapseclient")
# syn <- synapse$Synapse()
# synutils <- synapse$utils
# syn$login()
source_python('evaluation_metrics.py')

spearman_py <- function(gold, pred){
  gold_py <- gold %>% np_array()
  pred_py <- pred %>% np_array()
  spearman(gold_py, pred_py)
}

pearson_py <- function(gold, pred){
  gold_py <- gold %>% np_array()
  pred_py <- pred %>% np_array()
  pearson(gold_py, pred_py)
}

rmse_py <- function(gold, pred){
  gold_py <- gold %>% np_array()
  pred_py <- pred %>% np_array()
  rmse(gold_py, pred_py)
}

auc_py <- function(gold, pred){
  gold_py <- gold %>% np_array()
  pred_py <- pred %>% np_array()
  average_AUC(gold_py, pred_py)
}
##auc doesnt work with randomized test data, here's example data
##auc_py(c(4,4,4,5,6,7,7,8,9,10), c(4,4,4,5,6,7,7,10,3,1))

ci_py <- function(gold, pred){
  gold_py <- gold %>% np_array()
  pred_py <- pred %>% np_array()
  ci(gold_py, pred_py)
}

f1_py <- function(gold, pred){
  gold_py <- gold %>% np_array()
  pred_py <- pred %>% np_array()
  f1(gold_py, pred_py)
}

##f1 doesnt work with randomized test data, here's example data
##f1_py(c(1,2,3,4,5,6,7,8),c(1,2,3,4,5,6,10,8))

bootLadderBoot(predictions = goodSim, ##data frame or path
               predictionColname = 'prediction',
               goldStandard = truth, ##data frame or path
               goldStandardColname = 'value',
               prevPredictions = badSim, ##data frame or path or NULL
               scoreFun = spearman_py,
               bootstrapN = 10000,
               reportBootstrapN = 10,
               bayesThreshold = 3,
               largerIsBetter = T,
               verbose = T,
               doParallel = T)

bootLadderBoot(predictions = goodSim, ##data frame or path
               predictionColname = 'prediction',
               goldStandard = truth, ##data frame or path
               goldStandardColname = 'value',
               prevPredictions = badSim, ##data frame or path or NULL
               scoreFun = rmse_py,
               bootstrapN = 10000,
               reportBootstrapN = 10,
               bayesThreshold = 3,
               largerIsBetter = F, ##Note RMSE largerIsBetter=F
               verbose = T,
               doParallel = T)


