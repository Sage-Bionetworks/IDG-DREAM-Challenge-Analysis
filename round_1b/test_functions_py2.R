library(tidyverse)
library(reticulate)
library(challengescoring)

use_python("/usr/local/bin/python2")
synapse <- import("synapseclient")
syn <- synapse$Synapse()
synutils <- synapse$utils
syn$login()

source_python('../round_1/evaluation_metrics_python2_test.py')


best_path <- syn$get("syn17053907")$path
gold_path <- syn$get("syn16809884")$path
rand_path <- syn$get("syn17053881")$path

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

fcts <- c(f1_py)

for(i in fcts){
  print(i)

  out <- bootLadderBoot(predictions = best_path,
                        predictionColname = 'pKd_.M._pred',
                        goldStandard = gold_path,
                        goldStandardColname = 'pKd_.M.',
                        prevPredictions = rand_path,
                        scoreFun = i,
                        bootstrapN = 2,
                        reportBootstrapN = 2,
                        bayesThreshold = 3,
                        largerIsBetter = T,
                        doParallel = F)

  print(out)
}

print(rmse_py)

out <- bootLadderBoot(predictions = best_path,
                      predictionColname = 'pKd_.M._pred',
                      goldStandard = gold_path,
                      goldStandardColname = 'pKd_.M.',
                      prevPredictions = rand_path,
                      scoreFun = rmse_py,
                      bootstrapN = 10000,
                      reportBootstrapN = 10,
                      bayesThreshold = 3,
                      largerIsBetter = F,
                      doParallel = T)

print(out)

