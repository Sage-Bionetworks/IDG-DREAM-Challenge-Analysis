library(tidyverse)
library(reticulate)
use_python('/usr/local/bin/python')

synapse <- import('synapseclient')
syn <- synapse$Synapse()
syn$login()

library(challengescoring)
library(doMC)
registerDoMC()

submitted_fv <- syn$tableQuery("select * from syn18485249", includeRowIdAndRowVersion = F)$asDataFrame()

##challengeutils query 'select auc, ci, createdOn, f1, name, objectId, pearson, rmse, spearman, submitterId, team, userId, bayes, metCutoff from evaluation_9614192 where prediction_file_status == "SCORED"'
leaderboard <- read_csv('round_1b/r1b_leaderboard.csv') %>%
  select(objectId, name, createdOn, submitterId, userId, team, auc, ci, f1, pearson, rmse, spearman, met_cutoff, bayes) %>%
  left_join(submitted_fv, by = c('objectId' = 'submissionId', 'userId')) %>%
  filter(!is.na(spearman))

gs <- syn$get('syn16809884')$path
##submissions before createdOn = 1.549496e+12 didn't track bayes factor, so let's calculate those now
##rows 1:67


#### IMPORT CHALLENGE SCORING FUNCTIONS
reticulate::source_python('round_1b/evaluation_metrics_python2.py')

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

###################


leaderboard_needsbayes <- leaderboard %>%
  slice(1:66) %>%
  group_by(submitterId) %>%
  add_tally() %>%
  ungroup() %>%
  filter(n > 1) %>%
  select(-n) %>%
  group_by(submitterId) %>%
  nest()

back_calculate_bayes <- function(df){
  temp <- df

  for(i in 1:nrow(temp)){
    if(i == 1){
      print(temp$id[1])
      best <- syn$get(temp$id[1])$path
      res <- bootLadderBoot(predictions = best,
                            predictionColname = "pKd_.M._pred",
                            goldStandard = gs,
                            goldStandardColname = "pKd_.M.",
                            scoreFun = spearman_py,
                            bootstrapN = 10000,
                            reportBootstrapN = 10,
                            bayesThreshold = 3,
                            seed = 98121,
                            largerIsBetter = TRUE,
                            verbose = TRUE,
                            doParallel = TRUE)
      temp$bayes[1] <- NA
    }else{
      print(temp$id[i])
      curr <- syn$get(temp$id[i])$path
      res <- bootLadderBoot(predictions = curr,
                            predictionColname = "pKd_.M._pred",
                            goldStandard = gs,
                            goldStandardColname = "pKd_.M.",
                            prevPredictions = best,
                            scoreFun = spearman_py,
                            bootstrapN = 10000,
                            reportBootstrapN = 10,
                            bayesThreshold = 3,
                            seed = 98121,
                            largerIsBetter = TRUE,
                            verbose = TRUE,
                            doParallel = TRUE)
      temp$bayes[i] <- res$bayes
      if(res$metCutoff==TRUE){
        best <- curr
      }
    }
  }
  return(temp)
}

all_bayes <- purrr::map(leaderboard_needsbayes$data, back_calculate_bayes)
names(all_bayes) <- leaderboard_needsbayes$submitterId
all_bayes_df <- bind_rows(all_bayes, .id = "submitterId") %>%
  mutate(submitterId = as.numeric(submitterId))

leaderboard_updated <- leaderboard %>%
  filter(!objectId %in% all_bayes_df$objectId) %>%
  bind_rows(all_bayes_df) %>%
  arrange(objectId)

gold <- syn$get(gs)$path %>% read.csv()

calculate_exact_scores <- function(submissionId){

  pred <-syn$get(submissionId)$path %>% read.csv()
  auc <- auc_py(gold$pKd_.M.,pred$pKd_.M._pred)
  ci <- ci_py(gold$pKd_.M.,pred$pKd_.M._pred)
  f1 <- f1_py(gold$pKd_.M.,pred$pKd_.M._pred)
  pea <- pearson_py(gold$pKd_.M.,pred$pKd_.M._pred)
  spe <- spearman_py(gold$pKd_.M.,pred$pKd_.M._pred)
  rms <- rmse_py(gold$pKd_.M.,pred$pKd_.M._pred)

  return(c('id' = submissionId, 'avg_auc_actual' = auc, 'ci_actual' = ci,
           'f1_actual' = f1, 'pearson_actual' = pea, 'spearman_actual' = spe,
           'rmse_actual' = rms))
}


exact_scores <- sapply(leaderboard$id, calculate_exact_scores)

exact_scores_num <- exact_scores %>% t() %>% as.data.frame() %>%
  mutate_at(c('avg_auc_actual', 'ci_actual', 'f1_actual',
              'pearson_actual', 'spearman_actual', 'rmse_actual'), as.character) %>%
  mutate_at(c('avg_auc_actual', 'ci_actual', 'f1_actual',
              'pearson_actual', 'spearman_actual', 'rmse_actual'), as.numeric)

full_leaderboard <- full_join(leaderboard_updated, exact_scores_num) %>% arrange(createdOn)

synapse$table$build_table("Round 2 Leaderboard Final", "syn15667962", full_leaderboard) %>% syn$store()



