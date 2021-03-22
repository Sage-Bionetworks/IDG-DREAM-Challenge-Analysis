source('R/bootstrap.R')
library(reticulate)
library(tidyverse)
library(doMC)
doMC::registerDoMC(cores = detectCores())

use_python("/usr/local/bin/python2")
synapse <- import("synapseclient")
syn <- synapse$Synapse()
synutils <- synapse$utils
syn$login()
source_python('https://raw.githubusercontent.com/Sage-Bionetworks/IDG-DREAM-Challenge-Analysis/master/round_1b/evaluation_metrics_python2.py?token=AE3WNSGB6AYUBWIOM75LP4S4ZB2WK')

spearman_py <- function(gold, pred){
  gold_py <- gold %>% np_array()
  pred_py <- pred %>% np_array()
  spearman(gold_py, pred_py)
}

rmse_py <- function(gold, pred){
  gold_py <- gold %>% np_array()
  pred_py <- pred %>% np_array()
  rmse(gold_py, pred_py)
}

fv <- syn$tableQuery("select id, submissionId AS objectId, teamId, userId from syn18513076")$filepath %>%
  read_csv()

leaderboard <- read_csv(syn$get("syn18520916")$path) %>% full_join(fv)

get_path <- function(id){
  syn$get(id)$path
}

gold <- read_csv(syn$get("syn18421225")$path)

disqualified_submissions <- c("9686285")

####SPEARMAN ANALYSIS

best_spearman_by_team <- leaderboard %>%
  group_by(submitterId) %>%
  top_n(1, spearman) %>%
  ungroup() %>%
  arrange(-spearman) %>%
  filter(!objectId %in% disqualified_submissions)

paths <- map_chr(best_spearman_by_team$id, get_path)

best_path <- paths[1]
other_paths <- paths[-1]

results_spearman <- finalScoring(predictions = other_paths,
             predictionColname = 'pKd_[M]_pred',
             predictionIds = best_spearman_by_team$objectId[-1],
             goldStandard = gold,
             goldStandardColname = 'pKd_[M]',
             bestPrediction = best_path,
             bestPredictionId = best_spearman_by_team$objectId[1],
             keyColumns = colnames(gold)[1:6],
             doParallel = TRUE,
             scoreFun = spearman_py)

tidy_bayes <- tibble('objectId' = colnames(results_spearman$bootstrappedScores),
                         'bayes' = results_spearman$bayes)

# tidy_bayes$bayes[tidy_bayes$objectId == 9686285] <- 0

tidy_res <- results_spearman$bootstrappedScores %>%
  as.data.frame %>%
  tidyr::gather(objectId, bootstrappedScore) %>%
  left_join(tidy_bayes) %>%
  left_join(leaderboard %>% mutate(objectId = as.character(objectId)))


ggplot(data = tidy_res) +
  geom_boxplot(aes(x = reorder(objectId, bootstrappedScore, fun = median), y = bootstrappedScore,
                   color = cut(bayes, c(-Inf, 3, 5,Inf))), outlier.shape = NA)+
  coord_flip() +
  scale_color_manual(values = c("#30C31E","#FFCA3E","#FF3E3E"),
                     name = "Bayes Factor",
                     labels = c("<3","3-5",">5")) +
  labs(x = "Submission", y = "Bootstrapped Spearman") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

ggplot(data = tidy_res %>% filter(bayes <20)) +
  geom_boxplot(aes(x = reorder(objectId, bootstrappedScore, fun = median), y = bootstrappedScore,
                   color = cut(bayes, c(-Inf, 3, 5,Inf))), outlier.shape = NA)+
  coord_flip() +
  geom_label(data = tidy_res %>% select(objectId, bayes, average_auc) %>%
               distinct %>%
               filter(bayes <5),
             aes(x = objectId, label = paste0("AUC: ", round(average_auc, 3))), y = 0.35) +
  scale_color_manual(values = c("#30C31E","#FFCA3E","#FF3E3E"),
                     name = "Bayes Factor",
                     labels = c("<3","3-5",">5")) +
  labs(x = "Submission", y = "Bootstrapped Spearman") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))


####RMSE ANALYSIS

best_rmse_by_team <- leaderboard %>%
  group_by(submitterId) %>%
  top_n(1, -rmse) %>%
  ungroup() %>%
  arrange(rmse)

paths <- map_chr(best_rmse_by_team$id, get_path)

best_path <- paths[1]
other_paths <- paths[-1]

results_rmse <- finalScoring(predictions = other_paths,
                        predictionColname = 'pKd_[M]_pred',
                        predictionIds = best_rmse_by_team$objectId[-1],
                        goldStandard = gold,
                        goldStandardColname = 'pKd_[M]',
                        bestPrediction = best_path,
                        bestPredictionId = best_rmse_by_team$objectId[1],
                        keyColumns = colnames(gold)[1:6],
                        doParallel = TRUE,
                        scoreFun = rmse_py,
                        largerIsBetter = F)

tidy_bayes <- tibble('objectId' = colnames(results_rmse$bootstrappedScores),
                     'bayes' = results_rmse$bayes)

tidy_res_rmse <- results_rmse$bootstrappedScores %>%
  as.data.frame %>%
  tidyr::gather(objectId, bootstrappedScore) %>%
  left_join(tidy_bayes) %>%
  left_join(leaderboard %>% mutate(objectId = as.character(objectId)))

ggplot(data = tidy_res_rmse) +
  geom_boxplot(aes(x = reorder(objectId, -bootstrappedScore, fun = median), y = log10(bootstrappedScore),
                   color = cut(bayes, c(-Inf, 3, 5, 20, Inf))), outlier.shape = NA)+
  coord_flip() +
  scale_color_manual(values = c("#30C31E","#FFCA3E","#FF3E3E",'#000000'),
                     name = "Bayes Factor",
                     labels = c("<3","3-5",">5")) +
  labs(x = "Submission", y = "log10(Bootstrapped RMSE)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

ggplot(data = tidy_res_rmse %>% filter(bayes <20)) +
  geom_boxplot(aes(x = reorder(objectId, -bootstrappedScore, fun = median), y = log10(bootstrappedScore),
                   color = cut(bayes, c(-Inf, 3, 5, 20, Inf))), outlier.shape = NA)+
  coord_flip() +
  geom_label(data = tidy_res_rmse %>% select(objectId, bayes, average_auc) %>%
               distinct %>%
               filter(bayes <5),
             aes(x = objectId, label = paste0("AUC: ", round(average_auc, 3))), y = 0.0050) +
  scale_color_manual(values = c("#30C31E","#FFCA3E","#FF3E3E",'#000000'),
                     name = "Bayes Factor",
                    labels = c("<3","3-5",">5")) +
  labs(x = "Submission", y = "log10(Bootstrapped RMSE)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))



###comparison of rounds

r1b <- syn$tableQuery('select * from syn18487972')$asDataFrame()
r1 <- syn$tableQuery('SELECT * FROM syn17054253')$asDataFrame()

length(unique(leaderboard$submitterId[leaderboard$submitterId %in% c(r1b$submitterId)]))
length(unique(leaderboard$submitterId[!leaderboard$submitterId %in% c(r1b$submitterId)]))


length(unique(leaderboard$submitterId[leaderboard$userId %in% c(r1$userId)]))
length(unique(leaderboard$submitterId[!leaderboard$userId %in% c(r1$userId)]))

