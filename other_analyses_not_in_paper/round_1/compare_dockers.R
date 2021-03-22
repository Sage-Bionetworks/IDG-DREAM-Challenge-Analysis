# library(challengescoring)
library(synapser)
library(tidyverse)
# library(doMC)
# doMC::registerDoMC(cores = detectCores()-1)

synLogin()
# source_python('https://raw.githubusercontent.com/Sage-Bionetworks/IDG-DREAM-Challenge-Analysis/master/round_1b/evaluation_metrics_python2.py?token=AE3WNSGZXHMQD6TMZJ7Q5US44V3AG')

# spearman_py <- function(gold, pred){
#   gold_py <- gold %>% np_array()
#   pred_py <- pred %>% np_array()
#   spearman(gold_py, pred_py)
# }
# 
# rmse_py <- function(gold, pred){
#   gold_py <- gold %>% np_array()
#   pred_py <- pred %>% np_array()
#   rmse(gold_py, pred_py)
# }

fv <- synTableQuery("select id, submissionId AS objectId, teamId, userId from syn18513076")$filepath %>%
  read_csv()

leaderboard <- read_csv(synGet("syn18520916")$path) %>% full_join(fv)

get_path <- function(id){
  synGet(id)$path
}

# gold <- read_csv(syn$get("syn18421225")$path)


best_spearman_by_team <- leaderboard %>%
  group_by(submitterId) %>%
  top_n(1, spearman) %>%
  ungroup() %>%
  arrange(-spearman)

qed_pred_sub <- map_chr(best_spearman_by_team$id[1], get_path) %>% read_csv() %>% 
  mutate(qed_sub = `pKd_[M]_pred`, `pKd_[M]_pred` = NULL)

qed_pred <- read_csv(synGet("syn18700960")$path) %>% 
  mutate(qed_dock = `pKd_[M]_pred`, `pKd_[M]_pred` = NULL) %>% 
  full_join(qed_pred_sub)

ggplot(qed_pred) + 
  geom_point(aes(x = qed_sub, y = qed_dock))

mean(qed_pred$qed_dock-qed_pred$qed_sub)

best_rmse_by_team <- leaderboard %>%
  filter(objectId == 9686330)

dmis_pred_sub <- map_chr(best_rmse_by_team$id, get_path) %>% read_csv() %>% 
  mutate(dmis_sub = `pKd_[M]_pred`, `pKd_[M]_pred` = NULL) 

dmis_pred <- read_csv(synGet("syn18700968")$path) %>% 
  mutate(dmis_dock = `pKd_[M]_pred`, `pKd_[M]_pred` = NULL) %>% 
  full_join(dmis_pred_sub)

ggplot(dmis_pred) + 
  geom_point(aes(x = dmis_sub, y = dmis_dock))

mean(dmis_pred$dmis_dock-dmis_pred$dmis_sub)


#aiwic
best_rmse_by_team <- leaderboard %>%
  filter(objectId == 9686282)

aiwic_pred_sub <- map_chr(best_rmse_by_team$id, get_path) %>% read_csv() %>% 
  mutate(aiwic_sub = `pKd_[M]_pred`, `pKd_[M]_pred` = NULL) 

aiwic_pred <- read_csv(synGet("syn18707968")$path) %>% 
  mutate(aiwic_dock = `pKd_[M]_pred`, `pKd_[M]_pred` = NULL) %>% 
  full_join(aiwic_pred_sub)

ggplot(aiwic_pred) + 
  geom_point(aes(x = aiwic_sub, y = aiwic_dock)) +
  ggrepel::geom_label_repel(data = aiwic_pred %>% filter(aiwic_dock>aiwic_sub+0.05),
             aes(x= aiwic_sub, y = aiwic_dock, label = Entrez_Gene_Symbol))

glist <- c("MAPK14", "KDR", "PLK1", "SYK", "MTOR", "TEK")

ggplot(aiwic_pred) + 
  geom_point(aes(x = aiwic_sub, y = aiwic_dock)) +
  ggrepel::geom_label_repel(data = aiwic_pred %>% filter(Entrez_Gene_Symbol %in% glist),
                            aes(x= aiwic_sub, y = aiwic_dock, label = Entrez_Gene_Symbol)) +
  labs(x = "AI Winter is Coming R2", y = "AI Winter is Coming py script")

mean(aiwic_pred$aiwic_dock-aiwic_pred$aiwic_sub)


#r1_template <- synGet("syn16809883")$path %>% read.csv() 

r2_template <- synGet("syn16809885")$path %>% read.csv() 

# all_template <- bind_rows(r1_template,r2_template)

all_template <- r2_template
compounds <- select(all_template, Compound_SMILES, Compound_InchiKeys, Compound_Name) %>% distinct()

kinases <- select(all_template, UniProt_Id, Entrez_Gene_Symbol, DiscoveRx_Gene_Symbol) %>% distinct()

mat <- matrix(1, nrow=length(unique(compounds$Compound_SMILES)), ncol=length(unique(kinases$UniProt_Id)))
colnames(mat) <- unique(kinases$UniProt_Id)
rownames(mat) <- unique(compounds$Compound_SMILES)
mat <- as.data.frame(mat) %>% rownames_to_column("Compound_SMILES") %>% gather(-Compound_SMILES, key = UniProt_Id, value = foo) %>% select(-foo)

pairs <- mat %>% left_join(compounds) %>% left_join(kinases) %>% select(colnames(all_template)) %>% distinct()

pairwise_from_guru <- synGet("syn18774741")$path %>% read.csv() 


write_csv(pairs, "pairwise_r2.csv")
synStore(File("pairwise_r2.csv", parentId="syn15667963"))
