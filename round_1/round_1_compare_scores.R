library(tidyverse)
evaluation_id <- 9614078L
synapser::synLogin()

submitted_fv <- synTableQuery("select * from syn17051994", includeRowIdAndRowVersion = F)$asDataFrame()

original_metrics <- read.csv('r1_final_results.csv') %>%
  filter(status == "SCORED") %>% 
  select(objectId, ci, spearman, f1, average_AUC, pearson, rmse) %>% 
  set_names(c("submissionId","ci_orig", "spearman_orig", "f1_orig", "average_AUC_orig", "pearson_orig", "rmse_orig"))

recalculated_metrics <- read.csv('recalculated_metrics.csv')

dat <- full_join(submitted_fv, recalculated_metrics) %>% 
  full_join(original_metrics) %>% 
  mutate(spearman_orig = as.numeric(gsub("NaN", NA, spearman_orig))) ##challenge data had NaNs instead of NAs

##quick check to ensure all metrics were recalculated the same way
identical(dat$rmse, dat$rmse_orig)
identical(dat$pearson, dat$pearson_orig)
identical(dat$rmse, dat$rmse_orig)

##spearman appears to be slightly different in a few cases - 
#must be a difference in libraries. they are quite similar though.
#for consistency we will use the original values from scoring harness
identical(dat$spearman, dat$spearman_orig) 

##ci had a bug during round 1, so this will not be cor=1
cor(dat$ci, dat$ci_orig, method = "spearman")

ggplot(dat) +
  geom_point(aes(x = ci, y = ci_orig)) + 
  coord_fixed(ratio=1)



