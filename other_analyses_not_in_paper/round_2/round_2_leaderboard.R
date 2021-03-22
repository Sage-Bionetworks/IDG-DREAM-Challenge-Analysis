library(synapser)
synLogin()
# using challengeutils query 'select createdOn, name, objectId, rmse, spearman, average_auc, submitterId, team, userId, from evaluation_9614079 where prediction_file_status == "SCORED"'

leaderboard <- read_csv('round_2_leaderboard.csv')

this.file <- 'https://raw.githubusercontent.com/Sage-Bionetworks/IDG-DREAM-Challenge-Analysis/master/round_2/round_2_leaderboard.R'

synStore(File('round_2_leaderboard.csv', parentId = 'syn15667963'), executed = this.file)
