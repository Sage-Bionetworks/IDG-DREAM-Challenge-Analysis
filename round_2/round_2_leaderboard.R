library(synapser)
synLogin()
# using challengeutils query 'select createdOn, name, objectId, rmse, spearman, average_auc, submitterId, team, userId, from evaluation_9614079 where prediction_file_status == "SCORED"'

leaderboard <- read_csv('round_2_leaderboard.csv')

synStore(File('round_2_leaderboard.csv', parentId = ), executed = this.file)