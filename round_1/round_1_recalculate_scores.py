import pandas as pd
from pandas import DataFrame
import evaluation_metrics_python2 as eval
import multiprocessing
import synapseclient
syn = synapseclient.Synapse()
syn.login()

goldstandard_path = syn.get("syn16809884").path
gs_df = pd.read_csv(goldstandard_path)

def score1(sub_file_id):
    path = syn.get(sub_file_id).path
    sub_df = pd.read_csv(path)
    combined_df = pd.merge(sub_df, gs_df, how='inner')
    actual = combined_df["pKd_[M]"]
    predicted = combined_df["pKd_[M]_pred"]
    rmse = round(eval.rmse(actual, predicted), 4)
    pearson = round(eval.pearson(actual, predicted), 4)
    spearman = round(eval.spearman(actual, predicted), 4)
    ci = round(eval.ci(actual.values, predicted.values), 4)
    f1 = round(eval.f1(actual, predicted), 4)
    average_AUC = round(eval.average_AUC(actual, predicted), 4)
    return(rmse, pearson, spearman, ci, f1, average_AUC)


fv = syn.tableQuery("select id, name from syn17051994").asDataFrame()

result = map(score1, fv.id.values)

csvfile = "recalculated_metrics.csv"

df = DataFrame.from_records(result, columns=['rmse','pearson', 'spearman', 'ci', 'f1', 'average_AUC'])
df['id'] = fv.id.values

df.to_csv(csvfile, encoding='utf-8', index=False)