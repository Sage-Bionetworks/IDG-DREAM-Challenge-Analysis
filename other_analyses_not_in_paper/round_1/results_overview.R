setwd("/Users/cichonsk/Documents/PhD_FIMM_HIIT/DREAM/Round1_results")

results = as.matrix(read.table("results_round1_updatedCI.tsv", header = TRUE, sep = "\t",
                               comment.char = "", as.is = TRUE))
n_submissions = dim(results)[1]

baseline_results = t(as.matrix(results[which(results[,"objectId"] == "9681691"),]))



# RMSE
rmse_all = results[,"rmse"]
rmse_below100 = rmse_all[which(rmse_all < 100)]
rmse_below2 = rmse_all[which(rmse_all < 2)]

pdf("figures/rmse_all.pdf")
hist(rmse_all, main = "Results of Round 1 (170 submissions)", 
     xlim = c(round(min(rmse_all)),round(max(rmse_all)/1000)*1000), ylim = c(0,200),
     xlab = "RMSE", ylab = "Number of submissions", cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5,
     col = "mediumpurple3")
dev.off()

pdf("figures/rmse_below2.pdf")
hist(rmse_below2, main = "Results of Round 1\n (146 submissions with RMSE below 2.0)", 
     ylim = c(0,50),
     xlab = "RMSE", ylab = "Number of submissions", cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5,
     col = "lavender")
abline(v = baseline_results[,"rmse"], col="red", lwd=2)
mtext(baseline_results[,"rmse"], side = 1, col = "red", at = baseline_results[,"rmse"], font = 2, cex=1.25)
text(baseline_results[,"rmse"]+0.09, 45, "baseline",  cex = 1.3, col = "red")
text(1.8, 30, sprintf("best: %s\nuser: noname zhang\nteam: -", min(rmse_all)), cex = 1.3)
dev.off()


# Pearson correlation
pdf("figures/pearson.pdf")
hist(results[,"pearson"], main = "Results of Round 1 (170 submissions)", 
     xlim = c(-0.4, 0.8), ylim = c(0,50),
     xlab = "Pearson correlation", ylab = "Number of submissions", cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5,
     col = "cornflowerblue")
abline(v = baseline_results[,"pearson"], col="red", lwd=2)
mtext(baseline_results[,"pearson"], side = 1, col = "red", at = baseline_results[,"pearson"], font = 2, cex=1.25)
text(baseline_results[,"pearson"]+0.11, 40, "baseline",  cex = 1.3, col = "red")
text(0.67, 22, sprintf("best: %s\nuser: Junqiu Wu\nteam: -", max(results[,"pearson"])), cex = 1.3)
dev.off()


# Spearman correlation
pdf("figures/spearman.pdf")
hist(results[,"spearman"], main = "Results of Round 1 (170 submissions)", 
     xlim = c(-0.4, 0.8), ylim = c(0,50),
     xlab = "Spearman correlation", ylab = "Number of submissions", cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5,
     col = "mediumseagreen")
abline(v = baseline_results[,"spearman"], col="red", lwd=2)
mtext(baseline_results[,"spearman"], side = 1, col = "red", at = baseline_results[,"spearman"], font = 2, cex=1.25)
text(baseline_results[,"spearman"]+0.11, 40, "baseline",  cex = 1.3, col = "red")
text(0.67, 22, sprintf("best: %s\nuser: Junqiu Wu\nteam: -", max(results[,"spearman"], na.rm = TRUE)), cex = 1.3)
dev.off()


# Concordance index
pdf("figures/ci.pdf")
hist(results[,"ci"], main = "Results of Round 1 (170 submissions)", 
     xlim = c(0.4, 0.8), ylim = c(0,50),
     xlab = "CI", ylab = "Number of submissions", cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5,
     col = "paleturquoise3")
abline(v = baseline_results[,"ci"], col="red", lwd=2)
mtext(baseline_results[,"ci"], side = 1, col = "red", at = baseline_results[,"ci"], font = 2, cex=1.25)
text(baseline_results[,"ci"]+0.04, 47, "baseline",  cex = 1.3, col = "red")
text(0.74, 25, sprintf("best: %s\nuser: Junqiu Wu\nteam: -", max(results[,"ci"], na.rm = TRUE)), cex = 1.3)
dev.off()


# F1 score
pdf("figures/f1.pdf")
hist(results[,"f1"], main = "Results of Round 1 (170 submissions)", 
     xlim = c(0, 0.6), ylim = c(0,50),
     xlab = "F1 score (using interaction threshold of 7M)", ylab = "Number of submissions", cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5,
     col = "lemonchiffon2")
abline(v = baseline_results[,"f1"], col="red", lwd=2)
mtext(baseline_results[,"f1"], side = 1, col = "red", at = baseline_results[,"f1"], font = 2, cex=1.25)
text(baseline_results[,"f1"]+0.06, 40, "baseline",  cex = 1.3, col = "red")
text(0.52, 40, sprintf("best: %s\nuser: Junqiu Wu\nteam: -", max(results[,"f1"], na.rm = TRUE)), cex = 1.3)
dev.off()


# Average AUC
pdf("figures/average_auc.pdf")
hist(results[,"average_AUC"], main = "Results of Round 1 (170 submissions)", 
     xlim = c(0.3, 0.9), ylim = c(0,50),
     xlab = "Average AUC", ylab = "Number of submissions", cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5,
     col = "lightgoldenrod1")
abline(v = baseline_results[,"average_AUC"], col="red", lwd=2)
mtext(baseline_results[,"average_AUC"], side = 1, col = "red", at = baseline_results[,"average_AUC"], font = 2, cex=1.25)
text(baseline_results[,"average_AUC"]+0.06, 40, "baseline",  cex = 1.3, col = "red")
text(0.82, 23, sprintf("best: %s\nuser: Junqiu Wu\nteam: -", max(results[,"average_AUC"], na.rm = TRUE)), cex = 1.3)
dev.off()

