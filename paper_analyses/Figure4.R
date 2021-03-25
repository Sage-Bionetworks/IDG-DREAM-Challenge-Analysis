# Install required libraries
required_lib <- c("tidyverse")
for (package in required_lib){
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, quiet = FALSE)
  }
}

# Load tidyverse library
library(tidyverse)


# Read data ---------------------------------------------------------------------------------------------
df <- 
  readr::read_csv(
    "~/source_data_figures/Fig4/Fig4.csv",
    col_names = TRUE
  )


# Set parameters for the figures ------------------------------------------------------------------------ 
size_legend <-  2.9
size_text <- 10
size_tag <- 12
legend_xpos <- 5.05
legend_ypos <- 1.6


# Figure 4a ---------------------------------------------------------------------------------------------
# Spearman correlation sub-challenge top performer in Round 2 (Q.E.D)
ggplot(
  data = df %>% dplyr::filter(.data$figure_panels == "a, b, c"), 
  aes(x = pKd_true, y = pKd_pred_QED)
  ) + 
  geom_point(
    alpha = 0.4,
    size = 2,
    shape = 16,
    color = rgb(0.2, 0.2, 0.2)
  ) +
  labs(
    x = expression("Measured pK"[d]),
    y = expression("Predicted pK"[d]),
    tag = "a"
  ) + 
  annotate(
    geom = "text", 
    label = paste0(
      "RMSE = ", 
      (df$pKd_true - df$pKd_pred_QED)^2 %>% mean(na.rm = TRUE) %>% sqrt() %>% round(digits = 3),
      "\n",
      "Spearman = ",
      cor(df$pKd_true, df$pKd_pred_QED, method = "spearman", use = "complete.obs") %>% round(digits = 3)
    ),
    x = legend_xpos, 
    y = Inf, 
    hjust = 0, 
    vjust = legend_ypos, 
    size = size_legend
  ) +
  coord_equal() + 
  xlim(5,10) +
  ylim(5,10) + 
  geom_abline(
    intercept = 0, 
    slope = 1, 
    color = "gray"
  ) +
  theme_bw() + 
  theme(
    plot.title = element_blank(),
    axis.text = element_text(size = size_text), 
    axis.title = element_text(size = size_text),
    plot.tag = element_text(size = size_tag, face = "bold"),
    legend.text = element_text(size = size_legend)
  ) 


# Figure 4b ---------------------------------------------------------------------------------------------
# RMSE sub-challenge top performer in Round 2 (AI Winter is Coming)
ggplot(
  data = df %>% dplyr::filter(.data$figure_panels == "a, b, c"), 
  aes(x = pKd_true, y = pKd_pred_AIWiC)
  ) + 
  geom_point(
    alpha = 0.4,
    size = 2,
    shape = 16,
    color = rgb(0.2, 0.2, 0.2)
  ) +
  labs(
    x = expression("Measured pK"[d]),
    y = expression("Predicted pK"[d]),
    tag = "b"
  ) + 
  annotate(
    geom = "text", 
    label = paste0(
      "RMSE = ", 
      (df$pKd_true - df$pKd_pred_AIWiC)^2 %>% mean(na.rm = TRUE) %>% sqrt() %>% round(digits = 3),
      "\n",
      "Spearman = ",
      cor(df$pKd_true, df$pKd_pred_AIWiC, method = "spearman", use = "complete.obs") %>% round(digits = 3)
    ),
    x = legend_xpos, 
    y = Inf, 
    hjust = 0, 
    vjust = legend_ypos, 
    size = size_legend
  ) +
  coord_equal() + 
  xlim(5,10) +
  ylim(5,10) + 
  geom_abline(
    intercept = 0, 
    slope = 1, 
    color = "gray"
  ) +
  theme_bw() + 
  theme(
    plot.title = element_blank(),
    axis.text = element_text(size = size_text), 
    axis.title = element_text(size = size_text),
    plot.tag = element_text(size = size_tag, face = "bold"),
    legend.text = element_text(size = size_legend)
  ) 


# Figure 4c ---------------------------------------------------------------------------------------------
# Ensemble model that combines the top four models selected based on their Spearman correlation in Round 2
ggplot(
  data = df %>% dplyr::filter(.data$figure_panels == "a, b, c"), 
  aes(x = pKd_true, y = pKd_pred_top_4_ensemble)
  ) + 
  geom_point(
    alpha = 0.4,
    size = 2,
    shape = 16,
    color = rgb(0.2, 0.2, 0.2)
  ) +
  labs(
    x = expression("Measured pK"[d]),
    y = expression("Predicted pK"[d]),
    tag = "c"
  ) + 
  annotate(
    geom = "text", 
    label = paste0(
      "RMSE = ", 
      (df$pKd_true - df$pKd_pred_top_4_ensemble)^2 %>% mean(na.rm = TRUE) %>% sqrt() %>% round(digits = 3),
      "\n",
      "Spearman = ",
      cor(df$pKd_true, df$pKd_pred_top_4_ensemble, method = "spearman", use = "complete.obs") %>% round(digits = 3)
    ),
    x = legend_xpos, 
    y = Inf, 
    hjust = 0, 
    vjust = legend_ypos, 
    size = size_legend
  ) +
  coord_equal() + 
  xlim(5,10) +
  ylim(5,10) + 
  geom_abline(
    intercept = 0, 
    slope = 1, 
    color = "gray"
  ) +
  theme_bw() + 
  theme(
    plot.title = element_blank(),
    axis.text = element_text(size = size_text), 
    axis.title = element_text(size = size_text),
    plot.tag = element_text(size = size_tag, face = "bold"),
    legend.text = element_text(size = size_legend)
  ) 


# Figure 4d ---------------------------------------------------------------------------------------------
# The mean aggregation ensemble mode
scal_fact <- 1.6
ggplot(
  data = df %>% 
    dplyr::filter(grepl("*spearman_mean*", ensemble_model)) %>% 
    dplyr::filter(.data$ensemble_score > 0.45)
  ) +
  geom_line(
    data = df %>% 
      dplyr::filter(grepl("*rmse_mean*", ensemble_model)) %>% 
      dplyr::filter(.data$ensemble_score < 2),
    aes(
      x = ensemble_iteration, 
      y = ensemble_score/scal_fact, 
      linetype = ensemble_model_type)
  ) +
  geom_line(
    aes(
      x = ensemble_iteration, 
      y = ensemble_score, 
      linetype = ensemble_model_type
    )
  ) +
  geom_point(
    data = df %>% 
      dplyr::filter(grepl("*spearman_mean*", ensemble_model)) %>% 
      dplyr::filter(.data$ensemble_score > 0.45) %>% 
      dplyr::filter(!is.na(.data$ensemble_label)),
    aes(
      x = ensemble_iteration, 
      y = ensemble_score, 
      color = forcats::fct_inorder(ensemble_label)
    ), 
    size = 2
  ) +
  geom_point(
    data = df %>% 
      dplyr::filter(grepl("*rmse_mean*", ensemble_model)) %>% 
      dplyr::filter(ensemble_score < 2) %>% 
      dplyr::filter(!is.na(ensemble_label)),
    aes(
      x = ensemble_iteration, 
      y = ensemble_score/scal_fact, 
      color = forcats::fct_inorder(ensemble_label)
    ), 
    size = 2
  ) +
  scale_color_manual(
    values = c("#86180F", "#016DDC", "#009e73", "#e69f00"), 
    name = "",
    labels = c("Q.E.D", "+ Gregory Koytiger", "+ AI Winter is Coming", "+ Oliver Labayle")
  ) +
  scale_linetype_manual(
    "",
    values = c("RMSE" = 2, "Spearman" = 1)
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(~.*scal_fact, name = "RMSE")
  ) +
  labs(
    x = "Number of top models", 
    y = "Spearman correlation",
    tag = "d"
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = size_text), 
    axis.title = element_text(size = size_text),
    plot.tag = element_text(size = size_tag,  face = "bold")
    # legend.position = "none"
  )
