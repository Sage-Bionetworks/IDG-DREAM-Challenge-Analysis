# Install required libraries
required_lib <- c("tidyverse", "plyr", "beeswarm")
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
    "~/source_data_figures/Fig3/Fig3.csv",
    col_names = TRUE
  ) %>% 
  dplyr::filter(
    figure_panels == "a, c, e, f"
  ) %>% 
  dplyr::select(
    -.data$figure_panels,
    -(.data$spearman_random_predictions:.data$rmse_replicates)
  )


# Set parameters for the figures ----------------------------------------------------------------------- 
size_text <- 10
size_tag <- 12
main_point_size <- 1.2
highlight_point_size <- 1.9
stroke_size <- 0.7
stroke_size_highlight <- 1.3


# Figure 3a --------------------------------------------------------------------------------------------
# Create a nice layout
beeswarm_spearman <- 
  beeswarm::beeswarm(
    spearman ~ round, 
    data = df, 
    method = "swarm",
    pwcol = factor(submitter_id),  # keep submitter_id column in the beeswarm data frame
    pwbg = factor(object_id),      # keep object_id column in the beeswarm data frame
    spacing = 1.15
  ) 

spearman_median <- 
  purrr::map_dfr(
    c("Round 1", "Round 2"),
    ~tibble::tribble(
      ~round, 
      ~median,
      .x,
      df %>% 
        dplyr::filter(.data$round == .x) %>% 
        dplyr::pull(.data$spearman) %>% 
        median(na.rm = TRUE)
    )
  )

ggplot(data = beeswarm_spearman, aes(x, y)) +
  # Median line Round 1
  geom_segment(
    x = beeswarm_spearman %>% dplyr::filter(.data$x.orig == "Round 1") %>% dplyr::select(.data$x) %>% min() - 0.05,
    y = spearman_median %>% dplyr::filter(.data$round == "Round 1") %>% dplyr::pull(.data$median), 
    xend = beeswarm_spearman %>% dplyr::filter(.data$x.orig == "Round 1") %>% dplyr::select(.data$x) %>% max() + 0.05,
    yend = spearman_median %>% dplyr::filter(.data$round == "Round 1") %>% dplyr::pull(.data$median),
    color = rgb(0.25, 0.25, 0.25),
    size = 0.7
  ) +
  # Median line Round 2
  geom_segment(
    x = beeswarm_spearman %>% dplyr::filter(.data$x.orig == "Round 2") %>% dplyr::select(.data$x) %>% min() - 0.05,
    y = spearman_median %>% dplyr::filter(.data$round == "Round 2") %>% dplyr::pull(.data$median), 
    xend = beeswarm_spearman %>% dplyr::filter(.data$x.orig == "Round 2") %>% dplyr::select(.data$x) %>% max() + 0.05,
    yend = spearman_median %>% dplyr::filter(.data$round == "Round 2") %>% dplyr::pull(.data$median),
    color = rgb(0.25, 0.25, 0.25),
    size = 0.7
  ) +
  # Submissions better than random
  geom_point(
    data = beeswarm_spearman %>% 
      dplyr::filter(
        .data$bg %in% (df %>% dplyr::filter(.data$spearman_better_than_random == "yes") %>% dplyr::pull(.data$object_id))
      ),
    aes(x = x, y = y, colour = x.orig), 
    size = main_point_size
  ) +
  # Submissions not better than random
  geom_point(
    data = beeswarm_spearman %>% 
      dplyr::filter(
        .data$bg %in% (df %>% dplyr::filter(.data$spearman_better_than_random == "no") %>% dplyr::pull(.data$object_id))
      ),
    aes(x = x, y = y, colour = x.orig), 
    size = main_point_size - 0.05,
    shape = 1,
    stroke = stroke_size
  ) +
  scale_x_continuous(
    breaks = c(1:2), 
    labels = c("Round 1", "Round 2"), 
    expand = c(0, 0.2)
  ) + 
  scale_color_manual(
    values = c(
      rgb(0.59, 0.59, 0.59),
      rgb(0.59, 0.59, 0.59)
    )
  ) +
  ylim(-0.3, 0.6) +
  # Baseline model in Round 1
  geom_point(
    data = beeswarm_spearman %>% dplyr::filter(.data$col == "3379336", .data$x.orig == "Round 1"),
    aes(x, y), 
    colour = "#e50000", 
    size = highlight_point_size
  ) +
  # Baseline model in Round 2
  geom_point(
    data = beeswarm_spearman %>% dplyr::filter(.data$col == "3379336", .data$x.orig == "Round 2"),
    aes(x, y), 
    colour = "#e50000", 
    size = highlight_point_size
  ) +
  # - - - 
  # Highlight best teams
  # @gregkoytiger
  geom_point(
    data = beeswarm_spearman %>% dplyr::filter(.data$x.orig == "Round 1" & .data$col == "3368024") %>% dplyr::filter(.data$y == max(y)),
    aes(x, y), 
    colour = "#00b8e6", 
    size = highlight_point_size
  ) +
  geom_point(
    data = beeswarm_spearman %>% dplyr::filter(.data$x.orig == "Round 2" & .data$col == "3368024") %>% dplyr::filter(.data$y == max(y)),
    aes(x, y), 
    colour = "#00b8e6",
    size = highlight_point_size
  ) +
  # Q.E.D
  geom_point(
    data = beeswarm_spearman %>% dplyr::filter(.data$x.orig == "Round 1" & .data$col == "3377646") %>% dplyr::filter(.data$y == max(y)),
    aes(x, y), 
    colour = "#307905",
    fill = "black",
    size = highlight_point_size
  ) +
  geom_point(
    data = beeswarm_spearman %>% dplyr::filter(.data$x.orig == "Round 2" & .data$col == "3377646") %>% dplyr::filter(.data$y == max(y)),
    aes(x, y), 
    colour = "#307905", 
    size = highlight_point_size
  ) +
  # AI Winter is Coming
  geom_point(
    data = beeswarm_spearman %>% dplyr::filter(.data$x.orig == "Round 1" & .data$col == "3332429") %>% dplyr::filter(.data$y == max(y)),
    aes(x, y), 
    colour = "#0000FF", 
    size = highlight_point_size
  ) +
  geom_point(
    data = beeswarm_spearman %>% dplyr::filter(.data$x.orig == "Round 2" & .data$col == "3332429") %>% dplyr::filter(.data$y == max(y)),
    aes(x, y), 
    colour = "#0000FF", 
    size = highlight_point_size
  ) +
  # DMIS_DK (RF)
  geom_point(
    data = beeswarm_spearman %>% dplyr::filter(.data$x.orig == "Round 2" & .data$bg == "9686312") %>% dplyr::filter(.data$y == max(y)),
    aes(x, y), 
    colour = "#f2b63d",
    size = highlight_point_size
  ) + 
  # DMIS_DK (DL)
  geom_point(
    data = beeswarm_spearman %>% dplyr::filter(.data$x.orig == "Round 2" & .data$bg == "9686330") %>% dplyr::filter(.data$y == max(y)),
    aes(x, y), 
    colour = "#ff8c40",
    size = highlight_point_size
  ) +
  # olabayle
  geom_point(
    data = beeswarm_spearman %>% dplyr::filter(.data$x.orig == "Round 2" & .data$col == "3385484") %>% dplyr::filter(.data$y == max(y)),
    aes(x, y), 
    colour = "#00d91d", 
    size = highlight_point_size
  ) +
  # Best R1—>R2 RMSE improvement team, submitter_id: 3323475, R1 + R2
  geom_point(
    data = beeswarm_spearman %>% dplyr::filter(.data$x.orig == "Round 1" & .data$col == "3323475") %>% dplyr::filter(.data$y == max(y)),
    aes(x, y), 
    colour = "#f23de6", 
    size = main_point_size,
    shape = 1,
    stroke = stroke_size_highlight
  ) +
  geom_point(
    data = beeswarm_spearman %>% dplyr::filter(.data$x.orig == "Round 2" & .data$col == "3323475") %>% dplyr::filter(.data$y == max(y)),
    aes(x, y), 
    colour = "#f23de6", 
    size = highlight_point_size
  ) +
  # - - -
  labs(
    y = "Spearman correlation", 
    tag = "a"
  ) + 
  theme_bw() + 
  theme(
    axis.text = element_text(size = size_text),
    axis.title = element_text(size = size_text),
    axis.title.x = element_blank(),
    plot.tag = element_text(size = size_tag,  face = "bold"),
    legend.position = "none"
  )


# Figure 3c --------------------------------------------------------------------------------------------
# Omit submissions with RMSE > 2
df_rmse_leq_2 <- df %>% dplyr::filter(.data$rmse <= 2)

# Create a nice layout
beeswarm_rmse <- 
  beeswarm::beeswarm(
    rmse ~ round, 
    data = df_rmse_leq_2,
    method = "swarm",
    pwcol = factor(submitter_id),  # keep submitter_id column in the beeswarm data frame
    pwbg = factor(object_id),      # keep object_id column in the beeswarm data frame
    spacing = 1.15
  ) 

rmse_median <- 
  purrr::map_dfr(
    c("Round 1", "Round 2"),
    ~tibble::tribble(
      ~round, 
      ~median,
      .x,
      df %>% 
        dplyr::filter(.data$round == .x) %>% 
        dplyr::pull(.data$rmse) %>% 
        median(na.rm = TRUE)
    )
  )

ggplot(data = beeswarm_rmse, aes(x, y)) +
  # Median line Round 1
  geom_segment(
    x = beeswarm_rmse %>% dplyr::filter(.data$x.orig == "Round 1") %>% dplyr::select(.data$x) %>% min() - 0.05,
    y = rmse_median %>% dplyr::filter(.data$round == "Round 1") %>% dplyr::pull(.data$median), 
    xend = beeswarm_rmse %>% dplyr::filter(.data$x.orig == "Round 1") %>% dplyr::select(.data$x) %>% max() + 0.05,
    yend = rmse_median %>% dplyr::filter(.data$round == "Round 1") %>% dplyr::pull(.data$median),
    color = rgb(0.25, 0.25, 0.25),
    size = 0.7
  ) +
  # Median line Round 2
  geom_segment(
    x = beeswarm_rmse %>% dplyr::filter(.data$x.orig == "Round 2") %>% dplyr::select(.data$x) %>% min() - 0.05,
    y = rmse_median %>% dplyr::filter(.data$round == "Round 2") %>% dplyr::pull(.data$median), 
    xend = beeswarm_rmse %>% dplyr::filter(.data$x.orig == "Round 2") %>% dplyr::select(.data$x) %>% max() + 0.05,
    yend = rmse_median %>% dplyr::filter(.data$round == "Round 2") %>% dplyr::pull(.data$median),
    color = rgb(0.25, 0.25, 0.25),
    size = 0.7
  ) +
  # Submissions better than random
  geom_point(
    data = beeswarm_rmse %>% 
      dplyr::filter(
        .data$bg %in% (df %>% dplyr::filter(.data$rmse_better_than_random == "yes") %>% dplyr::pull(.data$object_id))
      ),
    aes(x = x, y = y, colour = x.orig), 
    size = main_point_size
  ) +
  # Submissions not better than random
  geom_point(
    data = beeswarm_rmse %>% 
      dplyr::filter(
        .data$bg %in% (df %>% dplyr::filter(.data$rmse_better_than_random == "no") %>% dplyr::pull(.data$object_id))
      ),
    aes(x = x, y = y, colour = x.orig), 
    size = main_point_size - 0.05,
    shape = 1,
    stroke = stroke_size
  ) +
  scale_x_continuous(
    breaks = c(1:2), 
    labels = c("Round 1", "Round 2"), 
    expand = c(0, 0.3)
  ) + 
  scale_color_manual(
    values = c(
      rgb(0.59, 0.59, 0.59),
      rgb(0.59, 0.59, 0.59)
    )
  ) +
  # Baseline model in Round 1
  geom_point(
    data = beeswarm_rmse %>% dplyr::filter(.data$col == "3379336", .data$x.orig == "Round 1"),
    aes(x, y), 
    colour = "#e50000", 
    size = highlight_point_size
  ) +
  # Baseline model in Round 2
  geom_point(
    data = beeswarm_rmse %>% dplyr::filter(.data$col == "3379336", .data$x.orig == "Round 2"),
    aes(x, y), 
    colour = "#e50000", 
    size = highlight_point_size
  ) +
  # - - - 
  # Highlight best teams
  # @gregkoytiger
  geom_point(
    data = beeswarm_rmse %>% dplyr::filter(.data$x.orig == "Round 1" & .data$col == "3368024") %>% dplyr::filter(.data$y == min(y)),
    aes(x, y), 
    colour = "#00b8e6", 
    size = highlight_point_size
  ) +
  geom_point(
    data = beeswarm_rmse %>% dplyr::filter(.data$x.orig == "Round 2" & .data$col == "3368024") %>% dplyr::filter(.data$y == min(y)),
    aes(x, y), 
    colour = "#00b8e6",
    size = highlight_point_size
  ) +
  # Q.E.D
  geom_point(
    data = beeswarm_rmse %>% dplyr::filter(.data$x.orig == "Round 1" & .data$col == "3377646") %>% dplyr::filter(.data$y == min(y)),
    aes(x, y), 
    colour = "#307905",
    fill = "black",
    size = highlight_point_size
  ) +
  geom_point(
    data = beeswarm_rmse %>% dplyr::filter(.data$x.orig == "Round 2" & .data$col == "3377646") %>% dplyr::filter(.data$y == min(y)),
    aes(x, y), 
    colour = "#307905", 
    size = highlight_point_size
  ) +
  # AI Winter is Coming
  geom_point(
    data = beeswarm_rmse %>% dplyr::filter(.data$x.orig == "Round 1" & .data$col == "3332429") %>% dplyr::filter(.data$y == min(y)),
    aes(x, y), 
    colour = "#0000FF", 
    size = highlight_point_size
  ) +
  geom_point(
    data = beeswarm_rmse %>% dplyr::filter(.data$x.orig == "Round 2" & .data$col == "3332429") %>% dplyr::filter(.data$y == min(y)),
    aes(x, y), 
    colour = "#0000FF", 
    size = highlight_point_size
  ) +
  # DMIS_DK (RF)
  geom_point(
    data = beeswarm_rmse %>% dplyr::filter(.data$x.orig == "Round 2" & .data$bg == "9686312") %>% dplyr::filter(.data$y == min(y)),
    aes(x, y), 
    colour = "#f2b63d",
    size = highlight_point_size
  ) + 
  # DMIS_DK (DL)
  geom_point(
    data = beeswarm_rmse %>% dplyr::filter(.data$x.orig == "Round 2" & .data$bg == "9686330") %>% dplyr::filter(.data$y == min(y)),
    aes(x, y), 
    colour = "#ff8c40",
    size = highlight_point_size
  ) +
  # olabayle
  geom_point(
    data = beeswarm_rmse %>% dplyr::filter(.data$x.orig == "Round 2" & .data$col == "3385484") %>% dplyr::filter(.data$y == min(y)),
    aes(x, y), 
    colour = "#00d91d", 
    size = highlight_point_size
  ) +
  # Best R1—>R2 RMSE improvement team, submitter_id: 3323475, R1 + R2
  geom_point(
    data = beeswarm_rmse %>% dplyr::filter(.data$x.orig == "Round 1" & .data$col == "3323475") %>% dplyr::filter(.data$y == min(y)),
    aes(x, y), 
    colour = "#f23de6", 
    size = main_point_size,
    shape = 1,
    stroke = stroke_size_highlight
  ) +
  geom_point(
    data = beeswarm_rmse %>% dplyr::filter(.data$x.orig == "Round 2" & .data$col == "3323475") %>% dplyr::filter(.data$y == min(y)),
    aes(x, y), 
    colour = "#f23de6", 
    size = highlight_point_size
  ) +
  # - - -
  labs(y = "RMSE", tag = "c") + 
  theme_bw() + 
  theme(
    axis.text = element_text(size = size_text), 
    axis.title = element_text(size = size_text),
    axis.title.x = element_blank(),
    plot.tag = element_text(size = size_tag,  face = "bold"),
    legend.position = "none"
  )


# Figure 3e --------------------------------------------------------------------------------------------
df_rmse_leq_2_r1 <- df_rmse_leq_2 %>% dplyr::filter(.data$round == "Round 1")

ggplot(data = df_rmse_leq_2_r1, mapping = aes(x = rmse, y = spearman)) + 
  geom_point(
    size = main_point_size,
    color = rgb(0.59, 0.59, 0.59)
  ) +
  geom_point(
    data = df_rmse_leq_2_r1 %>% dplyr::filter(.data$baseline_model == "yes"),
    aes(x = rmse, y = spearman),
    size = highlight_point_size,
    color = "#e50000"
  ) +
  # - - -
  # Highlight best teams
  # @gregkoytiger
  geom_point(
    data = df_rmse_leq_2_r1 %>% dplyr::filter(.data$highlighted_teams == "Gregory Koytiger"),
    aes(x = rmse, y = spearman),
    size = highlight_point_size,
    color = "#00b8e6"
  ) +
  # Q.E.D
  geom_point(
    data = df_rmse_leq_2_r1 %>% dplyr::filter(.data$highlighted_teams == "Q.E.D"),
    aes(x = rmse, y = spearman),
    size = highlight_point_size,
    color = "#307905"
  ) +
  # AI Winter is Coming
  geom_point(
    data = df_rmse_leq_2_r1 %>% dplyr::filter(.data$highlighted_teams == "AI Winter is Coming"),
    aes(x = rmse, y = spearman),
    size = highlight_point_size,
    color = "#0000FF"
  ) +
  # Zahraa Sobhy
  geom_point(
    data = df_rmse_leq_2_r1 %>% dplyr::filter(.data$highlighted_teams == "Zahraa Sobhy"),
    aes(x = rmse, y = spearman),
    size = highlight_point_size,
    color = "#f23de6"
  ) +
  # - - -
  labs(
    x = "RMSE", 
    y = "Spearman correlation", 
    tag = "e"
  ) + 
  ggtitle("Round 1") + 
  coord_equal(ratio = 1.3) + 
  xlim(0.89, 2) + 
  ylim(-0.19, 0.6) + 
  theme_bw() + 
  theme(
    plot.title = element_text(size = size_text,  face = "plain", hjust = 0.5),
    axis.text = element_text(size = size_text), 
    axis.title = element_text(size = size_text),
    plot.tag = element_text(size = size_tag, face = "bold")
  ) 


# Figure 3f --------------------------------------------------------------------------------------------
df_rmse_leq_2_r2 <- df_rmse_leq_2 %>% dplyr::filter(.data$round == "Round 2")

ggplot(data = df_rmse_leq_2_r2, mapping = aes(x = rmse, y = spearman)) + 
  geom_point(
    size = main_point_size,
    color =  rgb(0.59, 0.59, 0.59)
  ) +
  geom_point(
    data = df_rmse_leq_2_r2 %>% dplyr::filter(.data$baseline_model == "yes"),
    aes(x = rmse, y = spearman),
    size = highlight_point_size,
    color = "#e50000"
  ) +
  # Highlight deep learning methods
  geom_point(
    data = df_rmse_leq_2_r2 %>% dplyr::filter(.data$deep_learning == "yes"),
    aes(x = rmse, y = spearman),
    size = highlight_point_size + 0.4,
    color = rgb(0.5, 0.5, 0.5),
    shape = 17
  ) +
  # - - -
  # Highlight best teams
  # @gregkoytiger
  geom_point(
    data = df_rmse_leq_2_r2 %>% dplyr::filter(.data$highlighted_teams == "Gregory Koytiger"),
    aes(x = rmse, y = spearman),
    size = highlight_point_size,
    color = "#00b8e6",
    shape = 17
  ) +
  # Q.E.D
  geom_point(
    data = df_rmse_leq_2_r2 %>% dplyr::filter(.data$highlighted_teams == "Q.E.D"),
    aes(x = rmse, y = spearman),
    size = highlight_point_size,
    color = "#307905"
  ) + 
  # AI Winter is Coming
  geom_point(
    data = df_rmse_leq_2_r2 %>% dplyr::filter(.data$highlighted_teams == "AI Winter is Coming"),
    aes(x = rmse, y = spearman),
    size = highlight_point_size,
    color = "#0000FF"
  ) + 
  # DMIS_DK (DL)
  geom_point(
    data = df_rmse_leq_2_r2 %>% dplyr::filter(.data$highlighted_teams == "DMIS_DK (DL)"),
    aes(x = rmse, y = spearman),
    size = highlight_point_size,
    color = "#ff8c40",
    shape = 17
  ) + 
  # DMIS_DK (RF)
  geom_point(
    data = df_rmse_leq_2_r2 %>% dplyr::filter(.data$highlighted_teams == "DMIS_DK (RF)"),
    aes(x = rmse, y = spearman),
    size = highlight_point_size,
    color = "#f2b63d"
  ) + 
  # Olivier Labayle
  geom_point(
    data = df_rmse_leq_2_r2 %>% dplyr::filter(.data$highlighted_teams == "Olivier Labayle"),
    aes(x = rmse, y = spearman),
    size = highlight_point_size,
    color = "#00d91d"
  ) + 
  # Zahraa Sobhy
  geom_point(
    data = df_rmse_leq_2_r2 %>% dplyr::filter(.data$highlighted_teams == "Zahraa Sobhy"),
    aes(x = rmse, y = spearman),
    size = highlight_point_size,
    color = "#f23de6"
  ) +
  # - - -
  labs(
    x = "RMSE", 
    y = "Spearman correlation", 
    tag = "f"
  ) + 
  ggtitle("Round 2") + 
  coord_equal(ratio = 1.3) + 
  xlim(0.89, 2) + 
  ylim(-0.19, 0.6) + 
  theme_bw() + 
  theme(
    plot.title = element_text(size = size_text, face = "plain", hjust = 0.5),
    axis.text = element_text(size = size_text), 
    axis.title = element_text(size = size_text),
    plot.tag = element_text(size = size_tag, face = "bold")
  ) 
