# Install required libraries
required_lib <- c("tidyverse", "VennDiagram")
for (package in required_lib){
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, quiet = FALSE)
  }
}

# Load tidyverse library
library(tidyverse)


# Read data ----------------------------------------------------------------------------------------------
df <- 
  readr::read_csv(
    "~/source_data_figures/Fig2_FigS2/Fig2_FigS2.csv",
    col_names = TRUE
  )


# Kinase and compound overlap between Round 1 and Round 2 ------------------------------------------------

# Kinases
#dev.off()
venn.plot <- 
  VennDiagram::draw.pairwise.venn(
    area1 = df %>% 
      dplyr::filter(.data$round == "Round2") %>% 
      dplyr::pull(.data$UniProt_Id) %>% 
      n_distinct(),
    area2 = df %>% 
      dplyr::filter(.data$round == "Round1") %>% 
      dplyr::pull(.data$UniProt_Id) %>% 
      n_distinct(),
    cross.area = intersect(
      df %>% dplyr::filter(.data$round == "Round2") %>% dplyr::pull(.data$UniProt_Id) %>% unique(),
      df %>% dplyr::filter(.data$round == "Round1") %>% dplyr::pull(.data$UniProt_Id) %>% unique()
      ) %>% 
      length(),
    euler.d = TRUE,
    scaled = TRUE,
    inverted = TRUE,
    col = c(rgb(102/255,0/255,0/255), rgb(60/255,0/255,130/255)),
    fill = c(rgb(255/255,153/255,153/255), rgb(229/255,204/255,255/255)),
    alpha = 0.5,
    fontfamily = "Arial",
    cex = 2.8
)

# Compounds
dev.off()
venn.plot <- 
  VennDiagram::draw.pairwise.venn(
    area1 = df %>% 
      dplyr::filter(.data$round == "Round2") %>% 
      dplyr::pull(.data$Compound_Name) %>% 
      n_distinct(),
    area2 = df %>% 
      dplyr::filter(.data$round == "Round1") %>% 
      dplyr::pull(.data$Compound_Name) %>% 
      n_distinct(),
    cross.area = intersect(
      df %>% dplyr::filter(.data$round == "Round2") %>% dplyr::pull(.data$Compound_Name) %>% unique(),
      df %>% dplyr::filter(.data$round == "Round1") %>% dplyr::pull(.data$Compound_Name) %>% unique()
    ) %>% 
      length(),
    euler.d = TRUE,
    scaled = TRUE,
    inverted = TRUE,
    col = c(rgb(60/255,0/255,130/255), rgb(102/255,0/255,0/255)),
    fill = c(rgb(229/255,204/255,255/255), rgb(255/255,153/255,153/255)),
    alpha = 0.5,
    fontfamily = "Arial",
    cex = 2.8
)


# Distribution of pKd values -----------------------------------------------------------------------------

ggplot(data = df, aes(x = pKd)) + 
  geom_histogram(
    data = subset(df, round == "Round1"), 
    aes(fill = round), 
    alpha = 0.4
  ) +
  geom_histogram(
    data = subset(df, round == "Round2"),
    aes(fill = round), 
    alpha = 0.4
  ) +
  labs(
    x = expression("pK"["d"]),
    y = "Number of pairs"
  ) + 
  # Legend
  scale_fill_manual(
    name = "", 
    values = c(rgb(153/255, 0/255, 153/255), "red"), 
    labels = c(
      expression("Round 1: 430 pairs"),
      expression("Round 2: 394 pairs")
    )
  ) + 
  theme_bw() + 
  theme(
    axis.text = element_text(size = 27),
    axis.title = element_text(size = 27),
    legend.title = element_blank(), 
    legend.text = element_text(size = 23),
    legend.position = c(0.985, 0.95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1,1,1,1),
    legend.spacing.x = unit(0.3, "cm"),
    legend.key.height = unit(2, "line"),
    plot.margin = unit(c(1, 0.5, 0.5, 0.5), "cm")
  )


# Distribution of compound SI ----------------------------------------------------------------------------
df_si <- 
  df %>% 
  dplyr::select(
    .data$round,
    .data$Compound_Name,
    .data$selectivity_index
  ) %>% 
  dplyr::distinct()

ggplot(data = df_si, aes(x = selectivity_index)) + 
  geom_histogram(
    data = subset(df_si, round == "Round1"), 
    bins = 35,
    aes(fill = round), 
    alpha = 0.4
  ) +
  geom_histogram(
    data = subset(df_si, round == "Round2"),
    bins = 35,
    aes(fill = round), 
    alpha = 0.4
  ) +
  labs(
    x = "Selectivity index",
    y = "Number of compounds"
  ) + 
  xlim(-0.005, 0.15) + 
  # Legend
  scale_fill_manual(
    name = "", 
    values = c(rgb(153/255, 0/255, 153/255), "red"), 
    labels = c(
      "Round 1: 70 compounds",
      "Round 2: 25 compounds"
    )
  ) + 
  theme_bw() + 
  theme(
    axis.text = element_text(size = 27), 
    axis.title = element_text(size = 27),
    legend.title = element_blank(), 
    legend.text = element_text(size = 23),
    legend.position = c(0.985, 0.95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1,1,1,1),
    legend.spacing.x = unit(0.3, "cm"),
    legend.key.height = unit(2, "line"),
    plot.margin = unit(c(1, 0.5, 0.5, 0.5), "cm")
  )



# Kinase groups overview ---------------------------------------------------------------------------------

df_kin <- 
  purrr::map_dfr(
    c("Round1", "Round2"), 
    ~ df %>% 
      dplyr::filter(.data$round == .x) %>% 
      dplyr::select(
        .data$UniProt_Id,
        .data$kinase_group
      ) %>% 
      dplyr::distinct() %>% 
      dplyr::group_by(.data$kinase_group) %>% 
      dplyr::summarise(n = n()) %>% 
      dplyr::mutate(round = .x)
  )

ggplot(df_kin, aes(x = kinase_group, y = n, fill = round)) + 
  geom_bar(
    stat = "identity", 
    position = "dodge",
    width = 0.6,
    alpha = 0.4
  ) + 
  labs(
    y = "Number of kinases", 
    x = "Kinase group"
  ) +
  # Legend
  scale_fill_manual(
    name = "", 
    values = c(rgb(153/255, 0/255, 153/255), "red"), 
    labels = c(
      "Round 1: 199 kinases",
      "Round 2: 207 kinases"
    )
  ) +
  theme(aspect.ratio = 3/1) + 
  theme_bw() +
  theme(
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 25), 
    axis.text.x = element_text(size = 20, angle = 45, hjust = 1), 
    axis.title = element_text(size = 25),
    legend.position = c(0.015, .97),
    legend.justification = c("left", "top"),
    legend.title = element_blank(),
    legend.text = element_text(size = 21.5),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    legend.spacing.x = unit(0.3, "cm"),
    legend.key.height = unit(2, "line")
  ) 
