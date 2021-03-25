# Install required libraries
required_lib <- c("tidyverse", "cocor")
for (package in required_lib){
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, quiet = FALSE)
  }
}

# Load tidyverse library
library(tidyverse)


# Read data ----------------------------------------------------------------------------------------------
df_prot <- 
  readr::read_csv(
    "~/source_data_figures/Fig6/Fig6_protein_kernels_pred.csv",
    col_names = TRUE
  )


# Calculate Pearson correlations between measured and predicted pKd's ------------------------------------
df_corr_prot <- 
  purrr::map_dfr(
    df_prot$kernel %>% unique(),
    function(.kernel){
      df_prot %>% 
        dplyr::filter(.data$kernel == .kernel) %>% 
        dplyr::mutate(
          pearson_corr = cor(pKd_pred, pKd_true)
        ) %>% 
        dplyr::select(
          kernel,
          color,
          pearson_corr
        ) %>% 
        dplyr::distinct()
    }
  )


# Statistical testing of the correlations obtained with the models based on the different protein kernels ---
# against the original Q.E.D model (SW) as the reference ----------------------------------------------------
reference <- "SW"

# Create a data frame with model predictions in separate columns
df_pred <- 
  df_prot %>% 
  dplyr::select(
    -.data$color
  ) %>% 
  tidyr::spread(
    key = kernel,
    value = pKd_pred
  ) %>% 
  as.data.frame() # cocor package does not work with a tibble

p_val <- 
  purrr::map(
    df_corr_prot$kernel %>% unique() %>% purrr::set_names(),
    function(.kernel_type){
      if (.kernel_type == reference){
        return("reference")
      } else{
        res <- 
          # two-sided test
          cocor::cocor(
            as.formula(paste("~", .kernel_type, "+ pKd_true |", reference, "+ pKd_true")),
            df_pred,
            #test = "pearson1898",
            return.htest = TRUE
          )$pearson1898
        
        pval <- res$p.value
        
        return(pval)
      }
    }
  ) %>% 
  dplyr::bind_rows() %>% 
  # transpose
  gather(key = kernel, value = p_val) 


# Benjamini-Hochberg adjusted p-values ------------------
p_val_prot <- 
  p_val %>%
  dplyr::filter(.data$p_val != "reference")

p_val_prot_BH_adj <- 
  tibble(
    p_val_BH_adj = stats::p.adjust(p = p_val_prot$p_val, method = "BH"),
    kernel = p_val_prot$kernel
  ) %>% 
  dplyr::mutate(
    p_val_BH_adj = 
      ifelse(
        p_val_BH_adj < 0.001,
        formatC(p_val_BH_adj, format = "e", digits = 2),
        round(p_val_BH_adj, digits = 3)
      )
  )

p_val <- 
  p_val %>% 
  dplyr::left_join(
    p_val_prot_BH_adj,
    by = "kernel"
  ) %>% 
  dplyr::mutate(
    p_val_BH_adj = dplyr::if_else(
      p_val == "reference",
      p_val,
      p_val_BH_adj
    )
  )

df_corr_prot <- 
  df_corr_prot %>% 
  dplyr::left_join(
    p_val,
    by = "kernel"
  )

x_axis_names <- tibble::tribble(
  ~kernel, ~kernel_label,
  "GO_based_similarity",            "Gene Ontologies",
  "Pathway_based_similarity",       "Protein-pathway\nassociation network",
  "PPI_based_similarity",           "Protein-protein\ninteraction network",
  "Structure_based_similarity",     "Protein structures",
  "SW",                             "Full amino acid\nsequences",
  "protein_SW_norm_kernel__atp",    "ATP binding pocket\nsequences",
  "protein_SW_norm_kernel__kindom", "Kinase domain\n sequences"
)

df_corr_prot <- 
  df_corr_prot %>% 
  dplyr::left_join(
    x_axis_names,
    by = "kernel"
  )

df_color <- 
  df_corr_prot %>% 
  dplyr::select(
    .data$kernel_label, 
    .data$color
  ) %>% 
  dplyr::distinct()

type_colors_prot <- df_color$color
names(type_colors_prot) <- df_color$kernel_label

type_colors_prot_line <- type_colors_prot
type_colors_prot_line[names(type_colors_prot_line)] <- "#808080"
type_colors_prot_line["Full amino acid\nsequences"] <- "#A2605E"


# Figure 6a
ggplot(
  data = df_corr_prot, 
  aes(
    x = kernel_label %>% reorder(pearson_corr), 
    y = pearson_corr
  )
  ) + 
  geom_bar(
    aes(fill = kernel_label, color = kernel_label), 
    stat = "identity", 
    width = 0.45
  ) + 
  scale_fill_manual(
    values = type_colors_prot
  ) + 
  scale_color_manual(
    values = type_colors_prot_line
  ) + 
  annotate(
    geom = "text",
    hjust = 0,
    x = df_corr_prot %>% dplyr::pull(.data$kernel_label) %>% unique(),
    y = 0.02,
    fontface = if_else(
      df_corr_prot %>% 
        dplyr::select(.data$kernel_label, .data$p_val_BH_adj) %>% 
        dplyr::distinct() %>% 
        dplyr::pull(.data$p_val_BH_adj) %>% 
        as.numeric() < 0.05,
      2,
      1
    ),
    label = df_corr_prot %>%
      dplyr::select(.data$kernel_label, .data$p_val_BH_adj) %>% 
      dplyr::distinct() %>% 
      dplyr::mutate(p_val_BH_adj = stringr::str_replace(p_val_BH_adj, "reference", "Submitted Q.E.D model")) %>% 
      dplyr::pull(.data$p_val_BH_adj),
    size = 4.2
  ) + 
  labs(
    y = "Pearson correlation",
    x = "",
    tag = "a",
    title = "Protein kernels"
  ) +
  theme_bw() + 
  theme(
    legend.position = "none",
    plot.tag = element_text(face = "bold", size = 21),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 17),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 17),
    axis.text.y = element_text(face = c(rep("plain",6) ,"bold"))
  ) +
  coord_flip()
