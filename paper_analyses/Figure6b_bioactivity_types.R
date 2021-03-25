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
df <- 
  readr::read_csv(
    "~/source_data_figures/Fig6/Fig6_bioactivity_types_pred.csv",
    col_names = TRUE
  )

df_n_train <- 
  readr::read_csv(
    "~/source_data_figures/Fig6/Fig6_bioactivity_types_n_training_data_points.csv",
    col_names = TRUE
  )


# Calculate Pearson correlations ------------------------------------------------------------------------------
df_corr <- 
  purrr::map_dfr(
  df$bioactivity_type %>% unique(),
  function(.bio){
    df %>% 
      dplyr::filter(.data$bioactivity_type == .bio) %>% 
      dplyr::mutate(
        pearson_corr = cor(pKd_pred, pKd_true)
      ) %>% 
      dplyr::select(
        bioactivity_type,
        color,
        pearson_corr
      ) %>% 
      dplyr::distinct()
  }
)


# Statistical testing of the correlations obtained with the models based on the different bioactivity types 
# against the original Q.E.D model (KdKiEC50) as the reference ----------------------------------------------
reference <- "KdKiEC50"

# Create a data frame with model predictions in separate columns
df_pred <- 
  df %>% 
  dplyr::select(
    -.data$color
  ) %>% 
  tidyr::spread(
    key = bioactivity_type,
    value = pKd_pred
  ) %>% 
  as.data.frame() # cocor package does not work with a tibble

# Correlation comparison
p_val <- 
  purrr::map(
    df_corr$bioactivity_type %>% unique() %>% purrr::set_names(),
    function(.bio){
      
      if (.bio == reference){
        return("reference")
      } else{
        
        res <- 
          cocor::cocor(
            as.formula(paste("~", .bio, "+ pKd_true |", reference, "+ pKd_true")),
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
  gather(key = bioactivities, value = p_val) 


# Benjamini-Hochberg adjusted p-values  ------------------
p_val_tmp <- 
  p_val %>%
  dplyr::filter(p_val != "reference")

p_val_BH_adj <- 
  tibble(
    p_val_BH_adj = stats::p.adjust(p = p_val_tmp$p_val, method = "BH"),
    bioactivities = p_val_tmp$bioactivities
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
    p_val_BH_adj,
    by = "bioactivities"
  ) %>% 
  dplyr::mutate(
    p_val_BH_adj = dplyr::if_else(
      p_val == "reference",
      p_val,
      p_val_BH_adj
    )
  )

df_corr <- 
  df_corr %>% 
  dplyr::left_join(
    p_val,
    by = c("bioactivity_type" = "bioactivities")
  )


df_color <- 
  df_corr %>% 
  dplyr::select(
    .data$bioactivity_type, 
    .data$color
  ) %>% 
  dplyr::distinct()

type_colors_bio <- df_color$color
names(type_colors_bio) <- df_color$bioactivity_type


x_axis_names <- 
  tibble::tribble(
    ~bioactivities, ~bioactivities_label,
    "EC50", "EC50",
    "IC50", "IC50",
    "Kd", "Kd",
    "Ki", "Ki",
    "KdEC50", "Kd, EC50",
    "KdIC50", "Kd, IC50",
    "KiEC50", "Ki, EC50", 
    "KiIC50", "Ki, IC50",
    "KdKi", "Kd, Ki",
    "KdKiEC50", "Kd, Ki, EC50",
    "KdKiIC50", "Kd, Ki, IC50"
  )

# Add number of training bioactivities
x_axis_names <- 
  x_axis_names %>% 
  dplyr::left_join(
    df_n_train,
    by = c("bioactivities" = "bioactivity_type")
  ) %>% 
  dplyr::mutate(
    bioactivities_label = paste0(
      bioactivities_label,
      " (",
      N,
      ")"
    )
  ) %>% 
  dplyr::select(-.data$N)

df_corr_bio <- 
  df_corr %>% 
  dplyr::left_join(
    x_axis_names,
    by = c("bioactivity_type" = "bioactivities")
  )

type_colors_bio_line <- type_colors_bio
type_colors_bio_line[names(type_colors_bio_line)] <- "#597390"    
type_colors_bio_line["KdKiEC50"] <- "#A2605E"


# Figure 6b
ggplot(
  data = df_corr_bio, 
  aes(
    x = bioactivities_label %>% reorder(pearson_corr),
    y = pearson_corr
  )
  ) + 
  geom_bar(
    aes(fill = bioactivity_type, color = bioactivity_type), 
    stat = "identity", 
    width = 0.55
  ) +
  scale_fill_manual(
    values = type_colors_bio
  ) +
  scale_color_manual(
    values = type_colors_bio_line
  ) +
  annotate(
    geom = "text",
    hjust = 0,
    x = df_corr_bio$bioactivities_label %>% unique(),
    y = 0.02,
    fontface = if_else(
      df_corr_bio %>% 
        dplyr::select(.data$bioactivities_label, .data$p_val_BH_adj) %>% 
        dplyr::distinct() %>% 
        dplyr::pull(.data$p_val_BH_adj) %>% 
        as.numeric() < 0.05,
      2,
      1
    ),
    label = df_corr_bio %>% 
      dplyr::select(.data$bioactivities_label, .data$p_val_BH_adj) %>% 
      dplyr::distinct() %>% 
      dplyr::mutate(p_val_BH_adj = stringr::str_replace(p_val_BH_adj, "reference", "Submitted Q.E.D model")) %>% 
      dplyr::pull(.data$p_val_BH_adj),
    size = 4.2
  ) +
  labs(
    x = "",
    y = "Pearson correlation",
    tag = "b",
    title = "Training bioactivity types"
  ) +
  # suboptimal solution (numbers from df_n_train)
  scale_x_discrete(labels = c(
    expression("EC"[50]*" (3296)"),
    expression("IC"[50]*" (21620)"),
    expression("K"[i]*", EC"[50]* " (51295)"),
    expression("K"[i]*" (48809)"),
    expression("K"[i]*", IC"[50]* " (56370)"),
    expression("K"[d]*", IC"[50]* " (30052)"),
    expression("K"[d]*", EC"[50]* " (13262)"),
    expression(bold("K"[d]*" (10536)")),
    expression("K"[d]*", K"[i]*", IC"[50]* " (63757)"),
    expression("K"[d]*", K"[i]*", EC"[50]* " (52532)"),
    expression("K"[d]*", K"[i]* " (58023)")
  )
  ) +
  theme_bw()  +
  theme(
    legend.position = "none",
    plot.tag = element_text(face = "bold", size = 21),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 17),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 17)
  ) + 
  coord_flip()
