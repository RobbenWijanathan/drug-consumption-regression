library(tidyverse)
library(readr)
library(fmsb)
library(RColorBrewer)
library(reshape2)
library(stringr)

url <- "https://raw.githubusercontent.com/RobbenWijanathan/drug-consumption-regression/main/drug_consumption.csv"
data <- read_csv(url)

cat("rows in original data:", nrow(data), "\n")
print(head(data))
print(colSums(is.na(data)))

drug_cols <- c("Alcohol","Amphet","Amyl","Benzos","Caff","Cannabis",
               "Choc","Coke","Crack","Ecstasy","Heroin","Ketamine",
               "Legalh","LSD","Meth","Mushrooms","Nicotine","VSA")

all_to_convert <- drug_cols
if ("Semer" %in% names(data)) all_to_convert <- c(all_to_convert, "Semer")

df <- data %>%
  mutate(across(all_of(intersect(all_to_convert, names(.))),
                ~ as.numeric(gsub("^CL", "", as.character(.)))))

# Ensure Semer exists and filter rows where Semer == 0, else warn and proceed without filtering
if ("Semer" %in% names(df)) {
  cat("Semer distribution before filtering:\n")
  print(table(df$Semer, useNA = "ifany"))
  df <- df %>%
    filter(!is.na(Semer) & Semer == 0) %>%
    select(-Semer)
  cat("Rows after keeping Semer == 0:", nrow(df), "\n")
} else {
  warning("Semer column not found — skip Semer filtering.")
}

if (nrow(df) == 0) stop("No rows remain after Semer filtering. Check Semer values or remove the filter.")

# Radar Plot for Each Demographic Attributes
cat_vars <- c("Age", "Gender", "Education", "Country", "Ethnicity")
top_n <- 5

make_radar_df <- function(means_df) {
  max_row <- rep(6, ncol(means_df))
  min_row <- rep(0, ncol(means_df))
  radar <- rbind(max_row, min_row, as.matrix(means_df))
  rownames(radar)[1:2] <- c("Max","Min")
  as.data.frame(radar)
}

get_palette <- function(n) {
  max_pal <- 8
  if (n <= 5) {
    brewer.pal(n, "Set2")
  } else if (n <= max_pal) {
    brewer.pal(n, "Set3")
  } else {
    colorRampPalette(brewer.pal(8, "Set3"))(n)
  }
}

for (var in cat_vars) {
  if (! (var %in% names(df)) ) {
    message("Column ", var, " not found in df — skipping.")
    next
  }
  
  top_groups <- df %>%
    filter(!is.na(.data[[var]])) %>%  
    count(across(all_of(var))) %>%
    arrange(desc(n)) %>%
    slice_head(n = top_n) %>%
    pull(!!sym(var)) %>%
    as.character()
  
  if (length(top_groups) == 0) {
    message("No non-NA groups for ", var, " — skipping.")
    next
  }
  
  means_df <- df %>%
    filter((!!sym(var)) %in% top_groups) %>%
    group_by(across(all_of(var))) %>%
    summarise(across(all_of(drug_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate(across(all_of(var), as.character)) %>%
    arrange(!!sym(var))
  
  means_df <- means_df %>% mutate(across(all_of(drug_cols), ~ ifelse(is.nan(.), NA, .)))
  
  rownames(means_df) <- means_df[[var]]
  group_labels <- str_wrap(means_df[[var]], width = 14)
  
  means_matrix <- means_df %>% select(all_of(drug_cols))
  
  radar_df <- make_radar_df(means_matrix)
  
  n_groups <- nrow(means_matrix)
  if (n_groups < 1) {
    message("No numeric means for groups in ", var, " — skipping plot.")
    next
  }
  
  line_cols <- get_palette(n_groups)
  fill_cols <- sapply(line_cols, function(x) adjustcolor(x, alpha.f = 0.3))
  
  par(mfrow = c(1,1),
      mar = c(2, 2, 3, 8),
      xpd = NA)
  
  radarchart(radar_df,
             axistype = 1,
             pcol = line_cols,
             pfcol = fill_cols,
             plwd = 2,
             plty = 1,
             cglcol = "grey80",
             cglty = 1,
             axislabcol = "grey40",
             caxislabels = 0:6,
             cglwd = 1,
             vlcex = 0.8,
             title = paste0("Average Drug Usage by ", var)
  )
  
  legend("right",
         inset = c(-0.20, 0),
         legend = group_labels,
         col = line_cols,
         pch = 16,
         bty = "n",
         cex = 0.9,
         pt.cex = 1.4,
         x.intersp = 0.5,
         y.intersp = 1
  )
}

# Spearman Correlation Plot for Each Personality Attributes
pers_cols <- c("Nscore","Escore","Oscore","AScore","Cscore","Impulsive","SS")
pers_cols <- intersect(pers_cols, names(df))
drug_cols <- intersect(drug_cols, names(df))

if (length(pers_cols) == 0 || length(drug_cols) == 0) {
  stop("No personality or drug columns found in df for correlation plot.")
}

dat_corr <- df %>% select(all_of(c(pers_cols, drug_cols)))

sds <- sapply(dat_corr, sd, na.rm = TRUE)
zero_var_cols <- names(sds)[is.na(sds) | sds == 0]
if (length(zero_var_cols) > 0) {
  message("Removing constant or all-NA columns before correlation: ", paste(zero_var_cols, collapse = ", "))
  dat_corr <- dat_corr %>% select(-all_of(zero_var_cols))
}

if (ncol(dat_corr) < 2) stop("Not enough columns to compute correlations after removing constant columns.")

corr_mat <- cor(dat_corr, method = "spearman", use = "pairwise.complete.obs")

pers_present <- intersect(pers_cols, rownames(corr_mat))
drug_present <- intersect(drug_cols, colnames(corr_mat))

if (length(pers_present) == 0 || length(drug_present) == 0) {
  stop("After cleaning, no overlapping personality/drug columns remain for plotting.")
}

corr_sub <- corr_mat[pers_present, drug_present, drop = FALSE]

corr_long <- reshape2::melt(corr_sub, varnames = c("personality","drug"), value.name = "Correlation") %>%
  as_tibble() %>%
  drop_na(Correlation)

ggplot(corr_long, aes(x = drug, y = personality, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-1,1), na.value = "grey80") +
  geom_text(aes(label = round(Correlation, 2)), size = 3, na.rm = TRUE) +
  labs(title = "Spearman Correlation (Personality vs Drugs)", x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar Plot for Drug Usage

drug_dist <- data %>%
  pivot_longer(cols = all_of(drug_cols),
               names_to = "Drug",
               values_to = "Class") %>%
  mutate(Class = factor(Class, levels = paste0("CL", 0:6))) %>%
  group_by(Drug, Class) %>%
  summarise(n = n(), .groups = "drop")

plot_list <- list()

for (drug in drug_cols) {
  
  p <- ggplot(drug_dist %>% filter(Drug == drug),
              aes(x = Class, y = n, fill = Class)) +
    geom_col() +
    scale_fill_brewer(palette = "Set2") +
    labs(title = paste("Distribution of", drug, "Usage (CL0–CL6)"),
         x = "Usage Class",
         y = "Count") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)       
  plot_list[[drug]] <- p   
}