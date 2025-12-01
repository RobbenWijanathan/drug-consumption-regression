library(tidyverse)
library(readr)
library(fmsb)
library(RColorBrewer)

url <- "https://raw.githubusercontent.com/RobbenWijanathan/drug-consumption-regression/main/drug_consumption.csv"
data <- read_csv(url)

drug_cols <- c("Alcohol","Amphet","Amyl","Benzos","Caff","Cannabis",
               "Choc","Coke","Crack","Ecstasy","Heroin","Ketamine",
               "Legalh","LSD","Meth","Mushrooms","Nicotine","Semer","VSA")

df <- data %>%
  mutate(across(all_of(drug_cols),
                ~ as.numeric(gsub("CL","", as.character(.)))))

# Radar Plot for Each Non-Personality Attributes

cat_vars <- c("Age", "Gender", "Education", "Country", "Ethnicity")
top_n <- 5

make_radar_df <- function(means_df) {
  max_row <- rep(6, ncol(means_df))
  min_row <- rep(0, ncol(means_df))
  radar <- rbind(max_row, min_row, as.matrix(means_df))
  rownames(radar)[1:2] <- c("Max","Min")
  as.data.frame(radar)
}

for (var in cat_vars) {
  
  top_groups <- df %>%
    filter(!is.na(.data[[var]])) %>%       
    count(across(all_of(var))) %>%
    arrange(desc(n)) %>%
    slice_head(n = top_n) %>%
    pull(!!sym(var)) %>%
    as.character()
  
  if (length(top_groups) == 0) {
    message("No groups for ", var, " — skipping.")
    next
  }
  
  means_df <- df %>%
    filter((!!sym(var)) %in% top_groups) %>%
    group_by(across(all_of(var))) %>%
    summarise(across(all_of(drug_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate(across(all_of(var), as.character)) %>%
    arrange(!!sym(var))
  
  rownames(means_df) <- means_df[[var]]
  library(stringr)
  group_labels <- str_wrap(means_df[[var]], width = 14)
  
  means_matrix <- means_df %>% select(all_of(drug_cols))
  
  radar_df <- make_radar_df(means_matrix)
  
  n_groups <- length(group_labels)
  line_cols <- brewer.pal(5, "Set2")[1:n_groups]
  fill_cols <- sapply(line_cols, function(x) adjustcolor(x, alpha.f = 0.3))
  
  par(mfrow = c(1,1),
      mar = c(2, 2, 3, 8),   
      xpd = NA)           
  
  radarchart(radar_df,
             axistype = 1,
             pcol = line_cols,
             pfcol = fill_cols,
             plwd = 3,
             plty = 1,
             cglcol = "grey80",
             cglty = 1,
             axislabcol = "grey40",
             caxislabels = 0:6,
             cglwd = 1,
             vlcex = 0.9,
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

library(reshape2)   

pers_cols <- c("Nscore","Escore","Oscore","AScore","Cscore","Impulsive","SS")
pers_cols <- intersect(pers_cols, names(df))
drug_cols <- intersect(drug_cols, names(df))

dat_corr <- df %>% select(all_of(c(pers_cols, drug_cols))) %>% drop_na()
corr_mat <- cor(dat_corr[, c(pers_cols, drug_cols)], method = "spearman", use = "pairwise.complete.obs")

corr_sub <- corr_mat[pers_cols, drug_cols]

corr_long <- reshape2::melt(corr_sub, varnames = c("personality","drug"), value.name = "Correlation")

ggplot(corr_long, aes(x = drug, y = personality, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-1,1)) +
  geom_text(aes(label = round(Correlation, 2)), size = 3) +
  labs(title = "Spearman Correlation (Personality vs Drugs)", x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


