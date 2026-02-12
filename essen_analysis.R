library(tidyverse)
library(PCAtest)
library(dplyr)
setwd("/Users/davidwhyatt/Downloads/essen_new")

features <- read_csv("essen_corpus_features_NEW.csv")
features_numeric <- select_if(features, is.numeric)

features_numeric <- features_numeric %>% 
  select(-melody_num) %>%
  mutate(row_num = row_number())

features_for_imputation <- features_numeric %>% select(-row_num)

na_counts <- sapply(features_for_imputation, function(x) sum(is.na(x)))
na_counts <- na_counts[na_counts > 0]
if (length(na_counts) > 0) {
  cat("Features requiring imputation (NA counts):\n")
  print(na_counts)
} else {
  cat("No missing values detected. No imputation needed.\n")
}

features_imputed <- features_for_imputation %>%
  mutate(across(everything(), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))

zero_var_cols <- sapply(features_imputed, function(x) var(x) == 0)
zero_var_feature_names <- names(features_imputed)[zero_var_cols]
if (length(zero_var_feature_names) > 0) {
  cat("Features dropped due to zero variance (post-imputation):\n")
  print(zero_var_feature_names)
} else {
  cat("No zero-variance features found after imputation.\n")
}

features_imputed <- features_imputed[, !zero_var_cols]

dropped_features <- names(features_for_imputation)[zero_var_cols]
if(length(dropped_features) > 0) {
  cat("Features dropped due to zero variance:\n")
  print(dropped_features)
} else {
  cat("No features were dropped due to zero variance\n")
}

features_scaled <- scale(features_imputed)

var_features <- apply(features_scaled, 2, var)

zero_var_features <- names(var_features[var_features == 0])
na_features <- colnames(features_scaled)[apply(features_scaled, 2, function(x) any(is.na(x)))]
zero_var_features <- c(zero_var_features, na_features)

if(length(zero_var_features) > 0) {
  cat("Removing zero variance features after scaling:\n")
  print(zero_var_features)
  features_scaled <- features_scaled[, !colnames(features_scaled) %in% zero_var_features]
} else {
  cat("No zero variance features found after scaling\n") 
}

# this takes absolutely forever
pca_res <- PCAtest(features_scaled, nboot=1000, nperm=1000, alpha=0.05,
                   varcorr=TRUE, plot=FALSE)


# save the pca_res object to an RDS file for later use
saveRDS(pca_res, file = "pca_res.rds")

pca_res <- readRDS("pca_res.rds")

pca_data <- prcomp(features_scaled, scale. = TRUE)
loadings <- pca_data$rotation[,1:5]

loadings_df <- data.frame(
  variable = rownames(loadings),
  PC1 = loadings[,1],
  PC2 = loadings[,2],
  PC3 = loadings[,3],
  PC4 = loadings[,4],
  PC5 = loadings[,5]
)

# add feature categories based on variable names
loadings_df <- loadings_df %>%
  mutate(category = case_when(
    grepl("^absolute_pitch\\.", variable) ~ "Absolute Pitch",
    grepl("^complexity\\.", variable) ~ "Complexity",
    grepl("^contour\\.", variable) ~ "Contour",
    grepl("^corpus\\.", variable) ~ "Corpus",
    grepl("^expectation\\.", variable) | grepl("^idyom\\.", variable) ~ "Expectation",
    grepl("^inter_onset_interval\\.", variable) ~ "Inter-Onset Interval",
    grepl("^interval\\.", variable) ~ "Pitch Interval",
    grepl("^lexical_diversity\\.", variable) ~ "Lexical Diversity",
    grepl("^metre\\.", variable) ~ "Metre",
    grepl("^pitch_class\\.", variable) ~ "Pitch Class",
    grepl("^timing\\.", variable) ~ "Timing",
    grepl("^tonality\\.", variable) ~ "Tonality",
    TRUE ~ "Other"
  ))
# print any uncategorized features
uncategorized <- loadings_df %>% filter(category == "Other")
if(nrow(uncategorized) > 0) {
  cat("Uncategorized Features:\n")
  print(uncategorized$variable)
}

# varimax rotation
k <- ncol(pca_data$rotation)
L_unrot <- sweep(pca_data$rotation[, seq_len(k), drop = FALSE], 2, pca_data$sdev[seq_len(k)], `*`)

varimax_res <- varimax(L_unrot)
rotated_loadings <- varimax_res$loadings

# also rotate scores to the same rotated component space
scores_unrot <- pca_data$x[, seq_len(k), drop = FALSE]
rotated_scores_matrix <- scores_unrot %*% varimax_res$rotmat
colnames(rotated_scores_matrix) <- paste0("Rot_PC", seq_len(ncol(rotated_scores_matrix)))

rotated_loadings_df <- data.frame(
  variable = rownames(rotated_loadings),
  Rot_PC1 = rotated_loadings[,1],
  Rot_PC2 = rotated_loadings[,2]
)

rotated_loadings_df <- rotated_loadings_df %>%
  left_join(loadings_df %>% select(variable, category), by = "variable")


# visualisations
library(ggplot2)
library(ggrepel)

ggplot(rotated_loadings_df, aes(x = Rot_PC1, y = Rot_PC2, color = category, label = variable)) +
  geom_point(size = 3) +
  geom_text_repel(size = 3, max.overlaps = 20) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  theme_minimal() +
  labs(
    title = "Biplot of Varimax-Rotated Loadings",
    subtitle = "Rotated PC1 vs Rotated PC2",
    x = "Rotated PC1",
    y = "Rotated PC2",
    color = "Category"
  )

ggsave("biplot_rotated_pc1_pc2.pdf", width = 10, height = 10)

write.csv(rotated_loadings_df, "Rotated_PCA_Loadings.csv", row.names = FALSE)

unrotated_loadings_df <- data.frame(
  variable = rownames(L_unrot),
  PC1 = L_unrot[, 1],
  PC2 = L_unrot[, 2],
  row.names = NULL
)
write.csv(unrotated_loadings_df, "Unrotated_PCA_Loadings.csv", row.names = FALSE)

# combined unrotated and rotated loadings for comparison
unrotated_loadings_df <- data.frame(
  variable = rownames(L_unrot),
  Unrot_PC1 = L_unrot[, 1],
  Unrot_PC2 = L_unrot[, 2],
  row.names = NULL
)

rotated_loadings_for_merge <- rotated_loadings_df %>%
  select(variable, Rot_PC1, Rot_PC2)

loadings_comparison_df <- unrotated_loadings_df %>%
  left_join(rotated_loadings_for_merge, by = "variable")

if ("category" %in% colnames(rotated_loadings_df)) {
  loadings_comparison_df <- loadings_comparison_df %>%
    left_join(rotated_loadings_df %>% select(variable, category), by = "variable")
}

if (!"category" %in% colnames(loadings_comparison_df)) {
  loadings_comparison_df <- loadings_comparison_df %>%
    mutate(
      category = case_when(
        grepl("^absolute_pitch\\.", variable, ignore.case = TRUE) ~ "Absolute Pitch",
        grepl("^complexity\\.", variable, ignore.case = TRUE) ~ "Complexity",
        grepl("^contour\\.", variable, ignore.case = TRUE) ~ "Contour",
        grepl("^corpus\\.", variable, ignore.case = TRUE) ~ "Corpus",
        grepl("^expectation\\.", variable, ignore.case = TRUE) | grepl("^idyom\\.", variable, ignore.case = TRUE) ~ "Expectation",
        grepl("^inter_onset_interval\\.", variable, ignore.case = TRUE) ~ "Inter-Onset Interval",
        grepl("^interval\\.", variable, ignore.case = TRUE) ~ "Pitch Interval",
        grepl("^lexical_diversity\\.", variable, ignore.case = TRUE) ~ "Lexical Diversity",
        grepl("^metre\\.", variable, ignore.case = TRUE) ~ "Metre",
        grepl("^pitch_class\\.", variable, ignore.case = TRUE) ~ "Pitch Class",
        grepl("^timing\\.", variable, ignore.case = TRUE) ~ "Timing",
        grepl("^tonality\\.", variable, ignore.case = TRUE) ~ "Tonality",
        TRUE ~ "Other"
      )
    )
}

loadings_comparison_df <- loadings_comparison_df %>%
  select(variable, category, Unrot_PC1, Rot_PC1, Unrot_PC2, Rot_PC2)

# prettify
loadings_comparison_prepped <- loadings_comparison_df %>%
  mutate(
    category2 = case_when(
      grepl("^absolute_pitch\\.", variable, ignore.case = TRUE) ~ "Absolute Pitch",
      grepl("^complexity\\.", variable, ignore.case = TRUE) ~ "Complexity",
      grepl("^contour\\.", variable, ignore.case = TRUE) ~ "Contour",
      grepl("^corpus\\.", variable, ignore.case = TRUE) ~ "Corpus",
      grepl("^expectation\\.", variable, ignore.case = TRUE) | grepl("^idyom\\.", variable, ignore.case = TRUE) ~ "Expectation",
      grepl("^inter_onset_interval\\.", variable, ignore.case = TRUE) ~ "Inter-Onset Interval",
      grepl("^interval\\.", variable, ignore.case = TRUE) ~ "Pitch Interval",
      grepl("^lexical_diversity\\.", variable, ignore.case = TRUE) ~ "Lexical Diversity",
      grepl("^metre\\.", variable, ignore.case = TRUE) ~ "Metre",
      grepl("^pitch_class\\.", variable, ignore.case = TRUE) ~ "Pitch Class",
      grepl("^timing\\.", variable, ignore.case = TRUE) ~ "Timing",
      grepl("^tonality\\.", variable, ignore.case = TRUE) ~ "Tonality",
      TRUE ~ "Other"
    ),
    # drop leading category prefix up to first dot
    variable_clean = sub("^[^.]+\\.", "", variable),
    # replace underscores/dots with spaces
    variable_clean = gsub("[_.]", " ", variable_clean),
    variable_clean = trimws(variable_clean),
    # title case words
    variable_clean = tools::toTitleCase(variable_clean)
  ) %>%
  arrange(category2, variable_clean) %>%
  group_by(category2) %>%
  group_modify(~{
    cat_name <- unique(.x$category2)[1]
    header <- tibble(
      variable = cat_name,
      Unrot_PC1 = NA_real_,
      Rot_PC1 = NA_real_,
      Unrot_PC2 = NA_real_,
      Rot_PC2 = NA_real_
    )
    body <- .x %>%
      transmute(
        variable = variable_clean,
        Unrot_PC1, Rot_PC1, Unrot_PC2, Rot_PC2
      )
    bind_rows(header, body)
  }) %>%
  ungroup() %>%
  select(variable, Unrot_PC1, Rot_PC1, Unrot_PC2, Rot_PC2)

write.csv(loadings_comparison_prepped, "PCA_Loadings_Comparison.csv", row.names = FALSE)

# variance explained by rotated components
rot_mat <- unclass(rotated_loadings)
ss_loadings <- colSums(rot_mat^2)
prop_total <- ss_loadings / ncol(features_scaled)

# proportion within the retained k-dimensional space
prop_within_retained <- ss_loadings / sum(ss_loadings)

cat("Variance explained by each rotated component (absolute, of total):\n")
print(round(prop_total, 4))
cat("Variance explained by each rotated component (within retained k PCs):\n")
print(round(prop_within_retained, 4))

rot_names <- colnames(rot_mat)
if (is.null(rot_names)) rot_names <- paste0("Rot_PC", seq_along(prop_within_retained))
prop_within_retained_df <- data.frame(
  Rotated_Component = rot_names,
  Proportion_Explained = as.numeric(prop_within_retained)
)
write.csv(prop_within_retained_df, "Rotated_PCA_Proportion_Explained.csv", row.names = FALSE)

eigvals <- pca_data$sdev^2
comp_names <- paste0("PC", seq_along(eigvals))
unrotated_prop_explained_df <- data.frame(
  Component = comp_names,
  Proportion_Explained = eigvals / sum(eigvals)
)
write.csv(unrotated_prop_explained_df, "Unrotated_PCA_Proportion_Explained.csv", row.names = FALSE)



# 4x3 faceted grid of rotated loadings by feature category (Rot_PC1 vs Rot_PC2)
lim_rot <- max(abs(rotated_loadings_df$Rot_PC1), abs(rotated_loadings_df$Rot_PC2), na.rm = TRUE)
p_rotated_facets <- ggplot(rotated_loadings_df, aes(x = Rot_PC1, y = Rot_PC2, color = category)) +
  geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.4, alpha = 0.3) +
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.4, alpha = 0.3) +
  geom_point() +
  scale_x_continuous(limits = c(-lim_rot, lim_rot)) +
  scale_y_continuous(limits = c(-lim_rot, lim_rot)) +
  coord_equal() +
  facet_wrap(~category, ncol = 4) +
  labs(
    x = "Rotated PC1",
    y = "Rotated PC2"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 1),
    strip.background = element_rect(fill = "grey90", color = NA),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  )

ggsave("rotated_loadings_pc1_pc2_facets.pdf", width = 14, height = 12, plot = p_rotated_facets)

eigvals <- pca_data$sdev^2

rot_mat <- unclass(rotated_loadings)
ss_loadings <- colSums(rot_mat^2)
prop_within_retained <- ss_loadings / sum(ss_loadings)

n_retained <- length(prop_within_retained)
unrotated_var_explained <- eigvals[seq_len(n_retained)] / sum(eigvals)
rotated_var_explained <- as.numeric(prop_within_retained)

variance_table <- data.frame(
  Component = paste0("PC", seq_len(n_retained)),
  Unrotated_Variance_Explained = unrotated_var_explained,
  Rotated_Variance_Explained = rotated_var_explained,
  Unrotated_Cumulative_Explained = cumsum(unrotated_var_explained),
  Rotated_Cumulative_Explained = cumsum(rotated_var_explained)
)
write.csv(variance_table, "PCA_Variance_Explained_Table.csv", row.names = FALSE)

cat("Variance explained by component (unrotated vs rotated):\n")
print(variance_table)

duration_rot_pc1 <- rotated_loadings_df %>%
  filter(category == "Timing") %>%
  select(variable, Rot_PC1) %>%
  arrange(desc(Rot_PC1)) %>%
  mutate(
    variable = gsub("^(?i)duration[_[:space:]]*features\\.?[_[:space:]]*", "", variable, perl = TRUE),
    variable = gsub("[_.]", " ", variable),
    variable = trimws(variable),
    variable = tools::toTitleCase(variable)
  )

cat("Duration Feature Loadings on Rotated PC1 (Sorted by |Loading|):\n")
print(duration_rot_pc1)

write.csv(duration_rot_pc1, "Duration Rotated PC1 Loadings.csv", row.names = FALSE)

rotated_scores <- as_tibble(rotated_scores_matrix) %>%
  mutate(
    row_num = features_numeric$row_num,
    melody_id = features$melody_id,
    .before = 1
  )

cat("rotated_scores columns:\n")
print(colnames(rotated_scores))

# select top 3 melodies by Rotated PC1
selected_melodies_rotpc1 <- 
  rotated_scores %>%
  arrange(desc(Rot_PC1)) %>%
  slice(1:3) %>%
  select(row_num, melody_id, Rot_PC1)

cat("Top 3 melodies by Rotated PC1 score:\n")
{
  old_opts <- options(tibble.width = Inf)
  print(selected_melodies_rotpc1, n = nrow(selected_melodies_rotpc1), width = Inf)
  options(old_opts)
}
cat("Full paths (melody_id):\n")
writeLines(selected_melodies_rotpc1$melody_id)

cat("Available melody IDs (first 10):\n")
print(head(features$melody_id, 10))

# find the row number for the melody with highest Rotated PC1 score
top_melody_row_rotpc1 <- rotated_scores %>%
  arrange((Rot_PC1)) %>%
  slice(1) %>%
  select(row_num, melody_id, Rot_PC1)

cat("Top melody by Rotated PC1 score:\n")
{
  old_opts <- options(tibble.width = Inf)
  print(top_melody_row_rotpc1, n = nrow(top_melody_row_rotpc1), width = Inf)
  options(old_opts)
}
cat("Full path (melody_id):\n")
writeLines(top_melody_row_rotpc1$melody_id)

top_n_features_pc1 <-
  loadings_df %>%
  select(variable, PC1) %>%
  arrange(desc(abs(PC1))) %>%
  slice(1:10)

top_n_features_pc2 <-
  loadings_df %>%
  select(variable, PC2) %>%
  arrange(desc(abs(PC2))) %>%
  slice(1:10)


selected_melodies <- 
  bind_cols(
    row_num = features_numeric$row_num,
    features_imputed
  ) %>%
  arrange(desc(duration_features.note_density)) %>%
  select(row_num, duration_features.note_density) %>%
  slice(1:1)


library(ggplot2)
library(ggpubr)
ggplot(loadings_df, aes(x = PC1, y = PC2, color = category)) +
  geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.4, alpha = 0.3) +
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.4, alpha = 0.3) +
  geom_point() +
  scale_x_continuous(limits = c(-0.2, 0.2)) +
  scale_y_continuous(limits = c(-0.2, 0.2)) +
  facet_wrap(~category, ncol = 3) +
  labs(
    x = "PC1",
    y = "PC2"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 1),
    strip.background = element_rect(fill = "grey90", color = NA),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5)
  )

ggsave("loadings_pc1_pc2_new.pdf", width = 10, height = 10)
