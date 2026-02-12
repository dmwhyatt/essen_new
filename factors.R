setwd("/Users/davidwhyatt/Downloads/essen_new")
library(tidyverse)
library(psych)

features <- read_csv("essen_corpus_features_NEW.csv")
features_numeric <- features %>% select(where(is.numeric)) %>% select(-melody_num)

features_numeric_clean <- features_numeric %>% drop_na()

variances <- sapply(features_numeric_clean, var)
zero_var_cols <- names(variances[variances == 0 | is.na(variances)])

cat("Columns with zero/constant variance:\n")
print(zero_var_cols)

features_numeric_clean <- features_numeric_clean %>% 
  select(-any_of(zero_var_cols))

features_numeric_scaled <- as.data.frame(scale(features_numeric_clean))
features_numeric <- features_numeric_scaled

cat("\nVariables (original):", ncol(features_numeric), "\n")
cat("Sample size:", nrow(features_numeric), "\n\n")


# check condition number of correlation matrix
cor_matrix <- cor(features_numeric)
condition_number <- kappa(cor_matrix)
cat("Condition number:", format(condition_number, scientific=TRUE), "\n")
if (condition_number > 1e10) {
  cat("WARNING: Correlation matrix is still poorly conditioned\n")
}
cat("\n")


pa_result <- fa.parallel(features_numeric, fa="fa", plot=TRUE, n.iter=100)

cat("Suggested number of factors:", pa_result$nfact, "\n\n")

Nfacs <- 17 # from screeplot elbow
cat("Extracting", Nfacs, "factors\n\n")

fit <- fa(features_numeric, nfactors=Nfacs, rotate="promax", fm="pa")

if (!is.null(fit$Phi)) {
  factor_correlations <- fit$Phi
} else {
  cat("Phi matrix not available, computing from factor scores...\n")
  factor_scores <- factor.scores(features_numeric, fit)$scores
  factor_correlations <- cor(factor_scores)
}

factor_correlations <- as.matrix(factor_correlations)

diag(factor_correlations) <- 1.0

factor_correlations[is.na(factor_correlations)] <- 0

rownames(factor_correlations) <- paste0("F", 1:Nfacs)
colnames(factor_correlations) <- paste0("F", 1:Nfacs)

cat("\nFactor Correlation Matrix (Phi):\n")
print(round(factor_correlations, 3))

high_corr_threshold <- 0.3
high_corr_pairs <- which(abs(factor_correlations) > high_corr_threshold & 
                         abs(factor_correlations) < 1, arr.ind = TRUE)

if (nrow(high_corr_pairs) > 0) {
  cat("\nHighly correlated factor pairs (|r| >", high_corr_threshold, "):\n")
  unique_pairs <- high_corr_pairs[high_corr_pairs[,1] < high_corr_pairs[,2], , drop=FALSE]
  
  if (nrow(unique_pairs) > 0) {
    for (i in 1:nrow(unique_pairs)) {
      f1 <- unique_pairs[i, 1]
      f2 <- unique_pairs[i, 2]
      corr <- factor_correlations[f1, f2]
      cat(sprintf("  F%d <-> F%d: r = %.3f\n", f1, f2, corr))
    }
  }
} else {
  cat("\nNo highly correlated factor pairs found.\n")
}

library(ggplot2)
library(reshape2)

cat("\nDiagonal values:", diag(factor_correlations), "\n")

factor_corr_melted <- melt(factor_correlations)
colnames(factor_corr_melted) <- c("Factor1", "Factor2", "Correlation")

diagonal_idx <- factor_corr_melted$Factor1 == factor_corr_melted$Factor2
factor_corr_melted$Correlation[diagonal_idx] <- 1.0

p_corr <- ggplot(factor_corr_melted, aes(x = Factor1, y = Factor2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", Correlation), 
                color = abs(Correlation) > 0.5), size = 3) +
  scale_color_manual(values = c("black", "white"), guide = "none") +
  scale_fill_gradient2(low = "#3498db", mid = "#ecf0f1", high = "#e74c3c", 
                      midpoint = 0, limits = c(-1, 1)) +
  labs(
    title = "Factor Correlation Matrix (Promax Rotation)",
    subtitle = paste0(Nfacs, " factors from ", ncol(features_numeric), " variables"),
    x = "", y = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14),
    panel.grid = element_blank()
  ) +
  coord_fixed()
print(p_corr)

# interactive network diagram
library(visNetwork)

cat("Creating interactive network factor diagram...\n")

loadings_matrix <- as.matrix(fit$loadings)
rownames(loadings_matrix) <- colnames(features_numeric)

# custom factor names
factor_names <- c(
  "1. Timing",
  "2. Timing Variability", 
  "3. Melodic Complexity",
  "4. Pitch Complexity",
  "5. Intervallic Complexity",
  "6. Timing Pulses",
  "7. Pitch Variability",
  "8. Pitch Expectancy",
  "9. Corpus Summary",
  "10. Lexical Diversity",
  "11. Tonality",
  "12. Pitch Summary",
  "13. Pitch Commonness",
  "14. Timing Rests",
  "15. Timing Summary",
  "16. Pitch Contour",
  "17. Strongest Pulses"
)

if (length(factor_names) > Nfacs) {
  factor_names <- factor_names[1:Nfacs]
} else if (length(factor_names) < Nfacs) {
  for (i in (length(factor_names) + 1):Nfacs) {
    factor_names[i] <- paste0("Factor ", i)
  }
}

cat("\nUsing factor names:\n")
for (i in 1:length(factor_names)) {
  cat(sprintf("  F%d: %s\n", i, factor_names[i]))
}
cat("\n")

# function to create interactive network diagram
create_network_diagram <- function(loadings, cutoff = 0.3, max_factors = NULL, custom_names = NULL) {
  
  if (is.null(max_factors)) {
    max_factors <- ncol(loadings)
  }
  
  # Use custom names if provided
  if (is.null(custom_names)) {
    factor_labels <- paste0("Factor ", 1:max_factors)
  } else {
    factor_labels <- custom_names[1:max_factors]
  }
  
  # Create nodes for factors
  factor_nodes <- data.frame(
    id = paste0("F", 1:max_factors),
    label = factor_labels,
    group = "factor",
    shape = "ellipse",
    color = "#ff7f0e",
    size = 30,
    font.size = 20,
    title = factor_labels
  )
  
  # Create nodes and edges for variables
  var_nodes_list <- list()
  edges_list <- list()
  
  for (i in 1:max_factors) {
    factor_loadings <- loadings[, i]
    sig_idx <- abs(factor_loadings) > cutoff
    
    if (sum(sig_idx) > 0) {
      sig_vars <- names(factor_loadings)[sig_idx]
      sig_values <- factor_loadings[sig_idx]
      
      for (j in 1:length(sig_vars)) {
        var_name <- sig_vars[j]
        loading_value <- sig_values[j]
        
        # Add variable node if not already added
        if (!var_name %in% names(var_nodes_list)) {
          var_nodes_list[[var_name]] <- data.frame(
            id = var_name,
            label = var_name,
            group = "variable",
            shape = "box",
            color = "#1f77b4",
            size = 20,
            font.size = 12,
            title = var_name
          )
        }
        
        # Add edge
        edge_color <- ifelse(loading_value > 0, "green", "red")
        edges_list[[length(edges_list) + 1]] <- data.frame(
          from = paste0("F", i),
          to = var_name,
          value = abs(loading_value) * 10,  # Scale for visibility
          title = paste0("Loading: ", round(loading_value, 3)),
          color = edge_color,
          arrows = "to"
        )
      }
    }
  }
  
  # Combine all nodes
  var_nodes <- do.call(rbind, var_nodes_list)
  all_nodes <- rbind(factor_nodes, var_nodes)
  all_edges <- do.call(rbind, edges_list)
  
  # Create the network
  network <- visNetwork(all_nodes, all_edges, width = "100%", height = "800px") %>%
    visGroups(groupname = "factor", color = "#ff7f0e", shape = "ellipse") %>%
    visGroups(groupname = "variable", color = "#1f77b4", shape = "box") %>%
    visOptions(
      highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
      nodesIdSelection = TRUE,
      selectedBy = "group"
    ) %>%
    visLayout(randomSeed = 123) %>%
    visPhysics(
      solver = "forceAtlas2Based",
      forceAtlas2Based = list(gravitationalConstant = -50)
    ) %>%
    visInteraction(
      navigationButtons = TRUE,
      dragNodes = TRUE,
      dragView = TRUE,
      zoomView = TRUE
    ) %>%
    visLegend(width = 0.1, position = "right", main = "Node Type")
  
  return(network)
}

# full diagram with all factors
cat("Creating full network diagram...\n")
full_diagram <- create_network_diagram(loadings_matrix, cutoff = 0.3, max_factors = Nfacs, custom_names = factor_names)
visSave(full_diagram, "factor_network_full.html", selfcontained = FALSE)
