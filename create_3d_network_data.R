# Create JSON data for 3D force-directed graph
setwd("/Users/davidwhyatt/Downloads/essen_new")
library(tidyverse)
library(psych)
library(jsonlite)

# Load and prepare data
features <- read_csv("essen_corpus_features_NEW.csv", show_col_types = FALSE)
features_numeric <- features %>% select(where(is.numeric)) %>% select(-melody_num)
features_numeric_clean <- features_numeric %>% drop_na()

variances <- sapply(features_numeric_clean, var)
zero_var_cols <- names(variances[variances == 0 | is.na(variances)])
features_numeric_clean <- features_numeric_clean %>% select(-any_of(zero_var_cols))
features_numeric_scaled <- as.data.frame(scale(features_numeric_clean))
features_numeric <- features_numeric_scaled

# Factor analysis
Nfacs <- 17
fit <- fa(features_numeric, nfactors=Nfacs, rotate="promax", fm="pa")
loadings_matrix <- as.matrix(fit$loadings)
rownames(loadings_matrix) <- colnames(features_numeric)

# Custom factor names
factor_names <- c(
  "Timing", "Timing Variability", "Melodic Complexity", "Pitch Complexity",
  "Intervallic Complexity", "Timing Pulses", "Pitch Variability", "Pitch Expectancy",
  "Corpus Summary", "Lexical Diversity", "Tonality", "Pitch Summary",
  "Pitch Commonality", "Timing Rests", "Timing Summary", "Pitch Contour", "Strongest Pulses"
)

# Build network data
cutoff <- 0.3
nodes <- list()
links <- list()

# Add factor nodes
for (i in 1:Nfacs) {
  nodes[[length(nodes) + 1]] <- list(
    id = paste0("F", i),
    name = factor_names[i],
    type = "factor",
    val = 25
  )
}

# Add variable nodes and links
for (i in 1:Nfacs) {
  factor_loadings <- loadings_matrix[, i]
  sig_idx <- abs(factor_loadings) > cutoff
  
  if (sum(sig_idx) > 0) {
    sig_vars <- names(factor_loadings)[sig_idx]
    sig_values <- factor_loadings[sig_idx]
    
    for (j in 1:length(sig_vars)) {
      var_name <- sig_vars[j]
      loading_value <- sig_values[j]
      
      # Check if variable node already exists
      existing <- FALSE
      for (node in nodes) {
        if (node$id == var_name) {
          existing <- TRUE
          break
        }
      }
      
      # Add variable node if new
      if (!existing) {
        nodes[[length(nodes) + 1]] <- list(
          id = var_name,
          name = var_name,
          type = "variable",
          val = 8
        )
      }
      
      # Add link
      links[[length(links) + 1]] <- list(
        source = paste0("F", i),
        target = var_name,
        value = abs(loading_value),
        sign = ifelse(loading_value > 0, "positive", "negative")
      )
    }
  }
}

# Create network data structure
network_data <- list(
  nodes = nodes,
  links = links
)

# Save to JSON
cat("Saving network data to JSON...\n")
json_data <- toJSON(network_data, auto_unbox = TRUE, pretty = TRUE)
write(json_data, "docs/network_data.json")

cat("\nNetwork data created:\n")
cat("  - Nodes:", length(nodes), "\n")
cat("  - Links:", length(links), "\n")
cat("  - Saved to: docs/network_data.json\n")
