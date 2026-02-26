setwd("/Users/davidwhyatt/Downloads/essen_new")
library(tidyverse)
library(psych)
library(threejs)

# Load and prepare data (same as factors.R)
features <- read_csv("essen_corpus_features_NEW.csv")
features_numeric <- features %>% select(where(is.numeric)) %>% select(-melody_num)
features_numeric_clean <- features_numeric %>% drop_na()

variances <- sapply(features_numeric_clean, var)
zero_var_cols <- names(variances[variances == 0 | is.na(variances)])
features_numeric_clean <- features_numeric_clean %>% select(-any_of(zero_var_cols))
features_numeric_scaled <- as.data.frame(scale(features_numeric_clean))
features_numeric <- features_numeric_scaled

cat("\nVariables (original):", ncol(features_numeric), "\n")
cat("Sample size:", nrow(features_numeric), "\n\n")

# Factor analysis
Nfacs <- 17
cat("Extracting", Nfacs, "factors\n\n")
fit <- fa(features_numeric, nfactors=Nfacs, rotate="promax", fm="pa")

loadings_matrix <- as.matrix(fit$loadings)
rownames(loadings_matrix) <- colnames(features_numeric)

# Custom factor names
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
  "13. Pitch Commonality",
  "14. Timing Rests",
  "15. Timing Summary",
  "16. Pitch Contour",
  "17. Strongest Pulses"
)

cat("Creating 3D interactive network diagram...\n")

# Build network structure
cutoff <- 0.3

# Prepare nodes and edges
all_nodes <- list()
node_colors <- character()
node_sizes <- numeric()
node_labels <- character()
edges_from <- integer()
edges_to <- integer()
edge_colors <- character()
edge_widths <- numeric()

node_id_map <- list()
current_id <- 1

# Add factor nodes
for (i in 1:Nfacs) {
  node_id_map[[paste0("F", i)]] <- current_id
  all_nodes[[current_id]] <- paste0("F", i)
  node_labels[current_id] <- factor_names[i]
  node_colors[current_id] <- "orange"  # Factor nodes
  node_sizes[current_id] <- 3  # Larger for factors
  current_id <- current_id + 1
}

# Add variable nodes and edges
for (i in 1:Nfacs) {
  factor_loadings <- loadings_matrix[, i]
  sig_idx <- abs(factor_loadings) > cutoff
  
  if (sum(sig_idx) > 0) {
    sig_vars <- names(factor_loadings)[sig_idx]
    sig_values <- factor_loadings[sig_idx]
    
    for (j in 1:length(sig_vars)) {
      var_name <- sig_vars[j]
      loading_value <- sig_values[j]
      
      # Add variable node if not already added
      if (!(var_name %in% names(node_id_map))) {
        node_id_map[[var_name]] <- current_id
        all_nodes[[current_id]] <- var_name
        node_labels[current_id] <- var_name
        node_colors[current_id] <- "steelblue"  # Variable nodes
        node_sizes[current_id] <- 1.5  # Smaller for variables
        current_id <- current_id + 1
      }
      
      # Add edge (from factor to variable)
      factor_id <- node_id_map[[paste0("F", i)]]
      var_id <- node_id_map[[var_name]]
      
      edges_from <- c(edges_from, factor_id)
      edges_to <- c(edges_to, var_id)
      edge_colors <- c(edge_colors, ifelse(loading_value > 0, "green", "red"))
      edge_widths <- c(edge_widths, abs(loading_value) * 5)  # Scale for visibility
    }
  }
}

cat("Total nodes:", length(all_nodes), "\n")
cat("  - Factors:", Nfacs, "\n")
cat("  - Variables:", length(all_nodes) - Nfacs, "\n")
cat("Total edges:", length(edges_from), "\n\n")

# Create 3D force-directed layout using igraph
library(igraph)

# Build igraph object
g <- graph_from_data_frame(
  d = data.frame(from = edges_from, to = edges_to),
  directed = TRUE,
  vertices = data.frame(
    id = 1:length(all_nodes),
    name = unlist(all_nodes),
    label = node_labels
  )
)

# Calculate 3D layout using Fruchterman-Reingold in 3D
set.seed(123)
layout_3d <- layout_with_fr(g, dim = 3, niter = 500)

# Scale layout to reasonable bounds
layout_3d <- layout_3d * 50

# Create the 3D visualization
cat("Generating 3D visualization...\n")

graph_3d <- graphjs(
  g,
  layout = layout_3d,
  vertex.color = node_colors,
  vertex.size = node_sizes,
  vertex.label = node_labels,
  edge.color = edge_colors,
  edge.width = edge_widths,
  edge.alpha = 0.6,
  main = "3D Factor Network Visualization",
  bg = "white",
  showLabels = TRUE,
  stroke = FALSE
)

# Save to HTML file
cat("Saving 3D network to docs/factor_network_3d.html...\n")
htmlwidgets::saveWidget(
  graph_3d, 
  "docs/factor_network_3d.html",
  selfcontained = FALSE,
  title = "3D Factor Network"
)

cat("\n3D network diagram created successfully!\n")
cat("Open docs/factor_network_3d.html to view the interactive 3D visualization.\n")
cat("\nControls:\n")
cat("  - Click and drag to rotate\n")
cat("  - Scroll to zoom\n")
cat("  - Click nodes to highlight connections\n")
