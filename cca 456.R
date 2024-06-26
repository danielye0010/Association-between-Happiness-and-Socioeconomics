
# Read the "456 data.csv" file into a data frame
data <- read.csv("456 data.csv")

# Extract the two sets of variables into separate data frames
economic_indicators <- data[, c("GDP", "INF", "UER", "TR", "RL", "UP", "CR", "GE", "PS")]
happiness_indicators <- data[, c("HI", "HDI")]

# Scale the data frames
scaled_economic_indicators <- scale(economic_indicators)
scaled_happiness_indicators <- scale(happiness_indicators)

# Perform PCA on the scaled economic indicators
pca_economic <- prcomp(scaled_economic_indicators, center = TRUE, scale. = TRUE)

# Function to create a scree plot
create_scree_plot <- function(pca_result, title) {
  explained_variance <- summary(pca_result)$importance["Proportion of Variance",] * 100
  num_components <- length(explained_variance)
  
  scree_data <- data.frame(
    Component = factor(1:num_components, levels = 1:num_components),
    Variance = explained_variance
  )
  
  scree_plot <- ggplot(scree_data, aes(x = Component, y = Variance)) +
    geom_col(fill = "steelblue") +
    geom_hline(yintercept = 80, linetype = "dashed", color = "red") +
    theme_minimal() +
    labs(title = title, x = "Principal Component", y = "Variance Explained (%)") +
    coord_cartesian(ylim = c(0, 100))
  
  return(scree_plot)
}

# Create scree plots
scree_plot_economic <- create_scree_plot(pca_economic, "Scree Plot - Economic Indicators")

# Display scree plots
print(scree_plot_economic)

# Determine the number of principal components to retain for economic indicators
explained_var_economic <- summary(pca_economic)$importance["Cumulative Proportion",]
num_economic_pcs <- which(explained_var_economic >= 0.8)[1]

# Determine the number of principal components to retain for happiness indicators
explained_var_happiness <- summary(pca_happiness)$importance["Cumulative Proportion",]
num_happiness_pcs <- which(explained_var_happiness >= 0.8)[1]

# Extract the reduced variables (principal components) for each set of indicators
reduced_economic_indicators <- pca_economic$x[, 1:num_economic_pcs]

# Perform Canonical Correlation Analysis using the reduced variables
cca_result <- cancor(reduced_economic_indicators, scaled_happiness_indicators)

# Display the results
print(cca_result)

# Display the number of retained variables for each set of indicators
cat("Number of retained variables for economic indicators:", num_economic_pcs, "\n")





# Extract the canonical loadings for the first canonical dimension
xcoef_first <- cca_result$xcoef[, 1]
ycoef_first <- cca_result$ycoef[, 1]

# Compute the variable scores
x_scores <- as.data.frame(reduced_economic_indicators %*% xcoef_first)
y_scores <- as.data.frame(scaled_happiness_indicators %*% ycoef_first)

# Combine the variable scores into one data frame
variable_scores <- cbind(x_scores, y_scores)
colnames(variable_scores) <- c("X_scores", "Y_scores")
rownames(variable_scores) <- c(colnames(reduced_economic_indicators), colnames(scaled_happiness_indicators))

# Convert the data frame to a long format for ggplot
variable_scores_long <- data.frame(Variable = rownames(variable_scores),
                                   X_scores = variable_scores$X_scores,
                                   Y_scores = variable_scores$Y_scores)

# Create a biplot
biplot <- ggplot(variable_scores_long, aes(x = X_scores, y = Y_scores, label = Variable)) +
  geom_point(color = "blue") +
  geom_text_repel(size = 5) +
  labs(title = "Biplot of the First Canonical Dimension",
       x = "Canonical Loading - Reduced Economic Indicators",
       y = "Canonical Loading - Scaled Happiness Indicators") +
  theme_minimal()

# Display the biplot
print(biplot)


