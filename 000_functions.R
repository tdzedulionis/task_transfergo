# Functions ---------------------------------------------------------------
# Function to remove outliers
remove_outliers <- function(data, column = "transaction_count", factor = 1.5) {
  
  # Calculate Q1, Q3, and IQR
  Q1 <- quantile(data[[column]], 0.25)
  Q3 <- quantile(data[[column]], 0.75)
  IQR <- Q3 - Q1
  
  # Define lower and upper bounds
  lower_bound <- Q1 - factor * IQR
  upper_bound <- Q3 + factor * IQR
  
  # Remove outliers
  data_clean <- data[data[[column]] >= lower_bound & data[[column]] <= upper_bound, ]
  
  removed_count <- nrow(data) - nrow(data_clean)
  removed_percentage <- removed_count / nrow(data) * 100
  
  cat(sprintf("Removed %d rows as outliers (%.2f%% of the data)\n", removed_count, removed_percentage))
  
  data_clean
}

# Function for summary statistics
summary_stats <- function(df, group_cols) {
  df %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(across(
      transaction_count,
      list(
        mean = ~mean(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE),
        total = ~sum(., na.rm = TRUE)
      )
    ))
}

# Function to perform k-means on a subset
perform_kmeans <- function(subset, columns, n_centers = 3, group_col_name = "cluster_group") {
  kmeans_result <- kmeans(subset[, columns, with = FALSE], centers = n_centers)
  subset[, (group_col_name) := kmeans_result$cluster]
  subset
}

# Function to create histogram
create_histogram <- function(data, x_var, facet_vars, title) {
  ggplot(data, aes(x = {{x_var}})) +
    geom_histogram(fill = "blue", color = "black") +
    facet_wrap(vars(!!!syms(facet_vars))) +
    theme_minimal() +
    labs(title = title, x = deparse(substitute(x_var)), y = "Frequency")
}

# Function to create boxplot
create_boxplot <- function(data, x_var, y_var, facet_var, title) {
  ggplot(data, aes(x = factor({{x_var}}), y = log({{y_var}}))) +
    geom_boxplot(fill = "blue", color = "black") +
    facet_wrap(vars({{facet_var}})) +
    theme_minimal() +
    labs(title = title, x = deparse(substitute(x_var)), y = deparse(substitute(y_var)))
}

# Function to create scatter plot
create_scatter <- function(data, x_var, y_var, color_var, facet_var, title) {
  ggplot(data, aes(x = {{x_var}}, y = {{y_var}}, color = factor({{color_var}}))) +
    geom_point() +
    facet_wrap(vars({{facet_var}})) +
    theme_minimal() +
    labs(title = title, x = deparse(substitute(x_var)), y = deparse(substitute(y_var))) +
    theme(legend.position="bottom")
}


# Function to perform ANOVA tests
perform_anova <- function(data, pair_id, col, user_activity_group = NULL) {
  subset_data <- data %>% 
    filter(pair_id == pair_id) %>%
    {if (!is.null(user_activity_group)) filter(., user_activity_group == user_activity_group) else .}
  
  anova_result <- aov(transaction_count ~ factor(eval(sym(col))), data = subset_data)
  anova_summary <- tidy(anova_result)
  tukey_result <- TukeyHSD(anova_result)
  tukey_summary <- tidy(tukey_result)
  
  list(anova_summary = anova_summary, tukey_summary = tukey_summary)
}

# Function to fit models and extract results
fit_and_extract <- function(data, pair, user_group = NULL, regressor) {
  # Negative Binomial Model
  nb_formula <- as.formula(paste("transaction_count ~", regressor))
  nb_model <- glm.nb(nb_formula, data = data)
  
  # GAM Model
  gam_formula <- as.formula(paste("transaction_count ~ s(", regressor, ")"))
  gam_model <- gam(gam_formula, family = nb(), data = data)
  
  # Extract results
  nb_results <- tidy(nb_model)
  gam_results <- tidy(gam_model)
  aic_comparison <- tibble(
    model = c("Negative Binomial", "GAM"),
    AIC = c(AIC(nb_model), AIC(gam_model))
  )
  
  # Generate predictions for NB model plot
  new_data <- tibble(!!sym(regressor) := seq(min(data[[regressor]]), 
                                             max(data[[regressor]]), 
                                             length.out = 100))
  
  nb_predictions <- predict(nb_model, newdata = new_data, type = "response")
  
  # Create a combined data frame for the lines
  lines_data <- tibble(
    !!sym(regressor) := as.data.frame(new_data)[, 1],
    nb_predictions = nb_predictions,
    gam_predictions = predict(gam_model, new_data)
  )
  
  # Plot with legend
  plot <- ggplot(data, aes_string(x = regressor, y = "transaction_count")) +
    geom_point(alpha = 0.1) +
    geom_line(data = lines_data, aes_string(x = regressor, y = "nb_predictions", color = '"Negative Binomial Model"'), size = 1) +
    geom_smooth(method = "gam", formula = y ~ s(x), method.args = list(family = nb), aes_string(color = '"GAM Model"'), size = 1) +
    labs(
      title = paste("GAM and NB models -", pair, "-", user_group),
      x = paste0(regressor),
      y = "Transaction Count",
      color = "Model"
    ) +
    scale_color_manual(values = c("Negative Binomial Model" = "forestgreen", "GAM Model" = "steelblue"))
  
  # Display the plot
  print(plot)
  
  list(
    pair = pair,
    user_activity_group = user_group,
    nb_results = nb_results,
    gam_results = gam_results,
    aic_comparison = aic_comparison,
    plot = plot
  )
}