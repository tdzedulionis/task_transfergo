fit_and_extract <- function(data, pair, user_group = NULL, regressor) {
  
  # Negative Binomial Model
  nb_formula <- as.formula(paste("transaction_count ~ ", regressor))
  nb_model <- glm.nb(nb_formula, data = data)
  
  # GAM Model
  gam_formula <- as.formula(paste("transaction_count ~ s(", regressor, ")"))
  
  # Fit the GAM model
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
  
  # NB model plot
  # Create a combined data frame for the lines
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
      x = "Transaction Cost Percentage",
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
  
  # Print summary of removed data
  removed_count <- nrow(data) - nrow(data_clean)
  removed_percentage <- removed_count / nrow(data) * 100
  
  cat(sprintf("Removed %d rows (%.2f%% of the data)\n", removed_count, removed_percentage))
  
  return(data_clean)
}

test <- transactional_activity_fx_rate_whole %>%
  group_by(pair_id) %>%
  group_modify(~ remove_outliers(.x, column = "transaction_count", factor = 1.5)) %>%
  ungroup()

+# Apply the function to your grouped data
  transactional_activity_cost_whole <- transactional_activity_cost_whole %>%
  group_by(pair_name) %>%
  remove_outliers_grouped("transaction_count", factor = 1.5)

transactional_activity_cost_whole <- remove_outliers(transactional_activity_cost_whole, "transaction_count", factor = 1.5)


model_results <- transactional_activity_cost_split %>%
  group_by(pair_name, user_activity_group) %>%
  group_map(~fit_and_extract(.x, .y$pair_name, .y$user_activity_group ,regressor = 'transaction_cost_pct')) %>%
  set_names(
    transactional_activity_cost_split %>%
      group_by(pair_name, user_activity_group) %>%
      group_keys() %>%
      mutate(name = paste(pair_name, user_activity_group, sep = " - ")) %>%
      pull(name)
  )
