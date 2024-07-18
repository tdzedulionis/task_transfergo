# EDA ---------------------------------------------------------------------

# # |---- Summary stats ----
# Generate summary statistics
summary_stats_cost_whole <- summary_stats(transactional_activity_cost_whole, c("pair_name", "transaction_cost_group"))
summary_stats_fx_rate_whole <- summary_stats(transactional_activity_fx_rate_whole, c("pair_id", "fx_rate_group"))
summary_stats_cost_split <- summary_stats(transactional_activity_cost_split, c("pair_name", "transaction_cost_group", "user_activity_group"))
summary_stats_fx_rate_split <- summary_stats(transactional_activity_fx_rate_split, c("pair_id", "fx_rate_group", "user_activity_group"))

# # |---- Plots ----
# Generate plots
hist_cost_whole <- create_histogram(transactional_activity_cost_whole, transaction_count, "pair_name", "Distribution of Transaction Counts of different transaction costs")
hist_cost_split <- create_histogram(transactional_activity_cost_split, transaction_count, c("pair_name", "user_activity_group"), "Distribution of Transaction Counts of different transaction costs")
boxplot_cost <- create_boxplot(transactional_activity_cost_whole, transaction_cost_group, transaction_count, pair_name, "Transaction Counts by Transaction Cost Group")
boxplot_fx_rate <- create_boxplot(transactional_activity_fx_rate_whole, fx_rate_group, transaction_count, pair_name, "Transaction Counts by fx_rate Group")
scatter_cost <- create_scatter(transactional_activity_cost_split, transaction_cost_pct, transaction_count, user_activity_group, pair_name, "Transaction Count vs. Transaction cost [%]")
scatter_fx_rate <- create_scatter(transactional_activity_fx_rate_split, fx_rate_spread, transaction_count, user_activity_group, pair_name, "Transaction Count vs. fx_rate_spread")


# # |---- Statistical tests ----
# Perform ANOVA tests
results_anova_transaction_costs <- list(
  GBP_RON = perform_anova(transactional_activity_cost_whole, pair_id = 1, col = 'transaction_cost_group'),
  PLN_UAH = perform_anova(transactional_activity_cost_whole, pair_id = 2, col = 'transaction_cost_group'),
  GBP_RON_UAG1 = perform_anova(transactional_activity_cost_split, pair_id = 1, user_activity_group = 1, col = 'transaction_cost_group'),
  GBP_RON_UAG2 = perform_anova(transactional_activity_cost_split, pair_id = 1, user_activity_group = 2, col = 'transaction_cost_group'),
  GBP_RON_UAG3 = perform_anova(transactional_activity_cost_split, pair_id = 1, user_activity_group = 3, col = 'transaction_cost_group'),
  PLN_UAH_UAG1 = perform_anova(transactional_activity_cost_split, pair_id = 2, user_activity_group = 1, col = 'transaction_cost_group'),
  PLN_UAH_UAG2 = perform_anova(transactional_activity_cost_split, pair_id = 2, user_activity_group = 2, col = 'transaction_cost_group'),
  PLN_UAH_UAG3 = perform_anova(transactional_activity_cost_split, pair_id = 2, user_activity_group = 3, col = 'transaction_cost_group')
)


results_anova_fx_rate <- list(
  GBP_RON = perform_anova(transactional_activity_fx_rate_whole, pair_id = 1, col = 'fx_rate_group'),
  PLN_UAH = perform_anova(transactional_activity_fx_rate_whole, pair_id = 2, col = 'fx_rate_group'),
  GBP_RON_UAG1 = perform_anova(transactional_activity_fx_rate_split, pair_id = 1, user_activity_group = 1, col = 'fx_rate_group'),
  GBP_RON_UAG2 = perform_anova(transactional_activity_fx_rate_split, pair_id = 1, user_activity_group = 2, col = 'fx_rate_group'),
  GBP_RON_UAG3 = perform_anova(transactional_activity_fx_rate_split, pair_id = 1, user_activity_group = 3, col = 'fx_rate_group'),
  PLN_UAH_UAG1 = perform_anova(transactional_activity_fx_rate_split, pair_id = 2, user_activity_group = 1, col = 'fx_rate_group'),
  PLN_UAH_UAG2 = perform_anova(transactional_activity_fx_rate_split, pair_id = 2, user_activity_group = 2, col = 'fx_rate_group'),
  PLN_UAH_UAG3 = perform_anova(transactional_activity_fx_rate_split, pair_id = 2, user_activity_group = 3, col = 'fx_rate_group')
)

