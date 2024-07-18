# Model with spliting by users activity groups -------------------------
# # |---- Trasanction costs ----
# Fit models for each pair
model_results_cost_split <- transactional_activity_cost_split %>%
  group_by(pair_name, user_activity_group) %>%
  group_map(~fit_and_extract(.x, .y$pair_name, .y$user_activity_group ,regressor = 'transaction_cost_pct')) %>%
  set_names(
    transactional_activity_cost_split %>%
      group_by(pair_name, user_activity_group) %>%
      group_keys() %>%
      mutate(name = paste(pair_name, user_activity_group, sep = " - ")) %>%
      pull(name)
  )

# GBP-RON pair
## Low activity users
print(model_results_cost_split$`GBP-RON - low-activity-users`$nb_results)
print(model_results_cost_split$`GBP-RON - low-activity-users`$gam_results)
print(model_results_cost_split$`GBP-RON - low-activity-users`$plot)

# GBP-RON pair
## Medium activity users
print(model_results_cost_split$`GBP-RON - medium-activity-users`$nb_results)
print(model_results_cost_split$`GBP-RON - medium-activity-users`$gam_results)
print(model_results_cost_split$`GBP-RON - medium-activity-users`$plot)

# GBP-RON pair
## High activity users
print(model_results_cost_split$`GBP-RON - high-activity-users`$nb_results)
print(model_results_cost_split$`GBP-RON - high-activity-users`$gam_results)
print(model_results_cost_split$`GBP-RON - high-activity-users`$plot)

# PLN-UAH pair
## Low activity users
print(model_results_cost_split$`PLN-UAH - low-activity-users`$nb_results)
print(model_results_cost_split$`PLN-UAH - low-activity-users`$gam_results)
print(model_results_cost_split$`PLN-UAH - low-activity-users`$plot)

# PLN-UAH pair
## Medium activity users
print(model_results_cost_split$`PLN-UAH - medium-activity-users`$nb_results)
print(model_results_cost_split$`PLN-UAH - medium-activity-users`$gam_results)
print(model_results_cost_split$`PLN-UAH - medium-activity-users`$plot)

# PLN-UAH pair
## High activity users
print(model_results_cost_split$`PLN-UAH - high-activity-users`$nb_results)
print(model_results_cost_split$`PLN-UAH - high-activity-users`$gam_results)
print(model_results_cost_split$`PLN-UAH - high-activity-users`$plot)

# # |---- FX-Rate ----
# Fit models for each pair
model_results_fx_split <- transactional_activity_fx_rate_split %>%
  group_by(pair_name, user_activity_group) %>%
  group_map(~fit_and_extract(.x, .y$pair_name, .y$user_activity_group ,regressor = 'fx_rate_spread')) %>%
  set_names(
    transactional_activity_cost_split %>%
      group_by(pair_name, user_activity_group) %>%
      group_keys() %>%
      mutate(name = paste(pair_name, user_activity_group, sep = " - ")) %>%
      pull(name)
  )

# GBP-RON pair
## Low activity users
print(model_results_cost_split$`GBP-RON - low-activity-users`$nb_results)
print(model_results_cost_split$`GBP-RON - low-activity-users`$gam_results)
print(model_results_cost_split$`GBP-RON - low-activity-users`$plot)

# GBP-RON pair
## Medium activity users
print(model_results_fx_split$`GBP-RON - medium-activity-users`$nb_results)
print(model_results_fx_split$`GBP-RON - medium-activity-users`$gam_results)
print(model_results_fx_split$`GBP-RON - medium-activity-users`$plot)

# GBP-RON pair
## High activity users
print(model_results_fx_split$`GBP-RON - high-activity-users`$nb_results)
print(model_results_fx_split$`GBP-RON - high-activity-users`$gam_results)
print(model_results_fx_split$`GBP-RON - high-activity-users`$plot)

# PLN-UAH pair
## Low activity users
print(model_results_fx_split$`PLN-UAH - low-activity-users`$nb_results)
print(model_results_fx_split$`PLN-UAH - low-activity-users`$gam_results)
print(model_results_fx_split$`PLN-UAH - low-activity-users`$plot)

# PLN-UAH pair
## Medium activity users
print(model_results_fx_split$`PLN-UAH - medium-activity-users`$nb_results)
print(model_results_fx_split$`PLN-UAH - medium-activity-users`$gam_results)
print(model_results_fx_split$`PLN-UAH - medium-activity-users`$plot)

# PLN-UAH pair
## High activity users
print(model_results_fx_split$`PLN-UAH - high-activity-users`$nb_results)
print(model_results_fx_split$`PLN-UAH - high-activity-users`$gam_results)
print(model_results_fx_split$`PLN-UAH - high-activity-users`$plot)

# # |---- Print results ----


# # |---- Summary ----
# Summarize results across all pairs
summary_results_cost_split <- map_dfr(model_results_cost_split, ~tibble(
  pair = paste0(.x$pair, " - ", .x$user_activity_group),
  nb_aic = .x$aic_comparison$AIC[1],
  gam_aic = .x$aic_comparison$AIC[2],
  nb_transaction_cost_pct_effect = .x$nb_results$estimate[.x$nb_results$term == "transaction_cost_pct"],
  gam_edf = .x$gam_results$edf[.x$gam_results$term == "s(transaction_cost_pct)"]
))

summary_results_fx_split <- map_dfr(model_results_fx_split, ~tibble(
  pair = paste0(.x$pair, " - ", .x$user_activity_group),
  nb_aic = .x$aic_comparison$AIC[1],
  gam_aic = .x$aic_comparison$AIC[2],
  nb_fx_rate_effect = .x$nb_results$estimate[.x$nb_results$term == "fx_rate_spread"],
  gam_edf = .x$gam_results$edf[.x$gam_results$term == "s(fx_rate_spread)"]
))

# View summary results
print(summary_results_cost_split)
print(summary_results_fx_split)
