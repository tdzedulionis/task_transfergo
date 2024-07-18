# Model without spliting by users activity groups -------------------------

# # |---- Trasanction costs ----
# Fit models for each pair
model_results_cost <- transactional_activity_cost_whole %>%
  group_by(pair_name) %>%
  group_map(~fit_and_extract(.x, .y$pair_name, regressor = 'transaction_cost_pct')) %>%
  set_names(
    transactional_activity_cost_split %>%
      group_by(pair_name) %>%
      group_keys() %>%
      mutate(name = paste(pair_name, sep = " - ")) %>%
      pull(name)
  )

# GBP-RON pair
print(model_results_cost$`GBP-RON`$nb_results)
print(model_results_cost$`GBP-RON`$gam_results)
print(model_results_cost$`GBP-RON`$plot)

# PLN-UAH pair
print(model_results_cost$`PLN-UAH`$nb_results)
print(model_results_cost$`PLN-UAH`$gam_results)
print(model_results_cost$`PLN-UAH`$plot)

# # |---- FX-Rate ----
# Fit models for each pair
model_results_fx <- transactional_activity_fx_rate_whole %>%
  group_by(pair_name) %>%
  group_map(~fit_and_extract(.x, .y$pair_name, regressor = 'fx_rate')) %>%
  set_names(
    transactional_activity_cost_split %>%
      group_by(pair_name) %>%
      group_keys() %>%
      mutate(name = paste(pair_name, sep = " - ")) %>%
      pull(name)
  )

# GBP-RON pair
print(model_results_fx$`GBP-RON`$nb_results)
print(model_results_fx$`GBP-RON`$gam_results)
print(model_results_fx$`GBP-RON`$plot)

# PLN-UAH pair
print(model_results_fx$`PLN-UAH`$nb_results)
print(model_results_fx$`PLN-UAH`$gam_results)
print(model_results_fx$`PLN-UAH`$plot)

# # |---- Summary ----

# Summarize results across all pairs
summary_results_cost <- map_dfr(model_results_cost, ~tibble(
  pair = paste0(.x$pair, " - ", .x$user_activity_group),
  nb_aic = .x$aic_comparison$AIC[1],
  gam_aic = .x$aic_comparison$AIC[2],
  nb_transaction_cost_pct_effect = .x$nb_results$estimate[.x$nb_results$term == "transaction_cost_pct"],
  gam_edf = .x$gam_results$edf[.x$gam_results$term == "s(transaction_cost_pct)"]
))

summary_results_fx <- map_dfr(model_results_fx, ~tibble(
  pair = paste0(.x$pair, " - ", .x$user_activity_group),
  nb_aic = .x$aic_comparison$AIC[1],
  gam_aic = .x$aic_comparison$AIC[2],
  nb_fx_rate_effect = .x$nb_results$estimate[.x$nb_results$term == "fx_rate"],
  gam_edf = .x$gam_results$edf[.x$gam_results$term == "s(fx_rate)"]
))

# View summary results
print(summary_results_cost)
print(summary_results_fx)

