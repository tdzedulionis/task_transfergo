# # Input -------------------------------------------------------------------
# # |---- Read data ----

# Read the CSV file
data <- fread(data_path) %>%
  # Calculate transaction cost (%)
  .[, `:=`(
    date = as.Date(created_at),
    transaction_cost_pct = round((payment_amount * (fx_rate - mid_market_fx_rate)) / (payment_amount * mid_market_fx_rate) * 100, 4),
    transaction_cost_pct_with_delivery_fee = round((payment_amount * (fx_rate - mid_market_fx_rate) + delivery_option_fee) / (payment_amount * mid_market_fx_rate) * 100, 4),
    fx_rate = round(fx_rate, 3),
    spread = fx_rate - mid_market_fx_rate
  )] %>%
  # Create a unique identifier for each country-currency pair
  .[, pair_id := .GRP, by = .(from_country, to_country)]


# Calculate aggregate revenue margin on daily level
revenue_margin <- data %>%
  .[, created_at := as.Date(created_at)] %>%
  .[, `:=`(
    revenue = payment_amount + delivery_option_fee,
    costs = (payment_amount * fx_rate) / mid_market_fx_rate
  )] %>%
  .[, .(
    daily_revenue = sum(revenue),
    daily_costs = sum(costs)
  ), by = created_at] %>%
  .[, daily_revenue_margin := (daily_revenue - daily_costs) / daily_revenue * 100]


# Clustering in groups and additional calcs -------------------------------

daily_transactions <- copy(data) %>%
  .[, .(
    transaction_count = .N,
    total_amount = sum(payment_amount),
    avg_transaction_cost_pct = mean(transaction_cost_pct)
  ), by = .(date, from_country, to_country, user_id)]

# Calculate activity metrics by user
user_activity <- data[, .(
  transaction_count = .N,
  total_volume = sum(payment_amount),
  avg_cost_pct_by_user = mean(transaction_cost_pct),
  first_transaction = min(created_at),
  last_transaction = max(created_at)
), by = .(user_id, pair_id)] %>%
  split(by = "pair_id") %>%
  # K-Means for clustering users by activity group (total_volume and transaction_count)
  map(~ perform_kmeans(., 
                       columns = c("total_volume", "transaction_count"), 
                       n_centers = 3, 
                       group_col_name = "user_activity_group")) %>%
  rbindlist() %>%
  .[, user_activity_group := fcase(
    user_activity_group == 1, 'low-activity-users',
    user_activity_group == 2, 'medium-activity-users',
    default = 'high-activity-users'
  )]

data <- merge(data, user_activity, by = c("user_id", "pair_id"))

# Calculate time since last transaction for each transaction
data <- data %>%
  .[order(user_id, created_at), time_since_last := c(NA, diff(created_at)), by = user_id]

# Calculate transactional activity at different transaction costs without splitting by users activity
transactional_activity_cost_whole <- copy(data) %>%
  .[, .(
    total_payment_amount = sum(payment_amount),
    transaction_count = .N
  ), by = .(transaction_cost_pct, pair_id)] %>%
  # Split transaction costs in low-medium-high categories
  .[, transaction_cost_group := cut(transaction_cost_pct, breaks = 3, labels = FALSE),
    by = .(pair_id)] %>%
  .[, pair_name := fcase(
    pair_id == 1, 'GBP-RON',
    pair_id == 2, 'PLN-UAH'
  )] %>%
  .[, transaction_cost_group := fcase(
    transaction_cost_group == 1, 'low-cost',
    transaction_cost_group == 2, 'medium-cost',
    default = 'high-cost'
  )] %>%
  group_by(pair_id) %>%
  # Remove outliers
  group_modify(~ remove_outliers(.x, column = "transaction_count", factor = 1.5)) %>%
  ungroup()

# Calculate transactional activity at different fx-rates without splitting by users activity
transactional_activity_fx_rate_whole <- copy(data) %>%
  .[, .(
    total_payment_amount = sum(payment_amount),
    transaction_count = .N
  ), by = .(fx_rate, pair_id)] %>%
  # Split fx-rate in low-medium-high categories
  .[, fx_rate_group := cut(fx_rate, breaks = 3, labels = FALSE),
    by = .(pair_id)] %>%
  .[, pair_name := fcase(
    pair_id == 1, 'GBP-RON',
    pair_id == 2, 'PLN-UAH'
  )] %>%
  .[, fx_rate_group := fcase(
    fx_rate_group == 1, 'low-fx-rate',
    fx_rate_group == 2, 'medium-fx-rate',
    default = 'high-fx-rate'
  )] %>%
  group_by(pair_id) %>%
  # Remove outliers
  group_modify(~ remove_outliers(.x, column = "transaction_count", factor = 1.5)) %>%
  ungroup()

# Calculate transactional activity at different transaction costs with splitting by users activity
transactional_activity_cost_split <- copy(data) %>%
  .[, .(
    total_payment_amount = sum(payment_amount),
    transaction_count = .N
  ), by = .(transaction_cost_pct, pair_id, user_activity_group)] %>%
  # Split transaction costs in low-medium-high categories
  .[, transaction_cost_group := cut(transaction_cost_pct, breaks = 3, labels = FALSE),
    by = .(pair_id)] %>%
  .[, pair_name := fcase(
    pair_id == 1, 'GBP-RON',
    pair_id == 2, 'PLN-UAH'
  )] %>%
  .[, transaction_cost_group := fcase(
    transaction_cost_group == 1, 'low-cost',
    transaction_cost_group == 2, 'medium-cost',
    default = 'high-cost'
  )] %>%
  group_by(pair_id, user_activity_group) %>%
  # Remove outliers
  group_modify(~ remove_outliers(.x, column = "transaction_count", factor = 1.5)) %>%
  ungroup()

# Calculate transactional activity at different fx-rates with splitting by users activity
transactional_activity_fx_rate_split <- copy(data) %>%
  .[, .(
    total_payment_amount = sum(payment_amount),
    transaction_count = .N
  ), by = .(fx_rate, pair_id, user_activity_group)] %>%
  # Split fx-rate in low-medium-high categories
  .[, fx_rate_group := cut(fx_rate, breaks = 3, labels = FALSE),
    by = .(pair_id)] %>%
  .[, pair_name := fcase(
    pair_id == 1, 'GBP-RON',
    pair_id == 2, 'PLN-UAH'
  )] %>%
  .[, fx_rate_group := fcase(
    fx_rate_group == 1, 'low-fx-rate',
    fx_rate_group == 2, 'medium-fx-rate',
    default = 'high-fx-rate'
  )] %>%
  group_by(pair_id, user_activity_group) %>%
  # Remove outliers
  group_modify(~ remove_outliers(.x, column = "transaction_count", factor = 1.5)) %>%
  ungroup()
