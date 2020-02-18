sandbox_configuration = config::get("postgres_sandbox", config = "dev", file = "enva_config.yml")

market_quantile_breaks = c(0, 0.5, 0.75, 1)

## Default plan price
plan_price = 33
plan_price_annual = 198
net_percentage = 0.485
paid_search_ltv_placeholder = 150

# See eda/10_24_months_ltv_with_annual/24_months_ltv.html for more information
paid_search_30_months_adjustment = 1.12
