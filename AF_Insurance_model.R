library(decisionSupport)
# function to make variables and test the model
# used only in model construction

make_variables <- function(est,n=1)
{x <- decisionSupport::random(rho=est,n=n)
for(i in colnames(x))assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(decisionSupport::estimate_read_csv(paste("data/Agroforestry_insurance_input_table.csv",sep="")))

data <- read.csv("data/Agroforestry_insurance_input_table.csv")

# Agroforestry Insurance Decision Model Function
agroforestry_insurance_function <- function(x, varnames){
  
  # CONSTANTS
  n_years <- 20
  USD_per_ha <- 1  # Base unit
  
  # 1. BASELINE SCENARIO (no insurance) - PER HA
  baseline_income <- vv(annual_income_per_ha, var_CV, n_years)  # USD/ha/year
  
  # Typhoon events - PER HA IMPACT
  typhoon_prob <- vv(typhoon_probability, var_CV, n_years)
  typhoon_event <- chance_event(typhoon_prob, 1, 0, n_years)
  typhoon_loss_rate <- vv(typhoon_loss_rate, var_CV, n_years)
  typhoon_loss <- typhoon_event * typhoon_loss_rate * baseline_income  # USD/ha/year
  
  # Aid received - PER HA
  aid_received <- typhoon_event * vv(aid_per_ha, var_CV, n_years)  # USD/ha/year
  
  # Baseline net income - PER HA
  baseline_net <- baseline_income - typhoon_loss + pmin(aid_received, typhoon_loss)
  
  # 2. INSURANCE OPTIONS - ALL PER HA
  
  # Premiums - PER HA
  premium_index <- vv(premium_index_per_ha, var_CV, n_years)  # USD/ha/year
  premium_traditional <- vv(premium_traditional_per_ha, var_CV, n_years)
  premium_hybrid <- vv(premium_hybrid_per_ha, var_CV, n_years)
  
  # Subsidies
  subsidy_rate <- vv(subsidy_level, var_CV, n_years)
  farmer_pays_index <- premium_index * (1 - subsidy_rate)  # USD/ha/year
  farmer_pays_traditional <- premium_traditional * (1 - subsidy_rate)
  farmer_pays_hybrid <- premium_hybrid * (1 - subsidy_rate)
  
  # Payouts - PER HA
  index_payout <- ifelse(
    typhoon_event & (typhoon_loss_rate > index_trigger),
    pmin(typhoon_loss * max_payout_index, max_payout_per_ha),  # USD/ha/year
    0
  )
  
  traditional_payout <- ifelse(
    typhoon_event,
    pmin(typhoon_loss * max_payout_traditional, max_payout_per_ha),  # USD/ha/year
    0
  )
  
  hybrid_payout <- (index_payout + traditional_payout) / 2  # USD/ha/year
  
  # 3. INSURED INCOME - PER HA
  insured_net_index <- baseline_income - farmer_pays_index + index_payout  # USD/ha/year
  insured_net_traditional <- baseline_income - farmer_pays_traditional + traditional_payout
  insured_net_hybrid <- baseline_income - farmer_pays_hybrid + hybrid_payout
  
  # 4. NET BENEFIT vs. BASELINE - PER HA
  net_benefit_index <- insured_net_index - baseline_net  # USD/ha/year
  net_benefit_traditional <- insured_net_traditional - baseline_net
  net_benefit_hybrid <- insured_net_hybrid - baseline_net
  
  # 5. NPV CALCULATION - PER HA
  discount_rate_adj <- discount_rate / 100
  
  NPV_index <- discount(net_benefit_index, discount_rate_adj, calculate_NPV = TRUE)  # USD/ha total
  NPV_traditional <- discount(net_benefit_traditional, discount_rate_adj, calculate_NPV = TRUE)
  NPV_hybrid <- discount(net_benefit_hybrid, discount_rate_adj, calculate_NPV = TRUE)
  
  # 6. DECISION OUTPUT
  return(list(
    # PER HA NPVs
    NPV_index = NPV_index,
    NPV_traditional = NPV_traditional,
    NPV_hybrid = NPV_hybrid,
    
    # TOTAL FARM NPV (convert at the end)
    NPV_index_total = NPV_index * farm_size_ha,
    NPV_traditional_total = NPV_traditional * farm_size_ha,
    NPV_hybrid_total = NPV_hybrid * farm_size_ha
  
  ))
}