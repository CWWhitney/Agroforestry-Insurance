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
  
  # Insurance Scheme Selection
  # Four main insurance types: none, index-based, traditional, and hybrid
  
  # BASELINE SCENARIO: No Insurance (Farmer's current reality)
  baseline_annual_income <- vv(annual_income_per_ha, var_CV, n_years)  # USD/ha/year
  
  # Typhoon impacts
  typhoon_occurrence <- chance_event(typhoon_probability, value_if = 1, value_if_not = 0, n = n_years)
  typhoon_loss_percentage <- vv(typhoon_loss_rate, var_CV, n_years)  # % of income lost
  
  # Post-typhoon coping mechanisms
  savings_available <- vv(savings_per_ha, var_CV, n_years)  # USD/ha available
  aid_received <- vv(aid_per_ha, var_CV, n_years) * typhoon_occurrence  # USD/ha/year
  
  # Baseline annual net income (USD/ha/year)
  baseline_net_annual <- baseline_annual_income * 
    (1 - typhoon_occurrence * typhoon_loss_percentage) + 
    pmin(aid_received, typhoon_occurrence * typhoon_loss_percentage * baseline_annual_income)
  
  # Premium Index Adjustment Calculations
  # Index ####
  # Insurance Type: Index-based insurance
  # Payout Trigger: Based on objective indices (weather data, satellite imagery, area yield)
  base_premium_index_adj <- vv(rep(base_premium_index/100, 
                                   n_years), var_CV, n_years)
  # Traditional ####
  # Insurance Type: Traditional indemnity insurance
  # Payout Trigger: Based on actual assessed losses on the farm
  base_premium_traditional_adj <- vv(rep(base_premium_traditional/100, 
                                         n_years), var_CV, n_years)
  # Hybrid ####
  # Insurance Type: Combination of index and traditional elements
  # Payout Trigger: Both index triggers and damage assessment
   base_premium_hybrid_adj <- vv(rep(base_premium_hybrid/100, 
                                    n_years), var_CV, n_years)
  
  # INSURANCE PREMIUMS (USD/ha/year)
premium_index <- vv(premium_index_per_ha, var_CV, n_years)  # USD/ha/year
premium_traditional <- vv(premium_traditional_per_ha, var_CV, n_years)
premium_hybrid <- vv(premium_hybrid_per_ha, var_CV, n_years)

# Subsidies
subsidy_available <- chance_event(subsidy_probability, value_if = 1, value_if_not = 0, n = n_years)
subsidy_rate <- vv(subsidy_level, var_CV, n_years)  # % of premium

farmer_pays_index <- premium_index * (1 - subsidy_rate * subsidy_available)
farmer_pays_traditional <- premium_traditional * (1 - subsidy_rate * subsidy_available)
farmer_pays_hybrid <- premium_hybrid * (1 - subsidy_rate * subsidy_available)

# INSURANCE PAYOUTS (USD/ha/year)
# Index insurance payout
index_payout <- ifelse(
  typhoon_occurrence & (typhoon_loss_percentage > index_trigger_threshold),
  typhoon_loss_percentage * baseline_annual_income * max_payout_index * (1 - basis_risk),
  0
)

# Traditional insurance payout  
traditional_payout <- ifelse(
  typhoon_occurrence,
  typhoon_loss_percentage * baseline_annual_income * max_payout_traditional * damage_assessment_accuracy,
  0
)

# Hybrid insurance payout
hybrid_payout <- (index_payout * hybrid_index_share) + (traditional_payout * (1 - hybrid_index_share))

# INSURED ANNUAL NET INCOME (USD/ha/year)
insured_net_annual_index <- baseline_annual_income - farmer_pays_index + index_payout
insured_net_annual_traditional <- baseline_annual_income - farmer_pays_traditional + traditional_payout
insured_net_annual_hybrid <- baseline_annual_income - farmer_pays_hybrid + hybrid_payout
  # Risk-adjusted premiums based on farm characteristics
  # Insurance companies use sophisticated risk-rating models
  # Base risk score (simplified GLM approach)
  # mimics how real agricultural insurers price policies while remaining implementable in your decision model.
  risk_score <- (vulnerability * 0.25 + 
                   exposure_risks * 0.35 + 
                   (1 - risk_mitigating_practices) * 0.20 +
                   spatial_geophysical * 0.15 +
                   log(af_system_size) * -0.05)
  
  # Experience rating component
  experience_modifier <- pmin(pmax(loss_ratio, 0.5), 2.0)  # Cap at 200%
  
  # Credibility-weighted final premium
  final_loss_ratio <- (credibility_factor * experience_modifier + 
                         (1 - credibility_factor) * pool_loss_ratio)
  
  # Add catastrophe load for extreme events
  catastrophe_loading <- catastrophe_load * likelihood_extreme_events
  
  
  # Subsidy calculations
  # Government subsidy scenarios
  subsidy_level <- chance_event(if_subsidy_available, value_if = 0.7, value_if_not = 0.3)
  
  # # Liquidity constraints 
  # liquidity_constraint <- ifelse(farmer_pays_final > liquidity_available, 0, 1)
  
  # Revised decision: Insurance vs. expected aid + self-insurance
 # net_benefit_insurance <- NPV_hybrid_farmer - expected_aid 
  
  farmer_pays_final_index <- premium_index * (1 - subsidy_level)
  farmer_pays_final_traditional <- premium_traditional * (1 - subsidy_level)
  farmer_pays_final_hybrid <- premium_hybrid * (1 - subsidy_level)
  
  # Insurance Uptake ####
  # Factors influencing uptake - using chance_event correctly
  uptake_drivers <- min(
    farmers_understand_insurance,
    affordability_premium,
    ease_getting_insurance,
    trust_insurers_farmers,
    cultural_acceptance
  )
  
  # Chance events for uptake 
  uptake_probability_index <- pmin(pmax(uptake_insurance * uptake_drivers / 25, 0), 1)
  uptake_probability_traditional <- pmin(pmax(uptake_insurance * uptake_drivers * 0.8 / 25, 0), 1)
  uptake_probability_hybrid <- pmin(pmax(uptake_insurance * uptake_drivers * 0.9 / 25, 0), 1)
  
  # Create time series of uptake events
  uptake_index <- chance_event(uptake_probability_index, value_if = 1, value_if_not = 0, n = n_years)
  uptake_traditional <- chance_event(uptake_probability_traditional, value_if = 1, value_if_not = 0, n = n_years)
  uptake_hybrid <- chance_event(uptake_probability_hybrid, value_if = 1, value_if_not = 0, n = n_years)
  
   # Risk Events and Payouts ####
  
  # Hazard occurrence 
  hazard_probability_adj <- pmin(pmax(hazard / 5, 0), 1)
  hazard_occurrence <- chance_event(hazard_probability_adj, value_if = 1, value_if_not = 0, n = n_years)
  
  # Damage calculation based on hazard and vulnerability
  total_insured_value <- (tree_value_per_ha + crop_value_per_ha) * af_system_size
  traditional_payout <- ifelse(
    typhoon_loss_rate > 0,
    typhoon_loss_rate * max_payout_traditional * total_insured_value * damage_assessment_accuracy,
    0
  )
  
  # Final premium calculation
  premium_index <- base_premium_index * risk_score * final_loss_ratio * 
    (1 + catastrophe_loading) * total_insured_value
  
  # Insurance payouts with trigger thresholds
  index_trigger_threshold <- 0.3
  max_payout_index <- 0.8
  max_payout_traditional <- 0.9
  damage_assessment_accuracy <- 0.8
  
  # Index-based insurance payout (based on triggers)
  # Use the agroforestry valuation from traditional insurance
  total_insured_value <- (tree_value_per_ha + crop_value_per_ha) * af_system_size
  
  index_payout <- ifelse(
    typhoon_loss_rate > index_trigger_threshold,
    (typhoon_loss_rate - index_trigger_threshold) * max_payout_index * total_insured_value * (1 - basis_risk),
    0
  )
  
  # Traditional insurance payout (based on damage assessment)
  traditional_payout <- ifelse(
    typhoon_loss_rate > 0,
    typhoon_loss_rate * max_payout_traditional * 10000 * damage_assessment_accuracy,
    0
  )
  
  # Hybrid insurance payout (combination)
  hybrid_payout <- (index_payout * 0.6) + (traditional_payout * 0.4)
  
  # Costs for Insurers ####
  admin_costs_index_adj <- vv(rep(admin_costs_index, n_years), var_CV, n_years)
  admin_costs_traditional_adj <- vv(rep(admin_costs_traditional, n_years), var_CV, n_years)
  admin_costs_hybrid_adj <- vv(rep(admin_costs_hybrid, n_years), var_CV, n_years)
  
  # Claims processing costs
  claims_costs_index <- index_payout * claims_processing_rate
  claims_costs_traditional <- traditional_payout * claims_processing_rate * 1.5
  claims_costs_hybrid <- hybrid_payout * claims_processing_rate * 1.2
  
  # Total costs for each scheme
  total_costs_index <- admin_costs_index_adj + claims_costs_index
  total_costs_traditional <- admin_costs_traditional_adj + claims_costs_traditional
  total_costs_hybrid <- admin_costs_hybrid_adj + claims_costs_hybrid
  
  # Revenues for Insurers ####
  premium_revenue_index <- premium_index * number_insured * 100 * uptake_index
  premium_revenue_traditional <- premium_traditional * number_insured * 100 * uptake_traditional
  premium_revenue_hybrid <- premium_hybrid * number_insured * 100 * uptake_hybrid
  
  # Net insurer results
  insurer_net_index <- premium_revenue_index - total_costs_index
  insurer_net_traditional <- premium_revenue_traditional - total_costs_traditional
  insurer_net_hybrid <- premium_revenue_hybrid - total_costs_hybrid
  
  # Farmer Benefits and Costs ####
  # Direct financial benefits
  farmer_benefit_index <- index_payout * uptake_index - farmer_pays_final_index * 100
  farmer_benefit_traditional <- traditional_payout * uptake_traditional - farmer_pays_final_traditional * 100
  farmer_benefit_hybrid <- hybrid_payout * uptake_hybrid - farmer_pays_final_hybrid * 100
  
  # Risk reduction benefits (enables investment)
  risk_reduction_value_adj <- vv(rep(risk_reduction_value, n_years), var_CV, n_years)
  risk_reduction_benefit <- risk_reduction_value_adj * (1 - typhoon_loss_rate) * uptake_insurance
  
  # Behavioral benefits (improved farm management)
  management_improvement_value_adj <- vv(rep(management_improvement_value, n_years), var_CV, n_years)
  management_improvement_benefit <- management_improvement_value_adj * risk_mitigating_practices * uptake_insurance
  
  # Social and Equity Benefits ####
  
  # Climate justice benefits
  climate_justice_benefit <- vv(climate_justice, var_CV, n_years) * 
    equity * fairness * uptake_insurance * 1000
  
  # Community cohesion benefits
  community_benefit <- vv(cohesiveness_community, var_CV, n_years) * 
    participatory * uptake_insurance * 500
  
  # Gender equity benefits
  gender_equity_benefit <- vv(gendered_decision_making, var_CV, n_years) * 
    equity * uptake_insurance * 300
  
  # Ecological Benefits ####
  
  # Agroforestry resilience benefits
  agroforestry_resilience_benefit <- vv(farm_resilience, var_CV, n_years) * 
    factors_resilience * af_profile * uptake_insurance * 800
  
  # Biodiversity benefits
  biodiversity_benefit <- vv(ecological_conditions, var_CV, n_years) * 
    diversity_of_group * uptake_insurance * 600
  
  # Risk Factors and Barriers ####
  
  # Implementation risks
  implementation_risk <- min(
    design_process,
    compliance_mechanism,
    data_availability,
    feasibility_insurance
  )
  
  # Sustainability risks
  sustainability_risk <- min(
    economically_sustainable,
    sustainability_insurance,
    availability_insurance,
    transferability_risk
  )
  
  # Cultural and perception risks
  cultural_risk <- min(
    risk_perception,
    cultural_importance,
    coverage_cultural_value,
    fairness
  )
  
  # Apply risk adjustments to benefits
  risk_adjusted_farmer_benefit_index <- farmer_benefit_index * implementation_risk
  risk_adjusted_farmer_benefit_traditional <- farmer_benefit_traditional * implementation_risk
  risk_adjusted_farmer_benefit_hybrid <- farmer_benefit_hybrid * implementation_risk
  
  # Total Benefit Calculations ####
  
  # Farmer total benefits (financial + risk reduction + behavioral)
  total_farmer_benefit_index <- risk_adjusted_farmer_benefit_index + 
    risk_reduction_benefit + management_improvement_benefit
  
  total_farmer_benefit_traditional <- risk_adjusted_farmer_benefit_traditional + 
    risk_reduction_benefit + management_improvement_benefit
  
  total_farmer_benefit_hybrid <- risk_adjusted_farmer_benefit_hybrid + 
    risk_reduction_benefit + management_improvement_benefit
  
  # Social and ecological benefits (apply sustainability risk)
  total_social_benefit <- (climate_justice_benefit + community_benefit + 
                             gender_equity_benefit) * sustainability_risk
  
  total_ecological_benefit <- (agroforestry_resilience_benefit + 
                                 biodiversity_benefit) * sustainability_risk
  
  # Alternative Scenario: No Insurance ####
  
  # Costs without insurance (self-insurance, savings, etc.)
  self_insurance_costs <- vv(rep(5000, n_years), var_CV, n_years)
  no_insurance_costs <- self_insurance_costs * vulnerability
  
  # Benefits without insurance (invested elsewhere)
  alternative_investment_returns <- vv(rep(3000, n_years), var_CV, n_years)
  no_insurance_benefits <- alternative_investment_returns
  
  no_insurance_net <- no_insurance_benefits - no_insurance_costs
  
# No Insurance ########
  # # Post-typhoon aid expectation (moral hazard)
  expected_aid <- vv(aid_expectation, var_CV, n_years) * hazard_occurrence
  
  
  # Final NPV Calculations ####
  # NPV for different perspectives
  
  # Farmer perspective (financial only)
  NPV_index_farmer <- discount(
    total_farmer_benefit_index - no_insurance_net,
    discount_rate = farmer_discount_rate/100,
    calculate_NPV = TRUE
  )
  
  NPV_traditional_farmer <- discount(
    total_farmer_benefit_traditional - no_insurance_net,
    discount_rate = farmer_discount_rate/100,
    calculate_NPV = TRUE
  )
  
  NPV_hybrid_farmer <- discount(
    total_farmer_benefit_hybrid - no_insurance_net,
    discount_rate = farmer_discount_rate/100,
    calculate_NPV = TRUE
  )
  
  # Social perspective (including social and ecological benefits)
  NPV_index_social <- discount(
    total_farmer_benefit_index + total_social_benefit + total_ecological_benefit - no_insurance_net,
    discount_rate = social_discount_rate/100,
    calculate_NPV = TRUE
  )
  
  NPV_traditional_social <- discount(
    total_farmer_benefit_traditional + total_social_benefit + total_ecological_benefit - no_insurance_net,
    discount_rate = social_discount_rate,
    calculate_NPV = TRUE
  )
  
  NPV_hybrid_social <- discount(
    total_farmer_benefit_hybrid + total_social_benefit + total_ecological_benefit - no_insurance_net,
    discount_rate = social_discount_rate,
    calculate_NPV = TRUE
  )
  
  # Insurer perspective
  NPV_index_insurer <- discount(
    insurer_net_index,
    discount_rate = farmer_discount_rate,
    calculate_NPV = TRUE
  )
  
  NPV_traditional_insurer <- discount(
    insurer_net_traditional,
    discount_rate = farmer_discount_rate,
    calculate_NPV = TRUE
  )
  
  NPV_hybrid_insurer <- discount(
    insurer_net_hybrid,
    discount_rate = farmer_discount_rate,
    calculate_NPV = TRUE
  )
  
  # In typhoon-prone SE Asia (Philippines, Vietnam, etc.), the real decision is:
  #   Option A: Pay insurance premiums and get protection
  # Option B: Self-insure (save money, diversify, rely on government aid)
  # Option C: Do nothing and absorb losses
# Philippines Reality Check:
#     Typical farmer: Marginal profits, liquidity constraints
#   Actual behavior: Most DON'T buy insurance unless heavily subsidized
# Reason: Premiums feel like "certain loss" vs. "probable loss" from typhoons
# Government role: Major subsidizer through PCIC (Philippine Crop Insurance Corp)

  # Return Results ####
  return(list(
    # Financial NPVs (farmer perspective)
    NPV_index_farmer = NPV_index_farmer,
    NPV_traditional_farmer = NPV_traditional_farmer,
    NPV_hybrid_farmer = NPV_hybrid_farmer,
    
    # Social NPVs (including externalities)
    NPV_index_social = NPV_index_social,
    NPV_traditional_social = NPV_traditional_social,
    NPV_hybrid_social = NPV_hybrid_social,
    
    # Insurer NPVs
    NPV_index_insurer = NPV_index_insurer,
    NPV_traditional_insurer = NPV_traditional_insurer,
    NPV_hybrid_insurer = NPV_hybrid_insurer,
    
    # Key performance indicators
    uptake_rate_index = mean(uptake_index),
    uptake_rate_traditional = mean(uptake_traditional),
    uptake_rate_hybrid = mean(uptake_hybrid),
    
    affordability_index = mean(affordability_premium),
    fairness_score = mean(fairness),
    sustainability_score = mean(sustainability_insurance),
    
    # Risk metrics
    implementation_risk_score = implementation_risk,
    cultural_risk_score = cultural_risk,
    
    # Benefit components
    total_financial_benefits = sum(total_farmer_benefit_hybrid),
    total_social_benefits = sum(total_social_benefit),
    total_ecological_benefits = sum(total_ecological_benefit)
  ))
}

