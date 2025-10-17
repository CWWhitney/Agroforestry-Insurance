


# Agroforestry Insurance Decision Model Function
agroforestry_insurance_function <- function(x, varnames){
  
  # Insurance Scheme Selection
  # Three main insurance types: index-based, traditional, and hybrid
  
  # Premium Calculations
  base_premium_index <- vv(base_premium_index, var_CV, n_years)
  base_premium_traditional <- vv(base_premium_traditional, var_CV, n_years)
  base_premium_hybrid <- vv(base_premium_hybrid, var_CV, n_years)
  
  # Risk-adjusted premiums based on farm characteristics
  risk_adjustment_factor <- vulnerability * exposure_risks * 
    (1 - risk_mitigating_practices * 0.3) # Risk reduction from good practices
  
  # Final premium calculations
  premium_index <- base_premium_index * risk_adjustment_factor
  premium_traditional <- base_premium_traditional * risk_adjustment_factor
  premium_hybrid <- base_premium_hybrid * risk_adjustment_factor
  
  # Subsidy calculations
  subsidy_level <- vv(subsidized, var_CV, n_years)
  farmer_pays_final_index <- premium_index * (1 - subsidy_level)
  farmer_pays_final_traditional <- premium_traditional * (1 - subsidy_level)
  farmer_pays_final_hybrid <- premium_hybrid * (1 - subsidy_level)
  
  # Insurance Uptake ####
  # Factors influencing uptake
  uptake_drivers <- min(
    farmers_understand_insurance,
    affordability_premium,
    ease_getting_insurance,
    trust_insurers_farmers,
    cultural_acceptance
  )
  
  # Chance events for uptake
  uptake_index <- chance_event(uptake_insurance * uptake_drivers, 1, 0, n = n_years)
  uptake_traditional <- chance_event(uptake_insurance * uptake_drivers * 0.8, 1, 0, n = n_years)
  uptake_hybrid <- chance_event(uptake_insurance * uptake_drivers * 0.9, 1, 0, n = n_years)
  
  # Risk Events and Payouts ####
  
  # Hazard occurrence
  hazard_occurrence <- chance_event(hazard, 1, 0, n = n_years)
  
  # Damage calculation based on hazard and vulnerability
  damage_magnitude <- hazard_occurrence * vulnerability * exposure_risks
  
  # Index-based insurance payout (based on triggers)
  index_payout <- ifelse(
    damage_magnitude > index_trigger_threshold,
    damage_magnitude * max_payout_index,
    0
  )
  
  # Traditional insurance payout (based on damage assessment)
  traditional_payout <- ifelse(
    damage_magnitude > 0,
    damage_magnitude * max_payout_traditional * damage_assessment_accuracy,
    0
  )
  
  # Hybrid insurance payout (combination)
  hybrid_payout <- (index_payout * 0.6) + (traditional_payout * 0.4)
  
  # Costs for Insurers ####
  
  # Administrative costs
  admin_costs_index <- vv(admin_costs_index, var_CV, n_years)
  admin_costs_traditional <- vv(admin_costs_traditional, var_CV, n_years)
  admin_costs_hybrid <- vv(admin_costs_hybrid, var_CV, n_years)
  
  # Claims processing costs
  claims_costs_index <- index_payout * claims_processing_rate
  claims_costs_traditional <- traditional_payout * claims_processing_rate * 1.5 # Higher for traditional
  claims_costs_hybrid <- hybrid_payout * claims_processing_rate * 1.2
  
  # Total costs for each scheme
  total_costs_index <- admin_costs_index + claims_costs_index
  total_costs_traditional <- admin_costs_traditional + claims_costs_traditional
  total_costs_hybrid <- admin_costs_hybrid + claims_costs_hybrid
  
  # Revenues for Insurers ####
  premium_revenue_index <- premium_index * number_insured * uptake_index
  premium_revenue_traditional <- premium_traditional * number_insured * uptake_traditional
  premium_revenue_hybrid <- premium_hybrid * number_insured * uptake_hybrid
  
  # Net insurer results
  insurer_net_index <- premium_revenue_index - total_costs_index
  insurer_net_traditional <- premium_revenue_traditional - total_costs_traditional
  insurer_net_hybrid <- premium_revenue_hybrid - total_costs_hybrid
  
  # Farmer Benefits and Costs ####
  
  # Direct financial benefits
  farmer_benefit_index <- index_payout * uptake_index - farmer_pays_final_index
  farmer_benefit_traditional <- traditional_payout * uptake_traditional - farmer_pays_final_traditional
  farmer_benefit_hybrid <- hybrid_payout * uptake_hybrid - farmer_pays_final_hybrid
  
  # Risk reduction benefits (enables investment)
  risk_reduction_benefit <- vv(risk_reduction_value, var_CV, n_years) * 
    (1 - damage_magnitude) * uptake_insurance
  
  # Behavioral benefits (improved farm management)
  management_improvement_benefit <- vv(management_improvement_value, var_CV, n_years) * 
    risk_mitigating_practices * uptake_insurance
  
  # Social and Equity Benefits ####
  
  # Climate justice benefits
  climate_justice_benefit <- vv(climate_justice, var_CV, n_years) * 
    equity * fairness * uptake_insurance
  
  # Community cohesion benefits
  community_benefit <- vv(cohesiveness_community, var_CV, n_years) * 
    participatory * uptake_insurance
  
  # Gender equity benefits
  gender_equity_benefit <- vv(gendered_decision_making, var_CV, n_years) * 
    equity * uptake_insurance
  
  # Ecological Benefits ####
  
  # Agroforestry resilience benefits
  agroforestry_resilience_benefit <- vv(farm_resilience, var_CV, n_years) * 
    factors_resilience * af_profile * uptake_insurance
  
  # Biodiversity benefits
  biodiversity_benefit <- vv(ecological_conditions, var_CV, n_years) * 
    diversity_of_group * uptake_insurance
  
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
  no_insurance_costs <- vv(self_insurance_costs, var_CV, n_years) * vulnerability
  
  # Benefits without insurance (invested elsewhere)
  no_insurance_benefits <- vv(alternative_investment_returns, var_CV, n_years)
  
  no_insurance_net <- no_insurance_benefits - no_insurance_costs
  
  # Final NPV Calculations ####
  
  # Discount rates
  farmer_discount_rate <- vv(farmer_discount_rate, var_CV, n_years)
  social_discount_rate <- vv(social_discount_rate, var_CV, n_years)
  
  # NPV for different perspectives
  
  # Farmer perspective (financial only)
  NPV_index_farmer <- discount(
    total_farmer_benefit_index - no_insurance_net,
    discount_rate = farmer_discount_rate,
    calculate_NPV = TRUE
  )
  
  NPV_traditional_farmer <- discount(
    total_farmer_benefit_traditional - no_insurance_net,
    discount_rate = farmer_discount_rate,
    calculate_NPV = TRUE
  )
  
  NPV_hybrid_farmer <- discount(
    total_farmer_benefit_hybrid - no_insurance_net,
    discount_rate = farmer_discount_rate,
    calculate_NPV = TRUE
  )
  
  # Social perspective (including social and ecological benefits)
  NPV_index_social <- discount(
    total_farmer_benefit_index + total_social_benefit + total_ecological_benefit - no_insurance_net,
    discount_rate = social_discount_rate,
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