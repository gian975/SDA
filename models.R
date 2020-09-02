model_not_linear_sqrt <- (co2_emission ~ euro_standard + transmission_type + sqrt(engine_capacity) +
                            fuel_type + combined_metric  + noise_level)

model_not_linear_log <- (co2_emission ~ euro_standard + transmission_type + log(engine_capacity) +
                           fuel_type + combined_metric  + noise_level)

model_not_linear_poly_2 <-(co2_emission ~ euro_standard + transmission_type + poly(engine_capacity, 2) +
                             fuel_type + combined_metric)

model_not_linear_poly_3 <-(co2_emission ~ euro_standard + transmission_type + poly(engine_capacity, 3) +
                             fuel_type + combined_metric)

model_not_linear_poly_4 <-(co2_emission ~ euro_standard + transmission_type + poly(engine_capacity, 4) +
                             fuel_type + combined_metric)

model_reduced_collinearity_CM <-(co2_emission ~ euro_standard + transmission_type +
                                   fuel_type + combined_metric  + noise_level)