source("R/filter_functions.R")
source("R/plot_functions.R")

cov_data <- read.csv("data.csv")

# --- plot incidence correlation matrix ---
incidences_df <- get_incidence_per_district(cov_data, 7)
incidence_correlation_pairs <- get_correlation_for_incidence_pairs(incidences_df)

plot_incidence_correlations_matrix(incidence_correlation_pairs,
                                   districts = c("SK Bochum", "SK Dortmund", "LK Esslingen"))

# --- plot incidence correlation barchart ---
incidences_df <- get_incidence_per_district(cov_data, 7)
incidence_correlation_pairs <- get_correlation_for_incidence_pairs(incidences_df)

plot_incidence_correlations_barchart(incidence_correlation_pairs)
