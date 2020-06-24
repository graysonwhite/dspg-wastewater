library(tidyverse)
library(readxl)

survey_2017 <- read_excel("data/raw/Water_Rates_Survey_2017.xlsx", 
                          sheet = "CLEANED")
colnames(survey_2017) <- survey_2017[1,]
survey_2017 <- survey_2017[-1,]

survey_2017_select <- survey_2017[, c(1, 36, 39, 42, 45, 48, 84,97, 157:199)]

colnames(survey_2017_select) <- c("City", "percent_rate_rev_to_debt", "percent_rate_rev_to_debt_NA", "assit_management_sys",
                                  "last_year_rate_study", "last_year_methodolgy_updates", "charge_for_ww_services",
                                  "5000_gal_bill", "provide_ww_service", "service_pop_residents_inside",
                                  "service_pop_residents_outside", "service_pop_peakseason_inside", "service_pop_peakseason_inside",
                                  "num_connections_res_inside", "num_connections_res_outside", "num_connections_comm_inside",
                                  "num_connections_comm_outside", "num_connections_other_inside", "num_connections_other_outside",
                                  "annual_vol_res_customer", "total_sewer_line_mi", "total_pumps_liftstations", "total_treatment_plants",
                                  "percent_combined_sewer", "treatment_level_primary", "treatment_level_secondary", "treatment_level_tertiary",
                                  "treatment_level_nitrogen_removal", "treatment_level_phosphorous_removal", "treatment_level_other",
                                  "treatment_level_other_text", "stream_water_TMDL", "explain_TMDL", "year_of_construction", "last_major_renovation",
                                  "capacity_dry_weather", "capacity_peak_wet_weather", "total_ww_treated_2016", "peak_wet_weather_flow_2016",
                                  "peak_dry_weather_flow_2016", "perc_capacity_operating_at", "year_at_max_capacity", "year_exceed_design_capacity",
                                  "indust_pretreatment", "reclaimed_water_public_private", "perc_reclaimed_is_reused_applied",
                                  "where_reuse", "biosolids_to_public_private", "perc_biosolids_applied", "where_biosolids",
                                  "additional_comments")

survey_2017_select$`5000_gal_bill` <- parse_number(survey_2017_select$`5000_gal_bill`)
survey_2017_select$total_sewer_line_mi <- parse_number(survey_2017_select$total_sewer_line_mi)
survey_2017_select$year_of_construction <- parse_number(survey_2017_select$year_of_construction)
survey_2017_select$last_major_renovation <- parse_number(survey_2017_select$last_major_renovation)

# overwrites data file with file called `survey_2017` in your global environment
# saveRDS(survey_2017, "data/cleaned-and-or-rds/survey_2017.rds")


survey_2019 <- read_excel("data/raw/Water Rates Survey 2019_November 19, 2019_08.19.xlsx", 
                          sheet = "Cleaned")

survey_2019_select <- survey_2019[, c(1:4, 27,71, 137:162)] %>%
  rename(rev_debt_percent = "What percentage of rate revenue is obligated to debt services for the following systems? - Rate Revenue - Wastewater - %",
         "5000_gal_bill" = "For wastewater services, if you were to bill a residential customer for 5,000 gallons (6.684 CCFs) with a 3/4'' meter size, what dollar amount would you bill them, including the base rate?",
         annual_usage_per_customer = "What is the annual average wastewater base (volume) for a residential customer (x1000 gal. or 1.337 CCFs)?",
         total_sewer_lines_mi = "Please provide the following facility, lines, and treatment information: - Total miles of sewer lines (all sizes), not including service laterals",
         total_pumps_lift_stations = "Please provide the following facility, lines, and treatment information: - Total number of pumps and lift stations in your city",
         total_number_treatment_plants = "Please provide the following facility, lines, and treatment information: - Total number of treatment plants",
         combined_sewer_percentage = "Please provide the following facility, lines, and treatment information: - What percent of city wastewater lines also serve stormwater (i.e. combined sewer)?",
         level_of_treatment = "What level of wastewater treatment is provided to city wastewater (Check all that apply)? - Selected Choice",
         level_of_treatment_other = "What level of wastewater treatment is provided to city wastewater (Check all that apply)? - Other (Please Specify) - Text",
         year_of_contruction = "Please provide the following system age and capacity information: - Year of original plant construction completion",
         year_of_renovation = "Please provide the following system age and capacity information: - Year of last major plant update",
         design_capacity_dry_weather = "Please provide the following system age and capacity information: - What is the design capacity of your treatment plant(s) in dry weather (MGD)?",
         design_capacity_peak_wet_weather = "Please provide the following system age and capacity information: - What is the design capacity of your treatment plant(s) in peak wet weather (MGD)?",
         total_ww_treated = "Please provide the following system age and capacity information: - What is the total amount of wastewater treated in 2018 (MG)?",
         peak_wet_weather_flow = "Please provide the following system age and capacity information: - What was the peak wet weather flow in 2018 (MGD)?",
         peak_dry_weather_flow = "Please provide the following system age and capacity information: - What was the peak dry weather flow in 2018 (MGD)?",
         operating_capacity = "At what percent (%) capacity is the entire wastewater system operating?",
         year_at_max_capacity = "In what year will the wastewater system be at maximum capacity?",
         year_exceed_max_capacity = "In what year will your daily production exceed design capacity?...153",
         pre_treatment = "Does your city administer an industrial wastewater pre-treatment program?",
         reclaimed_water_to_public_private = "Does your city apply or provide reclaimed water to public/private property?",
         percentage_reclaimed_is_reused_applied = "What percentage (%) of total reclaimed water is reused/applied?",
         where_is_reuse = "Where does this reuse and application occur (i.e. city park, private golf course, industrial cooling tower, etc.)?",
         biosolids_to_public_private = "Does your city apply biosolids to public/ private property?",
         where_is_biosolids = "Where does this biosolid application occur (i.e. city park, private golf course, etc.)?",
         landfill_biosolids = "Does your city landfill biosolids?",
         percentage_biosolids_landfill = "What percentage (%) of biosolids are landfilled?",
         additional_comments = "Do you have any additional comments on wastewater services?")

survey_2019_select$`5000_gal_bill` <- parse_number(survey_2019_select$`5000_gal_bill`)
survey_2019_select$total_sewer_lines_mi <- parse_number(survey_2019_select$total_sewer_lines_mi)
survey_2019_select$year_of_contruction <- parse_number(survey_2019_select$year_of_contruction)
survey_2019_select$year_of_renovation <- parse_number(survey_2019_select$year_of_renovation)


# overwrites data file with file called `survey_2019` in your global environment
# saveRDS(survey_2019, "data/cleaned-and-or-rds/survey_2019.rds")
