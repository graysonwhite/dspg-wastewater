# the script goes from total cost small to dat

dat <- read_csv("../final_cost_small.csv")

dat <- dat %>%
  mutate(
    has_pumps = case_when(
      Collection %in% c("Conventional Gravity, Effluent Pumps",
                        "Effluent Pumps", "Effluent Pumps, Conventional Gravity",
                        "Conventional Gravity, Effluent Pumps, Small Diameter Gravity") ~ TRUE,
      Collection %in% c("Conventional Gravity", "-") ~ FALSE
    )
  ) %>%
  select(-X1, -X1_1) %>%
  mutate(
    pop_density = Population / sq_mi
  )

dat <- dat %>%
  mutate(year_begin = as.numeric(substr(Year, 0, 4)))

dat <- dat %>%
  mutate(year_inflated_2019 = adjust_for_inflation(`Total Cost`, year_begin, "US", to_date = 2019)) 

dat$Region[8] <- "ER"
dat$Region[24] <- "WR"
dat$Region[9] <- "WR"

dat$pop_density[9] <- 1451.9
dat$pop_density[20] <- 885.0
dat$pop_density[24] <- 924.11
dat$Population[24] <- 750
dat$pop_density[28] <- 890.5

dat <- dat %>%
  mutate(
    total_cost = year_inflated_2019
  )