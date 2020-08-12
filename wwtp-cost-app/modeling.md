---
title: "Cost Modeling: Frequentist & Bayesian Approach"
author: "Grayson White, Jakob Oetinger, Amanda Reding"
output: html_document
---

```{r output = FALSE, warning = FALSE, message = FALSE}
# Load packages
library(tidyverse)
library(tidymodels)
library(patchwork)
```

```{r output = FALSE, warning = FALSE, message = FALSE}
# Load data
dat <- read_csv("dat.csv")
```

This document will preform both frequentist and bayesian methods, comparing the outcomes of the two. We will also use both standard functions such as `lm()` from the built-in `stats` package, and we will also perform our analyses with `tidymodels` packages.

# Frequentist, `stats` package
```{r include = FALSE}
# Model experimentation
m1 <- lm(log(`Total Cost`) ~ dryDesignFlowMGD + log(Population) + basic_treatment + Region + pop_density,
         data = dat)
summary(m1)

m2 <- lm(log(`Total Cost`) ~ log(Population) + Region + pop_density,
         data = dat)
summary(m2)

m2_inf <- lm(log(total_cost) ~ log(Population) + log(pop_density) + basic_treatment + has_pumps,
         data = dat)
summary(m2_inf)

m3 <- lm(log(`Total Cost`) ~ massLoadFlowMGD + Region + pop_density + coastal,
         data = dat)
summary(m3)

dat_filtered <- dat %>%
  filter(type1 %in% c("lagoons", "activated sludge"))

m4 <- lm(log(`Total Cost`) ~ log(Population) + Region + pop_density + type1,
         data = dat_filtered)
summary(m4)


m5 <- lm(log(total_cost) ~ log(Population) + Region + pop_density + basic_treatment + `Permit Type`,
         data = dat)
summary(m5)
```

```{r}
# Final model specification
final_model <- lm(log(total_cost) ~ log(Population) + log(pop_density) + basic_treatment + has_pumps,
         data = dat)
summary(final_model)
```


# Bayesian, `tidymodels` and `rstan` packages
```{r}
## Set seed for reproducibility :-)
set.seed(3737)

## Specify priors for each explanatory variable
prior_dist <- rstanarm::normal(location = c(0.5, -0.5, -0.5, 0, 1),
                               scale = c(0.25, 0.5, 0.5, 1, 0.5))

## Create model
bayes_mod <-
  linear_reg() %>% 
  set_engine("stan", # This is the Bayesian engine
             prior_intercept = rstanarm::normal(), 
             prior = prior_dist) %>%
  translate()

## Fit model to data
bayes_fit <-
  bayes_mod %>%
  fit(log(total_cost) ~ log(Population) + log(pop_density) + basic_treatment + has_pumps,
      data = dat)

bayes_fit
```

```{r echo = FALSE}
## Visualize dist for log(pop)
stan_fit <- bayes_fit$fit$stanfit

log_pop_dist_df <- data.frame(samples = c(stan_fit@sim$samples[[1]]$`beta[1]`,
                                          stan_fit@sim$samples[[2]]$`beta[1]`,
                                          stan_fit@sim$samples[[3]]$`beta[1]`,
                                          stan_fit@sim$samples[[4]]$`beta[1]`))
population <- ggplot(log_pop_dist_df,
       aes(samples)) +
  stat_function(fun = dnorm, args = list(mean = bayes_fit$fit$prior.info$prior$location[1],
                                         sd = bayes_fit$fit$prior.info$prior$scale[1])) +
  stat_function(fun = dnorm, args = list(mean = bayes_fit$fit$prior.info$prior$location[1],
                                         sd = bayes_fit$fit$prior.info$prior$scale[1]),
                geom = "area",
                aes(fill = "Prior"),
                alpha = 0.4) +
  scale_fill_manual(values = c("goldenrod", "steelblue")) +
  geom_density(aes(fill = "Posterior"), alpha = 0.4) +
  theme_bw() +
  xlim(-2,2) +
    theme(
    legend.position = "none"
  ) +
  labs(title = "Population")

log_pop_density_dist_df <- data.frame(samples = c(stan_fit@sim$samples[[1]]$`beta[2]`,
                                          stan_fit@sim$samples[[2]]$`beta[2]`,
                                          stan_fit@sim$samples[[3]]$`beta[2]`,
                                          stan_fit@sim$samples[[4]]$`beta[2]`))
population_density <- ggplot(log_pop_density_dist_df,
       aes(samples)) +
  stat_function(fun = dnorm, args = list(mean = bayes_fit$fit$prior.info$prior$location[2],
                                         sd = bayes_fit$fit$prior.info$prior$scale[2])) +
  stat_function(fun = dnorm, args = list(mean = bayes_fit$fit$prior.info$prior$location[2],
                                         sd = bayes_fit$fit$prior.info$prior$scale[2]),
                geom = "area",
                aes(fill = "Prior"),
                alpha = 0.4) +
  scale_fill_manual(values = c("goldenrod", "steelblue")) +
  geom_density(aes(fill = "Posterior"), alpha = 0.4) +
  theme_bw() +
  xlim(-2,2) +
    theme(
    legend.position = "none"
  ) +
  labs(title = "Population Density")

lagoons_dist_df <- data.frame(samples = c(stan_fit@sim$samples[[1]]$`beta[3]`,
                                          stan_fit@sim$samples[[2]]$`beta[3]`,
                                          stan_fit@sim$samples[[3]]$`beta[3]`,
                                          stan_fit@sim$samples[[4]]$`beta[3]`))
lagoons <- ggplot(lagoons_dist_df,
       aes(samples)) +
  stat_function(fun = dnorm, args = list(mean = bayes_fit$fit$prior.info$prior$location[3],
                                         sd = bayes_fit$fit$prior.info$prior$scale[3])) +
  stat_function(fun = dnorm, args = list(mean = bayes_fit$fit$prior.info$prior$location[3],
                                         sd = bayes_fit$fit$prior.info$prior$scale[3]),
                geom = "area",
                aes(fill = "Prior"),
                alpha = 0.4) +
  scale_fill_manual(values = c("goldenrod", "steelblue")) +
  geom_density(aes(fill = "Posterior"), alpha = 0.4) +
  theme_bw() +
  xlim(-2,2) +
    theme(
    legend.position = "none"
  ) +
  labs(title = "Treatment Type: Lagoons")

other_dist_df <- data.frame(samples = c(stan_fit@sim$samples[[1]]$`beta[4]`,
                                          stan_fit@sim$samples[[2]]$`beta[4]`,
                                          stan_fit@sim$samples[[3]]$`beta[4]`,
                                          stan_fit@sim$samples[[4]]$`beta[4]`))
other <- ggplot(other_dist_df,
       aes(samples)) +
  stat_function(fun = dnorm, args = list(mean = bayes_fit$fit$prior.info$prior$location[4],
                                         sd = bayes_fit$fit$prior.info$prior$scale[4])) +
  stat_function(fun = dnorm, args = list(mean = bayes_fit$fit$prior.info$prior$location[4],
                                         sd = bayes_fit$fit$prior.info$prior$scale[4]),
                geom = "area",
                aes(fill = "Prior"),
                alpha = 0.4) +
  scale_fill_manual(values = c("goldenrod", "steelblue")) +
  geom_density(aes(fill = "Posterior"), alpha = 0.4) +
  theme_bw() +
  xlim(-2,2) +
    theme(
    legend.position = "none"
  ) +
  labs(title = "Treatment Type: Other")

haspumps_df <- data.frame(samples = c(stan_fit@sim$samples[[1]]$`beta[5]`,
                                          stan_fit@sim$samples[[2]]$`beta[5]`,
                                          stan_fit@sim$samples[[3]]$`beta[5]`,
                                          stan_fit@sim$samples[[4]]$`beta[5]`))
haspumps <- ggplot(haspumps_df,
       aes(samples)) +
  stat_function(fun = dnorm, args = list(mean = bayes_fit$fit$prior.info$prior$location[5],
                                         sd = bayes_fit$fit$prior.info$prior$scale[5])) +
  stat_function(fun = dnorm, args = list(mean = bayes_fit$fit$prior.info$prior$location[5],
                                         sd = bayes_fit$fit$prior.info$prior$scale[5]),
                geom = "area",
                aes(fill = "Prior"),
                alpha = 0.4) +
  scale_fill_manual(values = c("goldenrod", "steelblue")) +
  geom_density(aes(fill = "Posterior"), alpha = 0.4) +
  theme_bw() +
  xlim(-2,2) +
    theme(
    legend.position = "bottom"
  ) +
  labs(title = "Has Pumps")

intercept_df <- data.frame(samples = c(stan_fit@sim$samples[[1]]$`alpha[1]`,
                                          stan_fit@sim$samples[[2]]$`alpha[1]`,
                                          stan_fit@sim$samples[[3]]$`alpha[1]`,
                                          stan_fit@sim$samples[[4]]$`alpha[1]`))
intercept <- ggplot(intercept_df,
       aes(samples)) +
  stat_function(fun = dnorm, args = list(mean = bayes_fit$fit$prior.info$prior_intercept$location,
                                         sd = bayes_fit$fit$prior.info$prior_intercept$scale)) +
  stat_function(fun = dnorm, args = list(mean = bayes_fit$fit$prior.info$prior_intercept$location,
                                         sd = bayes_fit$fit$prior.info$prior_intercept$scale),
                geom = "area",
                aes(fill = "Prior"),
                alpha = 0.4) +
  scale_fill_manual(values = c("goldenrod", "steelblue")) +
  geom_density(aes(fill = "Posterior"), alpha = 0.4) +
  theme_bw() +
  xlim(-6,18) +
  labs(title = "Intercept") +
  theme(
    legend.position = "none"
  )

(population + population_density + lagoons) / (other + haspumps + intercept)
```


```{r include = FALSE}
# class(stan_fit)
# stan_fit
# stan_fit@sim$samples[[4]]$`beta[4]`
# bayes_fit$fit
# bayes_fit$fit$coefficients
```




