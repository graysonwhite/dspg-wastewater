---
title: "Cost Modeling for WWTPs in Oregon"
output: html_document
---








```r
# Make the model
## Set seed
set.seed(3737)

## Specify priors
prior_dist <- rstanarm::student_t(df = 1, location = 0, scale = 2)

## Split train/test
wwtp_split <- usda_cost %>%
  initial_split()

wwtp_train <- training(wwtp_split)
wwtp_test <- testing(wwtp_split)

## Create model
bayes_mod <-
  linear_reg() %>% 
  set_engine("stan", 
             prior_intercept = prior_dist, 
             prior = prior_dist) %>%
  translate()

## Train
bayes_fit <-
  bayes_mod %>%
  fit(log(`Total Cost`) ~ log(Population) + basic_treatment + has_pumps,
      data = wwtp_train)

# bayes_fit$fit
# bayes_fit$fit$coefficients
stan_fit <- bayes_fit$fit$stanfit
# class(stan_fit)
# stan_fit
# stan_fit@sim$samples[[4]]$`beta[4]`

bayes_fit
```

```
## parsnip model object
## 
## Fit time:  838ms 
## stan_glm
##  family:       gaussian [identity]
##  formula:      log(`Total Cost`) ~ log(Population) + basic_treatment + has_pumps
##  observations: 25
##  predictors:   5
## ------
##                        Median MAD_SD
## (Intercept)            12.3    1.0  
## log(Population)         0.4    0.2  
## basic_treatmentLagoons -0.6    0.2  
## basic_treatmentOther   -1.3    0.6  
## has_pumpsTRUE           0.4    0.3  
## 
## Auxiliary parameter(s):
##       Median MAD_SD
## sigma 0.6    0.1   
## 
## ------
## * For help interpreting the printed output see ?print.stanreg
## * For info on the priors used see ?prior_summary.stanreg
```

```r
## Visualize dist for log(pop)
log_pop_dist_df <- data.frame(samples = c(stan_fit@sim$samples[[1]]$`beta[1]`,
                                          stan_fit@sim$samples[[2]]$`beta[1]`,
                                          stan_fit@sim$samples[[3]]$`beta[1]`,
                                          stan_fit@sim$samples[[4]]$`beta[1]`))
ggplot(log_pop_dist_df,
       aes(samples)) +
  stat_function(fun = dt, args = list(df = 1)) +
  stat_function(fun = dt, args = list(df = 1), geom = "area", aes(fill = "Prior"), alpha = 0.4) +
  scale_fill_manual(values = c("goldenrod", "steelblue")) +
  geom_density(aes(fill = "Posterior"), alpha = 0.4) +
  theme_bw() +
  xlim(-2,2)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)


```r
# Frequentist
## create model
freq_mod <- linear_reg() %>%
  set_engine("lm")

## train
freq_fit <- 
  freq_mod %>%
  fit(log(`Total Cost`) ~ log(Population) + basic_treatment + has_pumps,
      data = wwtp_train)
freq_fit
```

```
## parsnip model object
## 
## Fit time:  23ms 
## 
## Call:
## stats::lm(formula = log(`Total Cost`) ~ log(Population) + basic_treatment + 
##     has_pumps, data = data)
## 
## Coefficients:
##            (Intercept)         log(Population)  basic_treatmentLagoons    basic_treatmentOther           has_pumpsTRUE  
##                12.4908                  0.4256                 -0.6080                 -1.4167                  0.3985
```

```r
freq_fit$fit$coefficients
```

```
##            (Intercept)        log(Population) basic_treatmentLagoons   basic_treatmentOther          has_pumpsTRUE 
##             12.4908256              0.4256314             -0.6080177             -1.4166924              0.3984637
```

```r
tidy(freq_fit)
```

```
## # A tibble: 5 x 5
##   term                   estimate std.error statistic  p.value
##   <chr>                     <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)              12.5       1.03      12.2  1.08e-10
## 2 log(Population)           0.426     0.145      2.94 8.11e- 3
## 3 basic_treatmentLagoons   -0.608     0.231     -2.64 1.58e- 2
## 4 basic_treatmentOther     -1.42      0.592     -2.39 2.66e- 2
## 5 has_pumpsTRUE             0.398     0.267      1.49 1.52e- 1
```







