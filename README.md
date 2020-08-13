# Data Science for the Public Good

## Rural Wastewater Facility Cost Modeling & Planning Tools

### Overview
This repository includes data, scripts, and analysis done for a Data Science for the Public Good project at Oregon State University in summer 2020. This project is currently being worked on by Jakob Oetinger, Amanda Reding, and Grayson White, all under the supervision of Dr. Christine Kelly. 

The goal of this project is to provide rural communities with insights to the cost of implimenting a centralized wastewater facility in their area. To achieve that, we will create a  model that will predict the cost required to impliment such a system. This model will be focused on predicting the cost of small (<1 MGD) wastewater facilities.

### `shiny` Web Application
Our web-based application includes visualization and tools for understanding, planning, and cost modeling small, centralized, rural Oregon wastewater treatment facilities, and information on decentralized treatment. Intended audiences of this application include the interested public, sewer boards, councils, planners, and researchers. We have characterized *small scale* wastewater treatment plants, meaning that we only consider treatment plants with a dry design capacity of less than or equal to 1 MGD. The `shiny` web application can be found [here](https://graysonwhite.shinyapps.io/oregon-wwtps/).
![](https://github.com/graysonwhite/dspg-wastewater/blob/master/shiny.png)

### Results & Analysis 
We first characterized wastewater treatment plants in Oregon, and we have found some interesting results about their structure, location, and cost. Many of these results can be found on the [`shiny` web application](https://graysonwhite.shinyapps.io/oregon-wwtps/).

Below, we have a plot of 114 small (<1 MGD) wastewater treatment plants in Oregon:
![](https://github.com/graysonwhite/dspg-wastewater/blob/master/data%20visualizations/png-visualizations/geq.png)

We also performed both frequentist and Bayesian statistical analysis to predict the cost of a new wastewater treatment plant in Oregon. With only 34 observations, we primarily used the Bayesian methods to draw inferences from our regressions. We used Bayesian methods because we were (1) able to specify informative priors due to literature on cost modeling for wastewater treatment plants overseas, and (2) able to make informative inferences without a large sample size. Below, we have the code used for our Bayesian modeling and our prior and posterior distributions plotted for each explanatory variable.

```{r}
# Load packages
library(tidyverse)
library(tidymodels)
library(patchwork)

# Set seed for reproducibility
set.seed(3737)

# Specify priors for each explanatory variable
prior_dist <- rstanarm::normal(location = c(0.5, -0.5, -0.5, 0, 1),
                               scale = c(0.25, 0.5, 0.5, 1, 0.5))

# Create model
bayes_mod <-
  linear_reg() %>% 
  set_engine("stan", # This is the Bayesian engine
             prior_intercept = rstanarm::normal(), 
             prior = prior_dist) %>%
  translate()

# Fit model to data
bayes_fit <-
  bayes_mod %>%
  fit(log(total_cost) ~ log(Population) + log(pop_density) + basic_treatment + has_pumps,
      data = dat)
```

![](https://github.com/graysonwhite/dspg-wastewater/blob/master/modeling/test.png)

### SAGE Publication
Currently, we are in the process of writing up our results to publish to SAGE Methodspace. 


