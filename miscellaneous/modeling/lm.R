library(tidyverse)
m1 <- lm(log(`Total Cost`) ~ dryDesignFlowMGD + log(Population) + basic_treatment + Region + pop_density_new, data = final_cost_small)
summary(m1)
plot(m1)

m2 <- lm(`Total Cost` ~ `Construction Cost`, data = final_cost_small)
summary(m2)$r.squared

m3 <- lm(log(`Total Cost`) ~ dryDesignFlowMGD + Region + pop_density_new, data = final_cost_small)
summary(m3)
