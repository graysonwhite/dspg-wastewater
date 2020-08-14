pred_bayes <- data.frame(Population = 10000,
                         pop_density = 300,
                         basic_treatment = "Lagoons",
                         has_pumps = TRUE)

predict(bayes_fit, pred_bayes)
predict(bayes_fit, pred_bayes, type = "pred_int")


freq_mod <-
  linear_reg() %>% 
  set_engine("lm") %>%
  translate()

freq_fit <-
  freq_mod %>%
  fit(log(total_cost) ~ log(Population) + log(pop_density) + basic_treatment + has_pumps,
      data = dat)

predict(freq_fit, pred_bayes, type = "pred_int")

