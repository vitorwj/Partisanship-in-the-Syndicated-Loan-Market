library(devtools)
library(zaminfluence)
library(tidyverse)
library(gridExtra)
library(fastDummies)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df = readRDS('../data/final_dataset.RDS') %>%
  mutate(deal_amount = log(deal_amount)) %>%
  group_by(lender_parent_name) %>%
  mutate(se_group = cur_group_id() - 1) %>%
  mutate(interaction_kempf = if_else(kempf_dem == T, weighted_average_score, 0)) %>%
  mutate(interaction_dem55 = if_else(dem_55 == T, weighted_average_score, 0)) %>%
  mutate(interaction_dem60 = if_else(dem_60 == T, weighted_average_score, 0)) %>%
  mutate(interaction_dem55_imp = if_else(dem_55_imp == T, weighted_average_score, 0)) %>%
  mutate(interaction_dem60_imp = if_else(dem_60_imp == T, weighted_average_score, 0)) %>%
  dummy_cols(select_columns = c("major_industry_group", "cycle"), remove_first_dummy = T) %>%
  clean_names()

interactions = c('interaction_kempf',
                 'interaction_dem55',
                 'interaction_dem60',
                 'interaction_dem55_imp',
                 'interaction_dem60_imp')

summary_df_combined = data.frame()

for (i in 1:length(interactions)) {
  
  #############################
  # Fit a regression model.
  dummies = paste0(colnames(df %>% select(major_industry_group_aerospace_and_defense:major_industry_group_wholesale)), collapse = " + ")
  reg_form <- formula(paste0("deal_amount ~ weighted_average_score + ",
                             interactions[i],
                             " + realrate + fed_domestic_assets - 1 + cycle_2010 + cycle_2012 + cycle_2014 + cycle_2016 + cycle_2018 + cycle_2020 +",
                             dummies, collapse = ""))
  
  fit_object <- lm(data = df %>% filter(!is.na(weighted_average_score),
                                        !is.na(interactions[i]),
                                        !is.na(deal_amount),
                                        !is.na(fed_domestic_assets)), 
                   formula = reg_form,
                   x=TRUE, y=TRUE)
  summary(fit_object)
  #############################
  # Now it's zaminfluence time!
  
  # Get influence scores for the first two regressors.
  model_grads <-
    ComputeModelInfluence(fit_object) %>%
    AppendTargetRegressorInfluence(interactions[i])
  
  # Compute the changes needed to change sign, significance, and both
  signals <- GetInferenceSignals(model_grads)
  
  # Predict the changes, re-run the model at the left-out points, and
  # compare the two.
  
  preds <- PredictForSignals(signals, model_grads)
  reruns <- RerunForSignals(signals, model_grads)
  reruns_df <- GetSignalsAndRerunsDataframe(signals, reruns, model_grads)
  preds_df <- GetSignalsAndRerunsDataframe(signals, preds, model_grads)
  
  summary_df <-
    rbind(reruns_df %>% mutate(method="rerun"),
          preds_df %>% mutate(method="prediction")) %>%
    pivot_wider(-method, names_from=method, values_from=value) %>%
    filter(metric == 'param')
  
  if (i > 1) {
    summary_df_combined = rbind(summary_df_combined, summary_df)
  }
  else {
    summary_df_combined = summary_df
  }
}

library(stargazer)
stargazer(data.frame(summary_df_combined %>% select(!target_signal)
                     %>% select(!target_qoi) %>% select(!target_param_name)
                     %>% select(!metric)),
          summary=FALSE, rownames=FALSE
          )
ggplot(summary_df_combined) +
  geom_point(aes(x=prediction, y=rerun, color=param_name, shape=metric)) +
  geom_abline(aes(slope=1, intercept=0))

##############################################################
# Visualize which points are being dropped to change both
# sign and significance of the x1 regressor

signal <- signals[["x1"]][["both"]]
df$drop <- GetWeightVector(signal$apip$inds, nrow(df), bool=TRUE, invert=TRUE)
df$infl <- signal$qoi$infl
