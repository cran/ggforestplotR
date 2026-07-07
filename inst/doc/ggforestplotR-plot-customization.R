## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8.5,
  fig.height = 5.5
)

## ----setup--------------------------------------------------------------------
library(ggforestplotR)
library(ggplot2)

## ----facet-right--------------------------------------------------------------
coefs <- data.frame(
  term = c("Age", "BMI", "Smoking", "Stage II", "Stage III"),
  estimate = c(0.12, -0.10, 0.18, 0.30, 0.46),
  conf.low = c(0.03, -0.18, 0.04, 0.10, 0.18),
  conf.high = c(0.21, 0.02, 0.32, 0.50, 0.74),
  sample_size = c(120, 115, 98, 87, 83),
  p_value = c(0.04, 0.15, 0.29, 0.001, 0.075),
  section = c("Clinical", "Clinical", "Clinical", "Tumor", "Tumor")
)


ggforestplot(
  coefs,
  facet = "section",
  facet_strip_position = "right",
  striped_rows = TRUE
)

## ----separators---------------------------------------------------------------
block_coefs <- data.frame(
  term = c("race_black", "race_white", "race_other", "age", "bmi"),
  label = c("Black", "White", "Other", "Age", "BMI"),
  estimate = c(0.24, 0.08, -0.04, 0.12, -0.09),
  conf.low = c(0.10, -0.04, -0.18, 0.03, -0.17),
  conf.high = c(0.38, 0.20, 0.10, 0.21, -0.01),
  variable_block = c("Race", "Race", "Race", "Age", "BMI")
)

ggforestplot(
  block_coefs,
  label = "label",
  separate_groups = "variable_block",
  separate_lines = TRUE,
  striped_rows = TRUE
) +
  scale_y_discrete(limits = rev(c("BMI", "Age", "Race: White", 
                                  "Race: Black", "Race: Other")))

## ----left-side-table----------------------------------------------------------
ggforestplot(
  coefs,
  facet = "section",
  facet_strip_position = "right",
  n = "sample_size",
  p.value = "p_value",
  striped_rows = TRUE,
  term_labels = c("Smoking" = "Smoking status")
) +
  add_forest_table(
    columns = c("term", "sample_size", "estimate", "p_value"),
    column_labels = c("term" = "Variable", "sample_size" = "N",
                      "estimate" = "Beta (95% CI)", "p_value" = "P-value")
  )

## -----------------------------------------------------------------------------
ggforestplot(
  coefs,
  n = "sample_size",
  p.value = "p_value",
  striped_rows = TRUE
) +
  add_forest_table(
    position = "left",
    grid_lines = T,
    grid_line_linetype = 2,
    grid_line_colour = "red"
  )

## ----split-table--------------------------------------------------------------
ggforestplot(
  coefs,
  n = "sample_size",
  p.value = "p_value",
  striped_rows = TRUE
) +
  scale_x_continuous(limits = c(-.8,.8)) +
  add_split_table(
    left_columns = c("term","n"),
    right_columns = c("estimate","p"),
    column_labels = c("estimate" = "Beta [95% CI]"),
    estimate_fmt = "{estimate} [{conf.low}, {conf.high}]",
    estimate_digits = 2,
    interval_digits = 3
  ) 

## ----logistic-regression-data-------------------------------------------------
data(CO2)

l1 <- glm(Treatment ~ conc + uptake + Type, family = binomial(link = "logit"), 
    data = CO2)

## ----logistic-regression, warning=FALSE---------------------------------------

ggforestplot(l1, exponentiate = TRUE, striped_rows = T, term_labels = c("TypeMississippi" = "Mississippi")) +
  add_forest_table(position = "left", 
                   columns = c("term", "estimate"))

## ----survival-analysis-data---------------------------------------------------
lung <- survival::lung

lung <- lung |>  
  dplyr::mutate(
    status = dplyr::recode(status, `1` = 0, `2` = 1)
  )

s1 <- survival::coxph(Surv(time, status) ~ sex + age + ph.karno + pat.karno, data = lung)

## ----survival-analysis-plot---------------------------------------------------
ggforestplot(s1, exponentiate = T, striped_rows = T) +
  add_forest_table()

## ----comparison---------------------------------------------------------------
comparison_coefs <- data.frame(
  term = rep(c("Age", "BMI", "Smoking", "Stage II", "Stage III"), 2),
  estimate = c(0.12, -0.10, 0.18, 0.30, 0.46, 0.08, -0.05, 0.24, 0.40, 0.58),
  conf.low = c(0.03, -0.18, 0.04, 0.10, 0.18, 0.00, -0.13, 0.10, 0.20, 0.30),
  conf.high = c(0.21, -0.02, 0.32, 0.50, 0.74, 0.16, 0.03, 0.38, 0.60, 0.86),
  model = rep(c("Model A", "Model B"), each = 5),
  section = rep(c("Clinical", "Clinical", "Clinical", "Tumor", "Tumor"), 2)
)

ggforestplot(
  comparison_coefs,
  group = "model",
  facet = "section",
  striped_rows = TRUE,
  dodge_width = 0.5,
  facet_strip_position = "right"
) +
  theme(legend.position = "top") +
  scale_color_manual(values = c("#1F968BFF", "#453781FF")) +
  add_forest_table()

## ----bind-models, fig.width=8, fig.height=4.5, fig.dpi=300--------------------
fit1 <- lm(mpg ~ cyl, data = mtcars)
fit2 <- lm(mpg ~ cyl + disp, data = mtcars)
fit3 <- lm(mpg ~ cyl + disp + wt, data = mtcars)

bound_models <- bind_forest_models(list(fit1,fit2,fit3), 
                                   model_labels = c("Unadjusted", "Adjusted", "Fully Adjusted"))

ggforestplot(bound_models, striped_rows = T, p.value = "p.value") +
  scale_x_continuous(limits = c(-6,1)) +
  theme(legend.position = "top") +
  scale_color_manual(values = c("#1F968BFF", "#453781FF", "#FDE725FF")) +
  add_forest_table(columns = c("term", "estimate", "p.value"),
                   p_digits = 4)

