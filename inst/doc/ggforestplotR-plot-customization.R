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

## ----setup-data---------------------------------------------------------------
coefs <- data.frame(
  term = c("Age", "BMI", "Smoking", "Stage II", "Stage III"),
  estimate = c(0.12, -0.10, 0.18, 0.30, 0.46),
  conf.low = c(0.03, -0.18, 0.04, 0.10, 0.18),
  conf.high = c(0.21, -0.02, 0.32, 0.50, 0.74),
  sample_size = c(120, 115, 98, 87, 83),
  p_value = c(0.04, 0.15, 0.29, 0.001, 0.75),
  section = c("Clinical", "Clinical", "Clinical", "Tumor", "Tumor")
)

## ----grouping-right-----------------------------------------------------------
ggforestplot(
  coefs,
  grouping = "section",
  grouping_strip_position = "right",
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
  grouping = "section",
  grouping_strip_position = "right",
  n = "sample_size",
  p.value = "p_value",
  striped_rows = TRUE
) +
  add_forest_table(
    show_n = TRUE,
    show_p = TRUE,
    estimate_label = "Beta"
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
    show_n = TRUE,
    show_p = TRUE,
    estimate_label = "Beta",
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
    estimate_label = "Beta"
  ) 

## ----logistic-regression-data-------------------------------------------------
data(CO2)

l1 <- glm(Treatment ~ conc + uptake + Type, family = binomial(link = "logit"), 
    data = CO2)

## ----logistic-regression, warning=FALSE---------------------------------------

ggforestplot(l1, exponentiate = TRUE, striped_rows = T) +
  add_forest_table(position = "left", 
                   estimate_label = "OR", 
                   show_p = F)

## ----survival-analysis-data---------------------------------------------------
lung <- survival::lung

lung <- lung |>  
  dplyr::mutate(
    status = dplyr::recode(status, `1` = 0, `2` = 1)
  )

s1 <- survival::coxph(Surv(time, status) ~ sex + age + ph.karno + pat.karno, data = lung)

## ----survival-analysis-plot---------------------------------------------------
ggforestplot(s1, exponentiate = T, striped_rows = T) +
  add_forest_table(estimate_label = "HR")

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
  grouping = "section",
  striped_rows = TRUE,
  dodge_width = 0.5,
  grouping_strip_position = "right"
) +
  theme(legend.position = "bottom")

