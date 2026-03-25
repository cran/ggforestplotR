## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 5
)

## ----setup--------------------------------------------------------------------
library(ggforestplotR)
library(ggplot2)

## ----basic-plot---------------------------------------------------------------
basic_coefs <- data.frame(
  term = c("Age", "BMI", "Treatment"),
  estimate = c(0.10, -0.08, 0.34),
  conf.low = c(0.02, -0.16, 0.12),
  conf.high = c(0.18, 0.00, 0.56)
)

ggforestplot(basic_coefs)

## ----grouped-striped----------------------------------------------------------
sectioned_coefs <- data.frame(
  term = c("Age", "BMI", "Smoking", "Stage II", "Stage III", "Nodes"),
  estimate = c(0.10, -0.08, 0.20, 0.34, 0.52, 0.28),
  conf.low = c(0.02, -0.16, 0.05, 0.12, 0.20, 0.06),
  conf.high = c(0.18, 0.00, 0.35, 0.56, 0.84, 0.50),
  section = c("Clinical", "Clinical", "Clinical", "Tumor", "Tumor", "Tumor")
)

ggforestplot(
  sectioned_coefs,
  grouping = "section",
  striped_rows = TRUE,
  stripe_fill = "grey94"
)

## ----side-table---------------------------------------------------------------
tabled_coefs <- data.frame(
  term = c("Age", "BMI", "Smoking", "Stage II", "Stage III"),
  estimate = c(0.12, -0.10, 0.18, 0.30, 0.46),
  conf.low = c(0.03, -0.18, 0.04, 0.10, 0.18),
  conf.high = c(0.21, -0.02, 0.32, 0.50, 0.74),
  sample_size = c(120, 115, 98, 87, 83)
)

ggforestplot(tabled_coefs, n = "sample_size", striped_rows = TRUE) +
  add_forest_table(
    position = "left",
    show_n = TRUE,
    estimate_label = "Beta"
  )

## ----split-table--------------------------------------------------------------
ggforestplot(tabled_coefs, n = "sample_size", striped_rows = T) +
  add_split_table(
    left_columns = c("term", "n"),
    right_columns = c("estimate")
  )

## ----model-plot---------------------------------------------------------------
fit <- lm(mpg ~ wt + hp + qsec, data = mtcars)

ggforestplot(fit, sort_terms = "descending")

