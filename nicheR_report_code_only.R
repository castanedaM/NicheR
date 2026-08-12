knitr::purl("C:/Users/mcguz/Downloads/nicheR_report_20260812_171601/nicheR_report.Rmd", output = "nicheR_report_code_only.R", documentation = 1)

## ----setup, include = FALSE--------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(nicheR)
library(terra)


## ----build-data, eval = TRUE-------------------------------
# Bundled example data
bios <- terra::rast(
  system.file("extdata/ma_bios.tif", package = "nicheR"))
bios <- terra::subset(bios, c("bio_1", "bio_5", "bio_6"))


## ----build-ranges, eval = TRUE-----------------------------
range_1 <- data.frame(
  bio_1 = c(22.85, 26.48),
  bio_5 = c(30.90, 34.17),
  bio_6 = c(13.73, 20.73)
)

range_2 <- ranges_from_stats(
  mean = c(bio_1 = 24.5, bio_5 = 31.0, bio_6 = 20.0),
  sd = c(bio_1 = 1.5, bio_5 = 1.5, bio_6 = 0.5),
  cl = 0.9,
  expand_min = c(bio_1 = 10, bio_5 = 0, bio_6 = 0),
  expand_max = c(bio_1 = 0, bio_5 = 25, bio_6 = 0)
)



## ----build-1, eval = TRUE----------------------------------
# base_1, built from ranges
base_1 <- build_ellipsoid(
  range = range_1,
  cl = 0.95
)


## ----build-2, eval = TRUE----------------------------------
# ellipsoid_2, copied from base_1
ellipsoid_2 <- build_ellipsoid(
  range = range_1,
  cl = 0.95
)

ellipsoid_2 <- update_ellipsoid_covariance(
  object = ellipsoid_2,
  covariance = c(`bio_1-bio_5` = 0.0654, `bio_1-bio_6` = -0.2962, `bio_5-bio_6` = 0.1305)
)

ellipsoid_2 <- update_ellipsoid_centroid(
  ell = ellipsoid_2,
  new_centroid = c(bio_1 = 24.66, bio_5 = 32.53, bio_6 = 11.92)
)


## ----build-3, eval = TRUE----------------------------------
# base2, built from ranges
base2 <- build_ellipsoid(
  range = range_2,
  cl = 0.9
)


## ----build-4, eval = TRUE----------------------------------
# ellipsoid_5, copied from base2
ellipsoid_5 <- build_ellipsoid(
  range = range_2,
  cl = 0.9
)

ellipsoid_5 <- update_ellipsoid_covariance(
  object = ellipsoid_5,
  covariance = c(`bio_1-bio_5` = 0.4040, `bio_1-bio_6` = -0.0460, `bio_5-bio_6` = 0.1148)
)

ellipsoid_5 <- update_ellipsoid_centroid(
  ell = ellipsoid_5,
  new_centroid = c(bio_1 = 24.25, bio_5 = 31.62, bio_6 = 17.05)
)


## ----predict-1, eval = TRUE--------------------------------
predictions_1_ellipsoids <- list(base_1 = base_1)

predictions_1 <- lapply(predictions_1_ellipsoids, function(e){
  predict(e,
          newdata = bios,
          include_mahalanobis = FALSE,
          include_suitability = TRUE,
          mahalanobis_truncated = FALSE,
          suitability_truncated = TRUE,
          adjust_truncation_level = 0.95,
          verbose = FALSE)
})


## ----predict-2, eval = TRUE--------------------------------
predictions_2_ellipsoids <- list(ellipsoid_2 = ellipsoid_2)

predictions_2 <- lapply(predictions_2_ellipsoids, function(e){
  predict(e,
          newdata = bios,
          include_mahalanobis = TRUE,
          include_suitability = TRUE,
          mahalanobis_truncated = TRUE,
          suitability_truncated = TRUE,
          adjust_truncation_level = 0.8,
          verbose = FALSE)
})


## ----predict-3, eval = TRUE--------------------------------
predictions_3_ellipsoids <- list(base2 = base2, ellipsoid_5 = ellipsoid_5)

predictions_3 <- lapply(predictions_3_ellipsoids, function(e){
  predict(e,
          newdata = bios,
          include_mahalanobis = TRUE,
          include_suitability = TRUE,
          mahalanobis_truncated = TRUE,
          suitability_truncated = TRUE,
          adjust_truncation_level = 0.95,
          verbose = FALSE)
})


## ----session-info, eval = TRUE-----------------------------
sessionInfo()

