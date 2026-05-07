# Sample occurrence data from a prediction surface

Samples `n_occ` virtual occurrence points from a suitability or
Mahalanobis distance prediction surface. Supports centroid, edge, and
random sampling strategies, and accepts both raster (`SpatRaster`) and
data frame inputs.

## Usage

``` r
sample_data(
  n_occ,
  prediction,
  prediction_layer = NULL,
  sampling = "centroid",
  method = "suitability",
  sampling_mask = NULL,
  seed = 1,
  strict = NULL,
  verbose = TRUE
)
```

## Arguments

- n_occ:

  Integer. Number of occurrence points to sample.

- prediction:

  A `SpatRaster` or data frame containing the prediction surface to
  sample from.

- prediction_layer:

  Character. Name of the layer or column to use as the prediction
  values. Required when `prediction` contains multiple layers or
  columns.

- sampling:

  Character. Sampling strategy. One of `"centroid"` (default), `"edge"`,
  or `"random"`. Controls where within the niche points are
  preferentially drawn from.

- method:

  Character. Weighting method. One of `"suitability"` (default) or
  `"mahalanobis"`. Must match the type of values in `prediction_layer`:
  suitability values must be in `[0, 1]`, Mahalanobis values must be
  non-negative.

- sampling_mask:

  A `SpatRaster` or `SpatVector` used to restrict sampling to a
  geographic area. Only supported when `prediction` is a `SpatRaster`.

- seed:

  Integer. Random seed for reproducibility. Default is `1`.

- strict:

  Logical or `NULL`. If `TRUE`, removes `NA` and zero-valued cells
  before sampling (recommended with truncated prediction layers). If
  `NULL` (default), auto-detected from the layer name and the proportion
  of zeros and `NA`s in the prediction values.

- verbose:

  Logical. If `TRUE` (default), prints progress messages.

## Value

A data frame of sampled occurrence points with the same columns as the
input `prediction` (minus the internal `pred` column). If `prediction`
is a `SpatRaster`, the output includes `x` and `y` coordinate columns.

## Details

The `sampling` and `method` arguments interact to define the probability
weights used when drawing points:

- `sampling = "centroid"`, `method = "suitability"`: weights
  proportional to suitability — higher near the niche center.

- `sampling = "edge"`, `method = "suitability"`: weights proportional to
  \\1 - \text{suitability}\\ — higher near the niche boundary.

- `sampling = "centroid"`, `method = "mahalanobis"`: weights inversely
  proportional to Mahalanobis distance — higher near the centroid.

- `sampling = "edge"`, `method = "mahalanobis"`: weights proportional to
  Mahalanobis distance — higher near the boundary.

- `sampling = "random"`: equal weights regardless of method.

When `strict = NULL`, the function auto-detects truncation by checking
whether the layer name contains `"trunc"` or whether the proportion of
zeros or `NA`s exceeds 25%.

## Examples

``` r
range_df <- data.frame(bio_1  = c(22, 28),
                       bio_12 = c(1000, 3500))
ell <- build_ellipsoid(range = range_df)
#> Starting: building ellipsoidal niche from ranges...
#> Step: computing covariance matrix...
#> Step: computing additional ellipsoidal niche metrics...
#> Done: created ellipsoidal niche.

# \donttest{
ma_bios <- terra::rast(
  system.file("extdata/ma_bios.tif", package = "nicheR"))
back_df <- as.data.frame(ma_bios, xy = TRUE)

pred_df <- predict(ell,
                   newdata = back_df,
                   include_suitability = TRUE,
                   include_mahalanobis = FALSE,
                   suitability_truncated = TRUE)
#> Starting: suitability prediction using newdata of class: data.frame...
#> Step: Identified spatial columns: x, y
#> Step: Ignoring extra predictor columns: bio_5, bio_6, bio_7, bio_13, bio_14, bio_15
#> Step: Using 2 predictor variables: bio_1, bio_12
#> Done: Prediction completed successfully. Returned columns: x, y, bio_1, bio_12, suitability, suitability_trunc

# Centroid strategy: samples cluster near the niche center
occ_centroid <- nicheR::sample_data(n_occ = 100,
                            prediction = pred_df,
                            prediction_layer = "suitability_trunc",
                            sampling = "centroid",
                            method = "suitability",
                            strict = TRUE)
#> Starting: sample_data()
#> Done: sampled 100 points.
head(occ_centroid)
#>               x         y    bio_1 bio_12 suitability suitability_trunc
#> 34303 -62.91667  6.250000 25.82385   2194  0.70581667        0.70581667
#> 23138 -83.75000 13.916667 25.35428   2647  0.59650286        0.59650286
#> 21688 -85.41667 14.916667 24.15773   1828  0.41996126        0.41996126
#> 34500 -70.08333  6.083333 27.15791   2325  0.09589722        0.09589722
#> 33770 -71.75000  6.583333 25.07655   2536  0.78780669        0.78780669
#> 26248 -85.41667 11.750000 26.53833   1641  0.10525463        0.10525463

# Edge strategy: samples spread toward the niche boundary
occ_edge <- nicheR::sample_data(n_occ = 100,
                        prediction = pred_df,
                        prediction_layer = "suitability_trunc",
                        sampling = "edge",
                        method = "mahalanobis",
                        strict = TRUE)
#> Starting: sample_data()
#> Done: sampled 100 points.

# Random strategy: samples distributed uniformly across suitable area
occ_random <- nicheR::sample_data(n_occ = 100,
                          prediction = pred_df,
                          prediction_layer = "suitability_trunc",
                          sampling = "random")
#> Starting: sample_data()
#> Step: auto-detected a likely truncated prediction surface. Setting 'strict = TRUE' and removing NA and zero values. You can override this behavior with the 'strict' argument...
#> Done: sampled 100 points.
# }
```
