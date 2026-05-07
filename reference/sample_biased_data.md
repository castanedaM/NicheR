# Sample occurrence data from a bias-weighted prediction surface

Samples `n_occ` virtual occurrence points using the bias-weighted
prediction values directly as sampling probabilities. Unlike
[`sample_data()`](https://castanedam.github.io/nicheR/reference/sample_data.md),
there is no sampling strategy argument — the prediction layer values
themselves define where points are drawn from, making this function
suited for simulating realistically biased occurrence records.

## Usage

``` r
sample_biased_data(
  n_occ,
  prediction,
  prediction_layer = NULL,
  sampling_mask = NULL,
  seed = 1,
  verbose = TRUE,
  strict = NULL
)
```

## Arguments

- n_occ:

  Integer. Number of occurrence points to sample.

- prediction:

  A `SpatRaster` or data frame containing the bias-weighted prediction
  surface to sample from.

- prediction_layer:

  Character. Name of the layer or column to use as sampling weights.
  Required when `prediction` contains multiple layers or columns.

- sampling_mask:

  A `SpatRaster` or `SpatVector` used to restrict sampling to a
  geographic area. Only supported when `prediction` is a `SpatRaster`.

- seed:

  Integer. Random seed for reproducibility. Default is `1`.

- verbose:

  Logical. If `TRUE` (default), prints progress messages.

- strict:

  Logical or `NULL`. If `TRUE`, removes `NA` and zero-valued cells
  before sampling. If `NULL` (default), auto-detected from the layer
  name and the proportion of zeros and `NA`s in the prediction values.

## Value

A data frame of sampled occurrence points with the same columns as the
input `prediction` (minus the internal `pred` column). If `prediction`
is a `SpatRaster`, the output includes `x` and `y` coordinate columns.

## Details

Prediction values are used directly as sampling weights, so they must be
non-negative. Higher values correspond to higher sampling probability,
reflecting areas of greater bias (e.g., higher detectability or observer
effort). This is in contrast to
[`sample_data()`](https://castanedam.github.io/nicheR/reference/sample_data.md),
which transforms prediction values according to a `sampling` and
`method` argument.

Auto-detection of `strict` follows the same logic as
[`sample_data()`](https://castanedam.github.io/nicheR/reference/sample_data.md):
it is set to `TRUE` if the layer name contains `"trunc"` or if the
proportion of zeros or `NA`s exceeds 25%.

## See also

[`sample_data`](https://castanedam.github.io/nicheR/reference/sample_data.md)
for unbiased sampling with explicit strategy and method control,
[`apply_bias`](https://castanedam.github.io/nicheR/reference/apply_bias.md)
for generating the bias-weighted prediction surface used as input here.

## Examples

``` r
# \donttest{
range_df <- data.frame(bio_1  = c(22, 28),
                       bio_12 = c(1000, 3500),
                       bio_15 = c(50, 70))
ell <- nicheR::build_ellipsoid(range = range_df)
#> Starting: building ellipsoidal niche from ranges...
#> Step: computing covariance matrix...
#> Step: computing additional ellipsoidal niche metrics...
#> Done: created ellipsoidal niche.

ma_bios <- terra::rast(
  system.file("extdata/ma_bios.tif", package = "nicheR"))

pred_rast <- predict(ell,
                     newdata               = ma_bios[[ell$var_names]],
                     include_suitability   = TRUE,
                     suitability_truncated = TRUE)
#> Starting: suitability prediction using newdata of class: SpatRaster...
#> Step: Using 3 predictor variables: bio_1, bio_12, bio_15
#> Done: Prediction completed successfully. Returned raster layers: Mahalanobis, suitability, suitability_trunc

bias_rast <- terra::rast(
  system.file("extdata/ma_biases.tif", package = "nicheR"))

bias <- nicheR::prepare_bias(bias_surface     = bias_rast[[1]],
                     effect_direction = "direct")
#> Starting: prepare_bias()
#> Step: splitting SpatRaster into layers...
#> Step: bias_surface is a SpatRaster. Using first layer as template surface...
#> Step: mask_na = FALSE. Expanding template to union extent (keeping finest resolution).
#> Step: standarizing (min/max) and applying direction of effect to 1 bias layer/s...
#> Step: building standarized (min/max) directional composite bias surface (mask_na = FALSE)...
#> Done: prepare_bias()

biased_pred <- nicheR::apply_bias(prepared_bias    = bias,
                          prediction       = pred_rast,
                          prediction_layer = "suitability")
#> Starting: apply_bias()
#> Step: applying bias with 'direct' effect to to "suitability" layer...
#> Done: apply_bias(). Note: values are no longer probabilities

occ_biased <- nicheR::sample_biased_data(n_occ = 100,
                                         prediction = biased_pred,
                                         prediction_layer = "suitability_biased_direct")
#> Starting: sample_biased_data()
#> Done: sampled 100 points from biased prediction layer

head(occ_biased)
#>               x         y suitability_biased_direct
#> 33290 -71.75000  6.916667                0.28855259
#> 24329 -85.25000 13.083333                0.24032668
#> 29880 -80.08333  9.250000                0.17136800
#> 21686 -85.75000 14.916667                0.02813328
#> 33770 -71.75000  6.583333                0.32674086
#> 17346 -89.08333 17.916667                0.03220198
# }
```
