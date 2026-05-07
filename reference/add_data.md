# Add occurrence points or other data to an existing E-space plot

Adds points to an existing environmental space plot created with
[`plot_ellipsoid()`](https://castanedam.github.io/nicheR/reference/plot_ellipsoid.md).
Points can be plotted with a single color or colored by a continuous
variable (e.g., suitability) using a color palette.

## Usage

``` r
add_data(
  data,
  x,
  y,
  pts_col = "#000000",
  pts_alpha = 1,
  col_layer = NULL,
  pal = hcl.colors(100, palette = "Viridis"),
  rev_pal = FALSE,
  pch = 1,
  cex = 1,
  bg_sample = NULL,
  ...
)
```

## Arguments

- data:

  A data frame containing the points to plot. Must include columns
  matching `x` and `y`, and `col_layer` if provided.

- x:

  Character. Name of the column to use as the x-axis variable.

- y:

  Character. Name of the column to use as the y-axis variable.

- pts_col:

  Character. Color for all points when `col_layer` is `NULL`. Default is
  `"#000000"` (black).

- pts_alpha:

  Numeric in `[0, 1]`. Transparency of points when `col_layer` is
  `NULL`. Default is `1` (fully opaque).

- col_layer:

  Character or `NULL`. Name of a column in `data` to use for coloring
  points by a continuous variable. If `NULL` (default), all points are
  drawn with `pts_col`.

- pal:

  A color palette function or character vector of colors used when
  `col_layer` is provided. Default is
  `hcl.colors(100, palette = "Viridis")`.

- rev_pal:

  Logical. If `TRUE`, reverses the color palette before applying it.
  Default is `FALSE`.

- pch:

  Integer or character. Point symbol. Default is `1`.

- cex:

  Numeric. Size scaling for points. Default is `1`.

- bg_sample:

  Integer or `NULL`. If provided and `nrow(data)` exceeds this value, a
  random subsample of this size is drawn before plotting. Useful for
  large data frames. Default is `NULL` (plot all).

- ...:

  Additional arguments passed to
  [`points`](https://rdrr.io/r/graphics/points.html).

## Value

Called for its side effect of adding points to the current plot. Returns
`NULL` invisibly.

## Details

When `col_layer` is provided, points are colored by the values of that
column mapped onto the palette. `NA`s in `col_layer` are removed before
plotting; zeros are retained as valid values (e.g., truncated
suitability predictions outside the ellipsoid boundary).

## See also

[`plot_ellipsoid`](https://castanedam.github.io/nicheR/reference/plot_ellipsoid.md),
[`add_ellipsoid`](https://castanedam.github.io/nicheR/reference/add_ellipsoid.md)

## Examples

``` r
range_df <- data.frame(bio_1 = c(15, 25), bio_12 = c(500, 1500))
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
                     include_mahalanobis = FALSE)
#> Starting: suitability prediction using newdata of class: data.frame...
#> Step: Identified spatial columns: x, y
#> Step: Ignoring extra predictor columns: bio_5, bio_6, bio_7, bio_13, bio_14, bio_15
#> Step: Using 2 predictor variables: bio_1, bio_12
#> Done: Prediction completed successfully. Returned columns: x, y, bio_1, bio_12, suitability

  # Open base plot then add centroid as a cross
  nicheR::plot_ellipsoid(ell,
                         background = back_df,
                         col_ell = "#e10000", col_bg = "grey80",
                         lwd = 2, pch = 20, cex_bg = 0.4,
                         xlab = "Bio1", ylab = "Bio12")

  nicheR::add_data(as.data.frame(t(ell$centroid)),
                   x = "bio_1", y = "bio_12",
                   pts_col = "#e10000", pch = 8, cex = 1.5, lwd = 2)


  # Add points colored by suitability on top of background
  nicheR::plot_ellipsoid(ell,
                         background = back_df,
                         col_ell = "#e10000", col_bg = "grey80",
                         lwd = 2, pch = 20, cex_bg = 0.4,
                         xlab = "Bio1", ylab = "Bio12")

  nicheR::add_data(pred_df,
                   x = "bio_1", y = "bio_12",
                   col_layer = "suitability",
                   pch = 20, cex = 0.5)

# }
```
