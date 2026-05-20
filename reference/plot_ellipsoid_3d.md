# Plot a nicheR ellipsoid in 3D environmental space

Creates an interactive 3D plot of a `nicheR_ellipsoid` object with
support for background points or suitability surfaces.

## Usage

``` r
plot_ellipsoid_3d(object, dim = c(1, 2, 3), wire = FALSE, aspect = TRUE,
                  background = NULL, prediction = NULL, col_layer = NULL,
                  pal = hcl.colors(100, palette = "Viridis"),
                  rev_pal = FALSE, bg_sample = NULL, col_ell = "#8b0000",
                  alpha_ell = 1, alpha_bg = 1, col_bg = "#8A8A8A",
                  fixed_lims = NULL, xlab = NULL, ylab = NULL,
                  zlab = NULL, ...)
```

## Arguments

- object:

  A `nicheR_ellipsoid` object constructed with at least 3 dimensions.

- dim:

  Integer vector of length 3. Indices of dimensions to plot.

- wire:

  Logical. If `TRUE`, plots wireframe. The default, `FALSE`, plots a
  shaded volume.

- aspect:

  Logical. If `TRUE`, maintains aspect ratio (1:1:1).

- background:

  Optional data frame/matrix of background points. This argument has
  priority over `prediction` if both are provided.

- prediction:

  Optional data frame/matrix for prediction surfaces.

- col_layer:

  Character or `NULL`. Column in `prediction` to use for coloring.

- pal:

  Color palette function or character vector.

- rev_pal:

  Logical. If `TRUE`, reverses the palette.

- bg_sample:

  Integer or `NULL`. Subsample size for large data.

- col_ell:

  Color of the ellipsoid. Default is `"#000000"`.

- alpha_ell:

  Transparency of the ellipsoid. Default is `1`. Not applied to
  wireframe mode.

- alpha_bg:

  Transparency of background points. Default is `1`. Also applied to
  prediction points.

- col_bg:

  Color for background points.

- fixed_lims:

  Named list with `xlim`, `ylim`, and `zlim`.

- xlab:

  x-axis label. The default, `NULL`, uses ellipsoid object variable
  names, if any found.

- ylab:

  y-axis label. The default, `NULL`, uses ellipsoid object variable
  names, if any found.

- zlab:

  z-axis label. The default, `NULL`, uses ellipsoid object variable
  names, if any found.

- ...:

  Additional graphical parameters.

## Examples

``` r
# Building an ellipsoid
## Define ranges for three variables
range <- data.frame(bio_1  = c(22, 32),
                    bio_12 = c(800, 4200),
                    bio_15 = c(45, 115))

## Build the ellipsoid
ell5 <- build_ellipsoid(range = range)
#> Starting: building ellipsoidal niche from ranges...
#> Step: computing covariance matrix...
#> Step: computing additional ellipsoidal niche metrics...
#> Done: created ellipsoidal niche.
ell5$cov_limits
#>                        min     max
#> bio_1-bio_12   -472.222222  935.00
#> bio_1-bio_15     -9.722222   19.25
#> bio_12-bio_15 -3305.555555 6545.00

ell5 <- update_ellipsoid_covariance(ell5, c("bio_1-bio_12" = 200,
                                            "bio_1-bio_15" = 0,
                                            "bio_12-bio_15" = -3000))
#> Starting: updating covariance values...
#> Step: computing ellipsoid metrics...
#> Done: updated ellipsoidal niche metrics

# Plot the ellipsoid in 3D
if (FALSE) { # \dontrun{
plot_ellipsoid_3d(ell5)
} # }
```
