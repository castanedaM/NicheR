# Add data to an existing 3D E-space plot

Add data to an existing 3D E-space plot

## Usage

``` r
add_data_3d(data, dim = c(1, 2, 3), col_layer = NULL, alpha = 1, ...)
```

## Arguments

- data:

  A data frame or matrix containing the points.

- dim:

  Integer vector of length 3. Indices of dimensions to plot.

- col_layer:

  Character or `NULL`. Column for coloring.

- alpha:

  Transparency for the points. Default is `1`.

- ...:

  Additional arguments passed to
  [`rgl::points3d`](https://dmurdoch.github.io/rgl/dev/reference/primitives.html).

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

ell5u <- update_ellipsoid_covariance(ell5, c("bio_1-bio_12" = 200,
                                             "bio_1-bio_15" = 0,
                                             "bio_12-bio_15" = -3000))
#> Starting: updating covariance values...
#> Step: computing ellipsoid metrics...
#> Done: updated ellipsoidal niche metrics
if (FALSE) { # \dontrun{
# Plot the ellipsoid in 3D
plot_ellipsoid_3d(ell5u)

# Add background points
add_data_3d(back_data[, c(3, 7, 10)], col = "#8A8A8A")
} # }
```
