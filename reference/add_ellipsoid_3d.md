# Add an ellipsoid to an existing 3D E-space plot

Add an ellipsoid to an existing 3D E-space plot

## Usage

``` r
add_ellipsoid_3d(object, dim = c(1, 2, 3), wire = FALSE, col_ell = "#800000",
                  alpha_ell = 1, ...)
```

## Arguments

- object:

  A `nicheR_ellipsoid` object.

- dim:

  Integer vector of length 3.

- wire:

  Logical. If `TRUE`, plots wireframe, otherwise plots a shaded volume.
  Default is `FALSE`.

- col_ell:

  Color of the ellipsoid. Default is `"#000000"`.

- alpha_ell:

  Transparency of the ellipsoid. Default is `1`. Not applied if
  `wire = TRUE`.

- ...:

  Additional arguments passed to
  [`rgl::wire3d`](https://dmurdoch.github.io/rgl/dev/reference/shade3d.html)
  or
  [`rgl::shade3d`](https://dmurdoch.github.io/rgl/dev/reference/shade3d.html).

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

# Add the original ellipsoid as a wireframe
add_ellipsoid_3d(ell5, wire = TRUE, col = "#0e008b")
} # }
```
