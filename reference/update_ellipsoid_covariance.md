# Update covariances in a nicheR ellipsoid and recompute metrics

Updates one or more off-diagonal covariance values in a
`nicheR_ellipsoid` object and recomputes all ellipsoid metrics
(centroid, semi-axes, volume, etc.) from the new covariance matrix. This
allows iterative niche shaping by adjusting the rotation and correlation
structure of the ellipsoid without rebuilding it from scratch.

## Usage

``` r
update_ellipsoid_covariance(object, covariance, tol = 1e-06, verbose = TRUE)
```

## Arguments

- object:

  A `nicheR_ellipsoid` object, typically created with
  [`build_ellipsoid`](https://castanedam.github.io/nicheR/reference/build_ellipsoid.md).

- covariance:

  Either a single numeric value applied to all off-diagonal elements, or
  a named numeric vector where names identify the variable pair in the
  format `"var1-var2"` (e.g., `c("bio_1-bio_12" = 0.3)`).

- tol:

  Small positive number used as tolerance when computing safe covariance
  limits for positive definiteness. Default is `1e-6`.

- verbose:

  Logical. If `TRUE` (default), prints progress messages.

## Value

A `nicheR_ellipsoid` object with the updated covariance matrix and
recomputed ellipsoid metrics. An additional element
`cov_limits_remaining` is attached, giving the remaining safe covariance
limits for any variable pairs that still have zero covariance.

## Details

Covariance values control the orientation and correlation structure of
the ellipsoid in environmental space. Setting a positive covariance
between two variables tilts the ellipsoid so that high values of one
variable tend to co-occur with high values of the other. Negative
covariance tilts it in the opposite direction.

The updated covariance matrix must remain positive definite — if the
requested value would violate this, use
[`covariance_limits`](https://castanedam.github.io/nicheR/reference/covariance_limits.md)
to find the safe range before calling this function.

## See also

[`build_ellipsoid`](https://castanedam.github.io/nicheR/reference/build_ellipsoid.md)
to create the initial ellipsoid,
[`covariance_limits`](https://castanedam.github.io/nicheR/reference/covariance_limits.md)
to find valid covariance ranges before updating.

## Examples

``` r
range_df <- data.frame(bio_1  = c(22, 28),
                       bio_12 = c(1000, 3500))
ell <- build_ellipsoid(range = range_df)
#> Starting: building ellipsoidal niche from ranges...
#> Step: computing covariance matrix...
#> Step: computing additional ellipsoidal niche metrics...
#> Done: created ellipsoidal niche.

# Check covariance allowed covariance range
ell$cov_limits
#>                    min      max
#> bio_1-bio_12 -416.6667 416.6667

# Introduce a negative correlation between bio_1 and bio_12
cov_limits <- c("bio_1-bio_12" = -100)
ell_corr <- update_ellipsoid_covariance(object = ell,
                                          covariance =  cov_limits)
#> Starting: updating covariance values...
#> Step: computing ellipsoid metrics...
#> Done: updated ellipsoidal niche metrics
ell_corr
#> nicheR Ellipsoid Object
#> -----------------------
#> Dimensions:        2D
#> Chi-square cutoff: 9.21
#> Centroid (mu):     25, 2250
#> 
#> Covariance matrix:
#>        bio_1   bio_12
#> bio_1      1   -100.0
#> bio_12  -100 173611.1
#> 
#> Covariance Limits:
#>                   min     max
#> bio_1-bio_12 -416.667 416.667
#> 
#> Ellipsoid semi-axis lengths:
#>   1264.523, 2.946
#> 
#> Ellipsoid axis endpoints:
#>  Axis 1:
#>           bio_1   bio_12
#> vertex_a 25.728  985.477
#> vertex_b 24.272 3514.523
#> 
#>  Axis 2:
#>           bio_1   bio_12
#> vertex_a 27.946 2250.002
#> vertex_b 22.054 2249.998
#> 
#> Ellipsoid volume:  11703.94
#> 
```
