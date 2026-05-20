# Build a probabilistic ellipsoidal niche from ranges

Builds an ellipsoidal niche in multivariate environmental space using a
multivariate normal (MVN) contour defined by a constant Mahalanobis
distance. The ellipsoid is parameterized from user-provided variable
ranges by deriving a centroid and marginal standard deviations, and
assuming a diagonal covariance matrix.

## Usage

``` r
build_ellipsoid(range, cl = 0.99,
                       verbose = TRUE)
```

## Arguments

- range:

  A 2-row `matrix` or `data.frame` of bounds, with variables as columns.
  Rows may be ordered as min/max or max/min. Column names are required
  and used as variable names.

- cl:

  Numeric confidence level in (0, 1). Used to compute the chi-square
  cutoff defining the ellipsoid contour.

- verbose:

  Logical; if `TRUE`, prints brief progress messages.

## Value

An object of class `"nicheR_ellipsoid"` produced by
[`ellipsoid_calculator`](https://castanedam.github.io/nicheR/reference/ellipsoid_calculator.md)
(via
[`new_nicheR_ellipsoid`](https://castanedam.github.io/nicheR/reference/new_nicheR_ellipsoid.md)),
containing ellipsoid geometry and associated quantities (e.g., centroid,
covariance matrix, chi-square cutoff, semi-axis lengths, axis vertex
coordinates, volume, and covariance limits).

## Details

`range` must be a 2-row `matrix` or `data.frame` with variables in
columns. Rows represent lower and upper bounds for each variable (row
order may be min/max or max/min). The centroid is computed as: \$\$\mu_i
= (m_i + M_i)/2.\$\$

Marginal standard deviations are derived assuming bounds represent
approximately \\\pm 3\\ standard deviations: \$\$\sigma_i = (M_i -
m_i)/6,\$\$ and a diagonal covariance matrix is assumed: \$\$\Sigma =
\mathrm{diag}(\sigma_1^2,\dots,\sigma_n^2).\$\$

The ellipsoid contour is defined using a chi-square cutoff \\c^2 =
\chi^2\_{n}(\mathrm{cl})\\, where \\n\\ is the number of variables.

## Examples

``` r
# Two-dimensional ellipsoid from environmental ranges
range_df <- data.frame(bio_1  = c(22, 28),
                       bio_12 = c(1000, 3500))
ell2d <- build_ellipsoid(range = range_df)
#> Starting: building ellipsoidal niche from ranges...
#> Step: computing covariance matrix...
#> Step: computing additional ellipsoidal niche metrics...
#> Done: created ellipsoidal niche.
ell2d
#> nicheR Ellipsoid Object
#> -----------------------
#> Dimensions:        2D
#> Chi-square cutoff: 9.21
#> Centroid (mu):     25, 2250
#> 
#> Covariance matrix:
#>        bio_1   bio_12
#> bio_1      1      0.0
#> bio_12     0 173611.1
#> 
#> Covariance Limits:
#>                   min     max
#> bio_1-bio_12 -416.667 416.667
#> 
#> Ellipsoid semi-axis lengths:
#>   1264.523, 3.035
#> 
#> Ellipsoid axis endpoints:
#>  Axis 1:
#>          bio_1   bio_12
#> vertex_a    25  985.477
#> vertex_b    25 3514.523
#> 
#>  Axis 2:
#>           bio_1 bio_12
#> vertex_a 28.035   2250
#> vertex_b 21.965   2250
#> 
#> Ellipsoid volume:  12056.31
#> 

# Three-dimensional ellipsoid
range_3d <- data.frame(bio_1  = c(22, 28),
                       bio_12 = c(1000, 3500),
                       bio_15 = c(50, 70))
ell3d <- build_ellipsoid(range = range_3d)
#> Starting: building ellipsoidal niche from ranges...
#> Step: computing covariance matrix...
#> Step: computing additional ellipsoidal niche metrics...
#> Done: created ellipsoidal niche.
ell3d
#> nicheR Ellipsoid Object
#> -----------------------
#> Dimensions:        3D
#> Chi-square cutoff: 11.345
#> Centroid (mu):     25, 2250, 60
#> 
#> Covariance matrix:
#>        bio_1   bio_12 bio_15
#> bio_1      1      0.0  0.000
#> bio_12     0 173611.1  0.000
#> bio_15     0      0.0 11.111
#> 
#> Covariance Limits:
#>                    min    max
#> bio_1-bio_12  -208.333  412.5
#> bio_1-bio_15    -1.667    3.3
#> bio_12-bio_15 -694.444 1375.0
#> 
#> Ellipsoid semi-axis lengths:
#>   1403.423, 11.227, 3.368
#> 
#> Ellipsoid axis endpoints:
#>  Axis 1:
#>          bio_1   bio_12 bio_15
#> vertex_a    25  846.577     60
#> vertex_b    25 3653.423     60
#> 
#>  Axis 2:
#>          bio_1 bio_12 bio_15
#> vertex_a    25   2250 48.773
#> vertex_b    25   2250 71.227
#> 
#>  Axis 3:
#>           bio_1 bio_12 bio_15
#> vertex_a 21.632   2250     60
#> vertex_b 28.368   2250     60
#> 
#> Ellipsoid volume:  222308.1
#> 
```
