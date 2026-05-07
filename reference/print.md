# Print method for nicheR objects

Provides a concise summary of `nicheR` objects.

## Usage

``` r
# S3 method for class 'nicheR_ellipsoid'
print(x, digits = 3, ...)

# S3 method for class 'nicheR_community'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An object of the classes `"nicheR_ellipsoid"` or `"nicheR_community"`.

- digits:

  Integer. Number of decimal places used when printing numeric values.
  Default is 3.

- ...:

  Additional arguments.

## Value

The input object `x`, returned invisibly.

## Details

The function formats and rounds key quantities for readability but does
not modify the underlying object.

## See also

[`build_ellipsoid`](https://castanedam.github.io/nicheR/reference/build_ellipsoid.md)

## Examples

``` r
range_df <- data.frame(bio_1  = c(22, 28),
                       bio_12 = c(1000, 3500))
ell <- build_ellipsoid(range = range_df)
#> Starting: building ellipsoidal niche from ranges...
#> Step: computing covariance matrix...
#> Step: computing additional ellipsoidal niche metrics...
#> Done: created ellipsoidal niche.
print(ell)
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
```
