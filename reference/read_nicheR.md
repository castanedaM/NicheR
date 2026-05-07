# Read a nicheR object from disk

A wrapper around [`readRDS`](https://rdrr.io/r/base/readRDS.html) to
load saved nicheR objects back into the R environment.

## Usage

``` r
read_nicheR(file)
```

## Arguments

- file:

  Character. The path to the file to be read.

## Value

The saved `nicheR_ellipsoid` or `nicheR_community` object.

## See also

[`save_nicheR`](https://castanedam.github.io/nicheR/reference/save_nicheR.md)

## Examples

``` r
# Build and save an ellipsoid first
range_df <- data.frame(bio_1  = c(15, 25),
                       bio_12 = c(500, 1500))
ell <- build_ellipsoid(range = range_df)
#> Starting: building ellipsoidal niche from ranges...
#> Step: computing covariance matrix...
#> Step: computing additional ellipsoidal niche metrics...
#> Done: created ellipsoidal niche.

tmp <- tempfile(fileext = ".rds")
save_nicheR(ell, file = tmp)

# Read it back
ell_loaded <- read_nicheR(tmp)
ell_loaded
#> nicheR Ellipsoid Object
#> -----------------------
#> Dimensions:        2D
#> Chi-square cutoff: 9.21
#> Centroid (mu):     20, 1000
#> 
#> Covariance matrix:
#>        bio_1   bio_12
#> bio_1  2.778     0.00
#> bio_12 0.000 27777.78
#> 
#> Covariance Limits:
#>                   min     max
#> bio_1-bio_12 -277.778 277.778
#> 
#> Ellipsoid semi-axis lengths:
#>   505.809, 5.058
#> 
#> Ellipsoid axis endpoints:
#>  Axis 1:
#>          bio_1   bio_12
#> vertex_a    20  494.191
#> vertex_b    20 1505.809
#> 
#>  Axis 2:
#>           bio_1 bio_12
#> vertex_a 25.058   1000
#> vertex_b 14.942   1000
#> 
#> Ellipsoid volume:  8037.538
#> 

```
