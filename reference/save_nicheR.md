# Save a nicheR object to disk

A wrapper around [`saveRDS`](https://rdrr.io/r/base/readRDS.html) to
save `nicheR_ellipsoid` or `nicheR_community` objects to a file.
Includes a safety check for overwriting existing files and ensures the
file extension is .rds.

## Usage

``` r
save_nicheR(object, file, overwrite = FALSE, ...)
```

## Arguments

- object:

  A `nicheR_ellipsoid` or `nicheR_community` object.

- file:

  Character. The connection or name of the file where the object will be
  saved (usually ending in ".rds").

- overwrite:

  Logical. If `TRUE`, an existing file at the specified path will be
  replaced. Default is `FALSE`.

- ...:

  Additional arguments passed to
  [`saveRDS`](https://rdrr.io/r/base/readRDS.html).

## Value

No return value. Saves the object to the specified path.

## Examples

``` r
# Build a simple ellipsoid to save
range_df <- data.frame(bio_1  = c(15, 25),
                       bio_12 = c(500, 1500))
ell <- build_ellipsoid(range = range_df)
#> Starting: building ellipsoidal niche from ranges...
#> Step: computing covariance matrix...
#> Step: computing additional ellipsoidal niche metrics...
#> Done: created ellipsoidal niche.

# Save to a temporary file
tmp <- tempfile(fileext = ".rds")
save_nicheR(ell, file = tmp)

# Overwrite the same file
save_nicheR(ell, file = tmp, overwrite = TRUE)


```
