# nicheR 0.2.0

## New features

* `run_app()` launches a Shiny interface to the package workflow. The
  application covers building ellipsoids from environmental ranges, predicting
  suitability, preparing and applying sampling bias, and generating virtual
  occurrences. Sessions can be saved and reloaded, and several ellipsoids can
  be carried through the workflow at once for comparison.

* `update_ellipsoid_centroid()` translates an ellipsoid through environmental
  space, leaving its shape, size, and orientation unchanged. Use it to ask
  what a species with the same tolerance breadth would look like centred on
  different conditions.

## Breaking changes

* [`predict()` now returns a single multi-layer `SpatRaster` for `SpatRaster`
  input, rather than a list of single-layer rasters. Layer names match the
  column names returned for data frame input. Code that extracted layers by
  name, such as `pred$suitability` or `pred[["suitability"]]`, continues to
  work. Code that relied on list behaviour, such as `lapply()` over the
  result or `Reduce(c, pred)`, needs updating.]

## Documentation

* New vignette, `shiny_app`, documenting the interface.

# nicheR 0.1.0

* Initial CRAN submission.
