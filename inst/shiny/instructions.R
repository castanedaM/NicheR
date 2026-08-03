# Title: Instructions text for nicheR Shiny app
# Description: Centralized instructional copy shown above inputs (p() blocks)
#              and tooltip text shown via icon("circle-info"). Organized by
#              the tab/section each instruction belongs to.
# Dependencies: depends on MAX_DIMS from global.R
# Date last updated: 08/03/2026

instructions <- list(

  # ABOUT TAB

  about_app = HTML("An R package for ellipsoid-based ecological niche
modeling. Build niche models from environmental ranges, predict suitable
areas, account for sampling bias, and generate virtual occurrences, all in
a single reproducible workflow."),

  about_build = HTML("Define a species niche as an ellipsoid in environmental
space using background layers and user-defined variable ranges."),

  about_build_points = c(
    "Set ranges manually, from occurrence data, or from background statistics",
    "Adjust covariance to rotate the ellipsoid",
    "Move the centroid without rebuilding",
    "Save multiple named versions"
  ),

  about_predict = HTML("Project the ellipsoid onto raster layers or a data
frame to produce continuous or binary suitability surfaces."),

  about_predict_points = c(
    "Output: suitability, Mahalanobis distance, and truncated versions",
    "Binarization uses the ellipsoid's own confidence level",
    "E-space and G-space plots included",
    "Batch predict across multiple saved versions"
  ),

  about_bias = HTML("Upload a sampling bias raster to weight occurrence
generation toward areas with specific detection effort."),

  about_bias_points = c(
    "Optional but important for data-limited species",
    "Accepts any raster matching the background extent",
    "Higher cell values increase occurrence probability",
    "Examples: urbanization, distance to water, road density, collector coverage, any detection proxy"
  ),

  about_generate = HTML("Sample virtual presences from the fitted niche,
optionally weighted by the bias layer."),

  about_generate_points = c(
    "Specify number of presences and background ratio",
    "Output is a data frame of coordinates and environmental values",
    "Designed for virtual species and simulation workflows",
    "Supports rare-species SDM validation studies"
  ),

  # BUILD TAB: INPUTS

  build_raster_file_tooltip = "Environmental conditions of the study area in raster format.
Provides both environmental and geographic space. Accepted: .tif, .rds",

  build_df_file_tooltip = "The same data in tabular form. Geographic space is
available only if the file has coordinate columns on a regular grid. Accepted: .csv, .rds",

  data_input_type = HTML("Choose how to provide your background environmental
data"),

  data_upload = HTML("Background data: your study area's environmental
conditions.<br>
Upload either a raster (.tif, .rds) or a CSV of the same data. Once one is
provided the other is hidden. Use Clear files to switch."),

  no_spatial_cols = HTML("No coordinate columns were found. The app looks for
x, lon, longitude, or easting paired with y, lat, latitude, or northing. You
can still build and inspect the ellipsoid in environmental space, but
geographic maps will not be available."),

  irregular_grid = HTML("Coordinate columns were found but do not form a
regular grid, so they could not be converted to a raster. Work will continue
in environmental space only."),

  prev_session = HTML("Upload a session file (.rds) saved from a previous
visit. The app will resume at whatever step you last left off, including
any confirmed variables and built ellipsoid if they were saved."),

  virtual_mode = HTML("Work entirely in environmental space (E-space), with
no geographic coordinates. You will name your variables and define their
ranges directly, without uploading any spatial data."),

  example_data = HTML("Use the example dataset bundled with nicheR: WorldClim
bioclimatic variables for Central America, including a bias layer for later
steps. Click Continue to select which of the 16 available variables to use."),

  # BUILD TAB: RANGE

  build_variable_settings = HTML(paste0("Select up to ", MAX_DIMS,
                                  " variables to use in your analysis.<br>
Uncheck a variable to exclude it. The plot beside you shows the background
distribution of your selected variables and updates as you choose.")),

  build_virtual_variables = HTML(paste0("Choose how many variables to work with
(up to ", MAX_DIMS, ") and give each one a name. Default names (var1, var2,
...) are used if left blank.")),

  build_edit_variables_tooltip = "Editing your variables will delete the current
selection, the ellipsoid, and any covariance adjustments.",

  build_range_manual = HTML("Define the minimum and maximum values for each
variable.<br>
Defaults are the mean and mean + one standard deviation of your background
data. Lines in the plot update as you change these values. Use Reset to
Defaults to restore them, then Build Ellipsoid when ready."),

  build_range_data = HTML("Upload a CSV with occurrence or other environmental
data for your species of interest.<br>
Column names must match your selected variables exactly, or the upload
will not be recognized. Once matched, observed min/max values appear below,
and Expand Min/Max (%) let you widen the range outward in either direction.
Lines in the plot update as you adjust them."),

  build_range_stats = HTML("Provide summary statistics for the chosen variables.<br>
Defaults shown here are the mean and standard deviation of your background
data, but can be changed to known values for your species of interest.
Expand Min/Max (%) widen the resulting range outward, not the mean itself.
Confidence level controls how wide the range is before any expansion."),

  build_cl_range_tooltip = "Confidence level used with the chi-square distribution
when building the ellipsoid. Higher values produce a larger ellipsoid.",

  build_expand_range_tooltip = "Percentage to widen the range outward from the
observed or computed bounds. Can be negative to shrink the range inward.",

  # BUILD TAB: COVARIANCE

  build_covariance = HTML("Covariances describe the relationship between pairs of
variables. With 3 variables you get 3 pairs, with 4 variables you get 6, and
so on.<br>
Drag a slider to rotate the ellipsoid along that pair; the plot updates to
show the result in 2D or pairs view. Moving one slider updates the valid
range of the others. If a combination would make the ellipsoid invalid, an
error is shown and that slider resets. Click the rotate-left icon next to a
slider to reset that pair back to zero. Click Confirm when ready to move to
Prediction."),

  build_covariance_reset_tooltip = "Reset this pair's covariance back to zero.",

  # PREDICT TAB

  build_adjust_trunc_tooltip = "Adjust the level of truncation within the current
ellipsoid. This truncates the prediction inwards.",

  # BIAS TAB

  bias = "Bias input is intended to add controlled sampling bias to a prediction layer. Note that once bias has been applied, the prediction is no longer a probability. In the steps below you will need one or more raster layers (.rds, .tif) that will be standardized to a 0-1 scale and can have a direct effect (increase sampling probability) or inverse effect (decrease sampling probability).",

  bias_input = "Upload one or more raster layers (.rds, .tif) to represent sampling bias across the study area.",

  bias_input_example = "Upload one or more raster layers (.rds, .tif) to represent sampling bias across the study area or use example layers provided.",

  # PLOT SETTINGS (shared across tabs that show build_espace_plot)

  plot_settings = HTML("Adjust point shape, size, and colors, toggle range
lines, the ellipsoid, and centroid on or off, and zoom in manually or to
the ellipsoid's extent."),

  plot_zoom_tooltip = "Auto fits the plot to your data and ellipsoid.
Zoom to ellipsoid frames the plot tightly around the ellipsoid boundary.
Manual lets you set exact axis limits.",


  session_save = HTML("Save your current progress as an .rds file. This
  includes confirmed variables and the built ellipsoid, if any. Load it
  later from the Previous Session input option to resume where you left
  off.<br>
  If no ellipsoid has been confirmed yet, only the most recent
  in-progress values are saved.")

  # GENERATE TAB

)
