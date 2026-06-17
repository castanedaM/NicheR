# Title: Instructions text for nicheR Shiny app
# Description: Centralized instructional copy shown above inputs (p() blocks)
#              and tooltip text shown via icon("circle-info"). Organized by
#              the tab/section each instruction belongs to.
# Date last updated: 06/16/2026

instructions <- list(

  # TAB 1: INPUTS

  data_input_type = HTML("Choose how to provide your background environmental
data: upload your own layers, resume a previous session, work entirely in
environmental space with no geography (Virtual Mode), or use the example
dataset bundled with nicheR."),

  data_upload = HTML("Background data: your study area's environmental
conditions.<br>
Upload a raster (.tif, .rds) and/or a CSV of the same data. If both are
provided they will be validated for consistency. Occurrence records are
used later."),

  prev_session = HTML("Upload a session file (.rds) saved from a previous
visit. The app will resume at whatever step you last left off, including
any confirmed variables and built ellipsoid if they were saved."),

  virtual_mode = HTML("Work entirely in environmental space (E-space), with
no geographic coordinates. You will name your variables and define their
ranges directly, without uploading any spatial data."),

  example_data = HTML("Use the example dataset bundled with nicheR: WorldClim
bioclimatic variables for Central America, including a bias layer for later
steps. Click Continue to select which of the 16 available variables to use."),

  # TAB 2: RANGE

  variable_settings = HTML(paste0("Select up to ", MAX_DIMS,
                                  " variables to use in your analysis.<br>
Uncheck a variable to exclude it. The plot beside you shows the background
distribution of your selected variables and updates as you choose.")),

  virtual_variables = HTML(paste0("Choose how many variables to work with
(up to ", MAX_DIMS, ") and give each one a name. Default names (var1, var2,
...) are used if left blank.")),

  edit_variables_tooltip = "Editing your variables will delete the current
ellipsoid and any covariance adjustments.",

  range_manual = HTML("Define the minimum and maximum values for each
variable.<br>
Defaults are the mean and mean + one standard deviation of your background
data. Lines in the plot update as you change these values. Use Reset to
Defaults to restore them, then Build Ellipsoid when ready."),

  range_data = HTML("Upload a CSV with occurrence or other environmental
data for your species of interest.<br>
Column names must match your selected variables exactly, or the upload
will not be recognized. Once matched, observed min/max values appear below,
and Expand Min/Max (%) let you widen the range outward in either direction.
Lines in the plot update as you adjust them."),

  range_stats = HTML("Provide summary statistics for the chosen variables.<br>
Defaults shown here are the mean and standard deviation of your background
data, but can be changed to known values for your species of interest.
Expand Min/Max (%) widen the resulting range outward, not the mean itself.
Confidence level controls how wide the range is before any expansion."),

  cl_range_tooltip = "Confidence level used with the chi-square distribution
when building the ellipsoid. Higher values produce a larger ellipsoid.",

  expand_range_tooltip = "Percentage to widen the range outward from the
observed or computed bounds. Can be negative to shrink the range inward.",

  # TAB 3: COVARIANCE

  covariance = HTML("Covariances describe the relationship between pairs of
variables. With 3 variables you get 3 pairs, with 4 variables you get 6, and
so on.<br>
Drag a slider to rotate the ellipsoid along that pair; the plot updates to
show the result in 2D or pairs view. Moving one slider updates the valid
range of the others. If a combination would make the ellipsoid invalid, an
error is shown and that slider resets. Click the rotate-left icon next to a
slider to reset that pair back to zero. Click Confirm when ready to move to
Prediction."),

  covariance_reset_tooltip = "Reset this pair's covariance back to zero.",


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

)
