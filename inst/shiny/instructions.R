# Title: Instructions text for nicheR Shiny app
# Description: Centralized instructional copy shown above inputs (p() blocks)
#              and tooltip text shown via icon("circle-info"). Organized by
#              the tab and section each instruction belongs to. Keys are
#              prefixed with the tab they belong to, matching the input and
#              output naming convention used in the server scripts.
# Dependencies: depends on MAX_DIMS from global.R
# Date last updated: 08/03/2026

instructions <- list(

  # ABOUT TAB --------------------------------------------------------------

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


  # BUILD TAB: INPUTS ------------------------------------------------------

  build_data_input_type = HTML("Choose how to provide your background
environmental data."),

  build_data_upload = HTML("Background data: your study area's environmental
conditions.<br>
Upload either a raster (.tif, .rds) or a CSV of the same data. Once one is
provided the other is hidden. Use Clear files to switch."),

  build_raster_file_tooltip = "Environmental conditions of the study area in raster format.
Provides both environmental and geographic space. Accepted: .tif, .rds",

  build_df_file_tooltip = "The same data in tabular form. Geographic space is
available only if the file has coordinate columns on a regular grid. Accepted: .csv, .rds",

  build_no_spatial_cols = HTML("No coordinate columns were found. The app
looks for x, lon, longitude, or easting paired with y, lat, latitude, or
northing. You can still build and inspect the ellipsoid in environmental
space, but geographic maps will not be available."),

  build_irregular_grid = HTML("Coordinate columns were found but do not form a
regular grid, so they could not be converted to a raster. Work will continue
in environmental space only."),

  build_prev_session = HTML("Upload a session file (.rds) saved from a previous
visit. The app will resume at whatever step you last left off, including
any confirmed variables and built ellipsoid if they were saved."),

  build_virtual_mode = HTML("Work entirely in environmental space (E-space),
with no geographic coordinates. You will name your variables and define their
ranges directly, without uploading any spatial data."),

  build_example_data = HTML("Use the example dataset bundled with nicheR:
WorldClim bioclimatic variables for Central America, including a bias layer
for later steps. Click Continue to select which variables to use."),


  # BUILD TAB: VARIABLES ---------------------------------------------------

  build_variable_settings = HTML(paste0("Select up to ", MAX_DIMS,
                                        " variables to use in your analysis.<br>
Uncheck a variable to exclude it. The plot beside you shows the background
distribution of your selected variables and updates as you choose.")),

  build_virtual_variables = HTML(paste0("Choose how many variables to work
with (up to ", MAX_DIMS, ") and give each one a name. Default names (var1,
var2, ...) are used if left blank.")),

  build_edit_variables_tooltip = "Editing your variables will delete the
current selection, the ellipsoid, and any covariance adjustments.",


  # BUILD TAB: RANGES ------------------------------------------------------

  build_range_choice = HTML("Ranges set the minimum and maximum value of each
variable, which together define the extent of the ellipsoid. Choose one of
the three methods below. You can switch methods and change values at any
time, then rebuild."),

  build_range_manual = HTML("Define the minimum and maximum values for each
variable.<br>
Defaults are the first and third quartiles of your background data. Lines in
the plot update as you change these values. Use Reset to Defaults to restore
them, then Initialize Ellipsoid when ready."),

  build_range_data = HTML("Upload a CSV with occurrence or other environmental
data for your species of interest.<br>
Column names must match your selected variables exactly, or the upload will
not be recognized. Once matched, observed min/max values appear below, and
Expand Min/Max (%) let you widen the range outward in either direction. Lines
in the plot update as you adjust them."),

  build_range_stats = HTML("Provide summary statistics for the chosen
variables.<br>
Defaults shown here are derived from your background data, but can be changed
to known values for your species of interest. Expand Min/Max (%) widen the
resulting range outward, not the mean itself. Confidence level controls how
wide the range is before any expansion."),

  build_range_manual_tooltip = "Type minimum and maximum values directly for
each variable.",

  build_range_data_tooltip = "Upload a CSV to derive observed min/max ranges,
with optional expansion.",

  build_range_stats_tooltip = "Derive ranges from mean and standard deviation,
either from your background data or entered manually.",

  build_cl_range_tooltip = "Confidence level used with the chi-square distribution
when building the ellipsoid. Higher values produce a larger ellipsoid.",

  build_expand_range_tooltip = "Percentage to widen the range outward from the
observed or computed bounds. Can be negative to shrink the range inward.",

  build_range_file_unreadable = "Could not read the uploaded file. Check that
it is a valid .csv or .rds.",

  build_range_file_mismatch = "The uploaded file does not contain all of your
selected variables. Column names must match exactly.",

  build_range_invalid = "Please check your range inputs before building. Every
variable needs a valid minimum below its maximum.",


  # BUILD TAB: COVARIANCE --------------------------------------------------

  build_covariance = HTML("Covariances describe the relationship between pairs
of variables. With 3 variables you get 3 pairs, with 4 variables you get 6,
and so on.<br>
Drag a slider to rotate the ellipsoid along that pair; the plot updates to
show the result in 2D or pairs view. Moving one slider updates the valid
range of the others. If a combination would make the ellipsoid invalid, an
error is shown and that slider resets. Click the rotate-left icon next to a
slider to reset that pair back to zero. Click Set Covariances when ready."),

  build_covariance_reset_tooltip = "Reset this pair's covariance back to zero.",

  build_cov_set = "Covariances have been set for this ellipsoid.",


  # BUILD TAB: CENTROID ----------------------------------------------------

  build_centroid_mover = HTML("Move the ellipsoid through environmental space
without changing its shape or size. Each slider shifts the centroid along one
variable, and the ellipsoid follows. Use Reset all to return to the centroid
this version started from."),

  build_centroid_set = "Centroid has been set for this ellipsoid.",


  # BUILD TAB: LIBRARY -----------------------------------------------------

  build_library = HTML("Every ellipsoid you save is listed here. The
highlighted row at the top is the working ellipsoid, the one used by the
plots and by every tab downstream. Each saved version can be viewed
read-only, edited in place, copied into a new version, or deleted."),

  build_library_empty = "No saved versions yet. Save the working ellipsoid to
add it here.",

  build_view_only = "You are viewing a saved version. Editing is locked. Use
Return to editing, or the copy button in the library, to make changes.",

  build_save_ell_modal = "Give this ellipsoid a name. Use letters, numbers,
and spaces only. Spaces will be replaced with underscores.",

  build_delete_ell = "This will permanently remove the ellipsoid and any
prediction results associated with it.",

  build_reference = HTML("The ellipsoid summary reports volume and centroid
changes relative to a reference. By default this is whatever the working
ellipsoid was built or copied from. Point it at any saved version to compare
against that instead. Changing it only affects the summary, it does not move
your ellipsoid."),


  # PREDICT TAB ------------------------------------------------------------

  predict_adjust_trunc_tooltip = "Adjust the level of truncation within the
current ellipsoid. This truncates the prediction inwards.",

  predict_library = HTML("Ellipsoids saved on the Build tab appear here.
Select one to view its settings read-only, or delete it along with any
predictions made from it. To edit an ellipsoid, go back to the Build tab."),

  predict_library_empty = "No saved ellipsoids yet. Save one on the Build tab
to predict with it.",

  predict_delete_ell = "This will permanently remove the ellipsoid, its
predictions, and any biased predictions derived from them.",

  predict_ellipsoid_select_tooltip = "Choose which saved ellipsoid to predict
with, or All versions to predict with every one at once.",

  # BIAS TAB ---------------------------------------------------------------

  bias = HTML("Bias input adds controlled sampling bias to a prediction
layer. Note that once bias has been applied, the prediction is no longer a
probability.<br>
In the steps below you will need one or more raster layers (.rds, .tif).
These are standardized to a 0-1 scale and can have a direct effect (increase
sampling probability) or an inverse effect (decrease sampling probability)."),

  bias_input = HTML("Upload one or more raster layers (.rds, .tif) to
represent sampling bias across the study area, or use the example layers
provided."),


  # GENERATE TAB -----------------------------------------------------------


  # SHARED: PLOT SETTINGS --------------------------------------------------

  plot_settings = HTML("Adjust point shape, size, and colors, toggle range
lines, the ellipsoid, and centroid on or off, and zoom in manually or to
the ellipsoid's extent."),

  plot_zoom_tooltip = "Auto fits the plot to your data and ellipsoid.
Zoom to ellipsoid frames the plot tightly around the ellipsoid boundary.
Manual lets you set exact axis limits.",


  # SHARED: SESSION --------------------------------------------------------

  session_save = HTML("Save your current progress as an .rds file. This
includes confirmed variables and the built ellipsoid, if any. Load it later
from the Previous Session input option to resume where you left off.<br>
If no ellipsoid has been confirmed yet, only the most recent in-progress
values are saved.")

)
