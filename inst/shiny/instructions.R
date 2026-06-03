instructions <- list(
  data_upload = HTML("Upload a raster layer (.tif, .rds) and/or a CSV (can also be .rd).<br>
                             If both provided they will be validated for matching layers and cell counts."),
  variable_settings = HTML(paste0("Select up to ", MAX_DIMS,
                                  " variables to use in your analysis.<br>
                                 Uncheck a variable to exclude it.")),
  range_manual = HTML("Define the minimum and maximum values for each variable.<br>
                             Values can be negative and up to two decimal places."),
  range_data = HTML("Upload a CSV with precomputed ranges.<br>
                             Must contain columns named min and max with variable names as row names."),
  range_stats = HTML("Compute ranges automatically from the data using summary statistics.<br>
                             Select the method and confidence level below.")
)
