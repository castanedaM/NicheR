instructions <- list(
  data_upload = HTML("Background data: your study area's environmental conditions.
Upload a raster (.tif, .rds) and/or a CSV of the same data. If both are provided they will be validated for consistency. Occurrence records are used later."),
  variable_settings = HTML(paste0("Select up to ", MAX_DIMS,
                                  " variables to use in your analysis.<br>
                                 Uncheck a variable to exclude it.")),
  range_choice = HTML("Select the method of preference for building elliposid, choose from manual (max min), from data (using df), from stats (mean and sd)"),
  range_manual = HTML("Define the minimum and maximum values for each variable.<br>
                             Values can be negative and up to two decimal places."),
  range_data = HTML("Upload a CSV with occurences or other info for ranges.<br>
                             Must contain matching name columns as the background data"),
  range_stats = HTML("Provide summary statistics for the choosen variables. Defaiults shown here represent the mean and standard deaviation of all backgound points.<br>
                             Select the method and confidence level below.")
)
