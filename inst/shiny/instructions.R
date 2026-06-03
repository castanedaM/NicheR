instructions <- list(
  data_upload = HTML("This step ask you to upload your BACKGROUND information, i.e., the enviromaetal rastr layers that compose your entire study area. If you have occurece data, hold on to them after this steps, of uploading and selecting the varibales of interest. Upload a raster layer (.tif, .rds) and/or a CSV (can also be .rd).<br>
                             If both provided they will be validated for matching layers and cell counts."),
  variable_settings = HTML(paste0("Select up to ", MAX_DIMS,
                                  " variables to use in your analysis.<br>
                                 Uncheck a variable to exclude it.")),
  range_choice = HTML("Select the method of preference for building elliposid, choose from manual (max min), from data (using df), from stats (mean and sd)"),
  range_manual = HTML("Define the minimum and maximum values for each variable.<br>
                             Values can be negative and up to two decimal places."),
  range_data = HTML("Upload a CSV with precomputed ranges.<br>
                             Must contain columns named min and max with variable names as row names."),
  range_stats = HTML("Compute ranges automatically from the data using summary statistics.<br>
                             Select the method and confidence level below.")
)
