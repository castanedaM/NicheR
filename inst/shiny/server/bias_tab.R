output$bias_raster_print <- renderPrint({
  req(input$bias_raster_file)
  ext <- tolower(tools::file_ext(input$bias_raster_file$name))
  result <- tryCatch(
    load_raster_file(input$bias_raster_file$datapath, ext),
    error = function(e) stop(safeError(e))
  )

  removeNotification("bias_raster_preview_msg")
  print(result)
})
