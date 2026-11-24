##----------------------------------------------------------------------------##
## UI elements to set additional parameters for the projection.
##----------------------------------------------------------------------------##
## Scatter controls deliberately live in a renderUI which is independent of the
## selected background image. This preserves users' point-size, opacity and
## sampling choices while moving among backgrounds.
output[["spatial_projection_scatter_parameters_UI"]] <- renderUI({
  appearance <- current_spatial_scatter_defaults()
  section_appearance <- spatialPointAppearance(
    Cerebro.options,
    viewer_current_dataset_key(),
    spatial_projection_single_section(input, availableSpatial())
  )
  if (!is.null(section_appearance)) {
    appearance$point_size <- section_appearance$point_size
    appearance$point_opacity <- section_appearance$point_opacity
  }
  roi_appearance <- spatialRoiSetting(
    Cerebro.options,
    viewer_current_dataset_key(),
    spatial_projection_single_section(input, availableSpatial()),
    spatial_roi_value(input[["spatial_projection_roi"]] %||% "")
  )
  if (!is.null(roi_appearance)) {
    appearance$point_size <- roi_appearance$point_size
    appearance$point_opacity <- roi_appearance$point_opacity
  }

  tagList(
    viewerLimitedSlider(
      "spatial_projection_point_size",
      label = "Point size",
      limits = preferences[["cell_point_size"]],
      value = appearance$point_size
    ),
    viewerLimitedSlider(
      "spatial_projection_point_opacity",
      label = "Point opacity",
      limits = preferences[["cell_point_opacity"]],
      min = 0,
      value = appearance$point_opacity
    )
  )
})

output[["spatial_projection_data_parameters_UI"]] <- renderUI({
  appearance <- current_spatial_scatter_defaults()

  tagList(
    viewerLimitedSlider(
      "spatial_projection_percentage_cells_to_show",
      label = "Show % of cells",
      limits = preferences[["cell_percentage_cells_to_show"]],
      value = appearance$percentage_cells_to_show
    )
  )
})

## Keep controls available while the settings drawer is hidden.
outputOptions(
  output,
  "spatial_projection_scatter_parameters_UI",
  suspendWhenHidden = FALSE
)
outputOptions(
  output,
  "spatial_projection_data_parameters_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
cerebroRegisterInfo(
  input,
  "spatial_projection_additional_parameters_info",
  spatial_projection_additional_parameters_info
)

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
spatial_projection_additional_parameters_info <- list(
  title = "Additional parameters for projection",
  text = HTML(
    "
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
      <li><b>Point size:</b> Controls how large the observations should be.</li>
      <li><b>Point opacity:</b> Controls the transparency of the observations.</li>
      <li><b>Show % of cells:</b> Randomly subsamples cells or spots after the active Sample, FOV / section, ROI, and group filters.</li>
    </ul>
    "
  )
)
