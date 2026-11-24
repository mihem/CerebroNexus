##----------------------------------------------------------------------------##
## Copy alignment as preset.
##
## The background-image editor is client-owned (see the shared editor in
## ui_background_image_editor.R). Its button sends the alignment on screen with
## the identity of the image it belongs to, and this turns it into one
## spatial_image_settings leaf to paste into Cerebro.options. The identity names
## the dataset, the section and the image, so the code is for exactly that
## image, whichever section of a grid or ROI the user was adjusting.
##----------------------------------------------------------------------------##
observeEvent(input[["spatial_projection_background_copy"]], {
  request <- input[["spatial_projection_background_copy"]]
  req(
    is.list(request),
    is.list(request[["identity"]]),
    is.list(request[["state"]])
  )
  identity <- request[["identity"]]
  state <- request[["state"]]
  ## A request made before the data set changed describes another data set.
  req(identical(identity[["dataset"]], viewer_current_dataset_key()))
  number <- function(name, default) {
    value <- state[[name]]
    if (length(value) == 1L && is.numeric(value) && is.finite(value)) {
      as.numeric(value)
    } else {
      default
    }
  }
  code <- format_spatial_preset_code(
    dataset = identity[["dataset"]],
    spatial_name = identity[["spatial_name"]],
    image_label = identity[["key"]] %||% identity[["label"]],
    offset_x = number("offsetX", 0),
    offset_y = number("offsetY", 0),
    scale_x = number("scaleX", 1),
    scale_y = number("scaleY", 1),
    flip_x = isTRUE(state[["flipX"]]),
    flip_y = isTRUE(state[["flipY"]]),
    rotation = number("rotate", 0),
    image_opacity = number("opacity", 1)
  )
  req(!is.null(code))
  session$sendCustomMessage(
    "cerebro_background_preset_code",
    list(prefix = "sp", text = code, identity = identity)
  )
})
