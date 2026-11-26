## The point size field shared by Spatial's and Linked views' settings.

## A point size slider with a small "Auto" switch at the right of its label:
## on, each spatial section is drawn with the size that suits how densely its
## cells sit and how large its panel is, and the slider is off (cell_views.js
## reads the switch; the server does not). One cell of the settings grid.
viewerPointSizeWithAuto <- function(slider, auto_id) {
  div(
    class = "cv-point-size",
    slider,
    tags$label(
      class = "cv-point-size-auto",
      title = paste(
        "On: each section gets the size that suits how densely its cells",
        "sit and how large its panel is. Off: the slider sets one size."
      ),
      tags$input(type = "checkbox", id = auto_id, checked = NA),
      "Auto"
    )
  )
}
