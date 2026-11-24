##----------------------------------------------------------------------------##
## Background-image editor, shared by the pages that have one (Linked views and
## Spatial). Loaded by shiny_UI.R at the top level, so the pages and the
## server's renderUI() calls both find it.
##
## The editor is client-owned: cell_views.js reads and writes its controls and
## re-draws the image on the canvas, with no round trip to the server. Its
## element ids carry the page's `prefix` ("cv" for Linked views), so that every
## page can mount its own editor without the ids colliding.
##----------------------------------------------------------------------------##

## The container: the section picker, the image picker, the controls (`bar`,
## rendered by the server from the image's preset, or empty) and the note.
## `image_select = FALSE` hides the image picker, for a page that chooses the
## image elsewhere. The styles are scoped under `.coordviews-page`, which a page
## other than Linked views asks for with `page_scope = TRUE`.
viewerBackgroundImageEditor <- function(
  prefix,
  bar,
  image_select = TRUE,
  page_scope = FALSE
) {
  id <- function(name) paste0(prefix, "-", name)
  editor <- div(
    class = "cv-ctl cv-bg-ctl cerebro-settings-full",
    id = id("img-pick-ctl"),
    style = "display:none",
    div(class = "cv-bg-space-tabs form-group", id = id("bg-space-tabs")),
    div(
      class = "cv-bg-settings",
      div(
        class = "cv-bg-display form-group",
        style = if (!image_select) "display:none",
        tags$label(
          class = "cv-bg-display-title",
          `for` = id("bg-image-select"),
          "Image"
        ),
        tags$select(id = id("bg-image-select"), class = "form-control")
      ),
      bar,
      tags$p(
        id = id("bg-session-note"),
        class = "cv-bg-session-note",
        "Changes apply only to this session."
      )
    )
  )
  if (page_scope) div(class = "coordviews-page", editor) else editor
}

## The controls, seeded from the alignment `preset` of the image the editor
## opens on, so an external image (Visium H&E) opens pre-aligned. The Move
## sliders are in data units, ranged to the section's coordinate `span`.
## `copy_preset` adds the button that turns the alignment into code to paste
## into `Cerebro.options`.
viewerBackgroundImageBar <- function(
  prefix,
  preset,
  span,
  copy_preset = FALSE
) {
  pr <- preset
  `%||%` <- function(x, y) if (is.null(x)) y else x
  if (is.null(span) || length(span) < 2) {
    span <- c(400, 400)
  }
  id <- function(name) paste0(prefix, "-img-", name)
  ## One labelled range + number pair; `label` names it for assistive tech.
  ctl <- function(name, heading, label, mn, mx, val) {
    range_id <- id(name)
    number_id <- paste0(range_id, "-number")
    div(
      class = "cv-img-ctl",
      tags$label(`for` = number_id, heading),
      div(
        class = "cv-img-range",
        tags$input(
          type = "range",
          id = range_id,
          `aria-label` = label,
          min = mn,
          max = mx,
          value = val,
          step = "any"
        ),
        tags$input(
          type = "number",
          id = number_id,
          class = "cv-img-number",
          min = mn,
          max = mx,
          value = val,
          step = "any",
          `aria-label` = paste(label, "value")
        )
      )
    )
  }
  chk <- function(name, label, on) {
    tags$label(
      class = "cv-chk",
      tags$input(
        type = "checkbox",
        id = id(name),
        checked = if (isTRUE(on)) "checked"
      ),
      label
    )
  }
  ## The range has to CONTAIN the value it is being asked to show. A preset is a
  ## calibration someone measured; a slider ranged on the coordinate span alone
  ## clamps anything outside it, and because the whole bar is read back together
  ## the clamped number is then written into the state by an unrelated nudge --
  ## the alignment silently becoming one nobody chose.
  sx <- signif(max(span[1] * 1.2, abs(pr$offsetX %||% 0) * 1.1), 3)
  sy <- signif(max(span[2] * 1.2, abs(pr$offsetY %||% 0) * 1.1), 3)
  scale_x <- pr$scaleX %||% 1
  scale_y <- pr$scaleY %||% scale_x
  scale_lo <- min(0.3, scale_x * 0.9, scale_y * 0.9)
  scale_hi <- max(3, scale_x * 1.1, scale_y * 1.1)
  div(
    class = "cv-imgbar",
    div(
      class = "cv-imgbar-heading",
      tags$span(class = "cv-imgbar-title", "Alignment")
    ),
    ctl("opacity", "Opacity", "Opacity", 0, 1, pr$opacity %||% 1),
    ctl("offx", "Horizontal", "Move X", -sx, sx, pr$offsetX %||% 0),
    ctl("offy", "Vertical", "Move Y", -sy, sy, pr$offsetY %||% 0),
    ## Two scales, not one. A preset can carry scaleX != scaleY -- a calibration
    ## that is genuinely non-uniform -- and a single slider had to pick a number
    ## for both, so touching ANY control in this bar silently squared the image
    ## up and threw that calibration away. Locked together by default, since a
    ## uniform scale is the common case and two sliders to drag is a worse one.
    ctl("scalex", "Scale X", "Scale X", scale_lo, scale_hi, scale_x),
    ctl("scaley", "Scale Y", "Scale Y", scale_lo, scale_hi, scale_y),
    ctl("rotate", "Rotation", "Rotate", -180, 180, pr$rotation %||% 0),
    div(
      class = "cv-img-checks",
      chk("show", "Show this image", TRUE),
      chk(
        "lock",
        "Lock aspect ratio",
        isTRUE(is.null(pr$scaleY) || identical(pr$scaleY, pr$scaleX))
      ),
      chk("flipx", "Flip horizontally", isTRUE(pr$flipX)),
      chk("flipy", "Flip vertically", isTRUE(pr$flipY))
    ),
    ## Alignment is fiddly and easy to lose; the preset is the state the data set
    ## shipped with, so there has to be a way back to it that is not "reload".
    tags$button(
      type = "button",
      id = id("reset"),
      class = "cv-imgbar-reset",
      "Reset to preset"
    ),
    if (copy_preset) {
      tagList(
        tags$button(
          type = "button",
          id = id("copy"),
          class = "cv-imgbar-reset",
          "Copy alignment as preset"
        ),
        tags$pre(id = id("code"), class = "cv-img-code", hidden = NA)
      )
    }
  )
}
