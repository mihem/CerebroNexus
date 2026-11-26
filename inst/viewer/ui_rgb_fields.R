##----------------------------------------------------------------------------##
## RGB co-expression gene field, shared by Gene expression, Spatial and Linked
## views. Loaded by shiny_UI.R at the top level, so the page and the server's
## renderUI() calls both find it.
##----------------------------------------------------------------------------##
## The three genes of RGB co-expression in one field: a dot of the channel's
## colour marks each box, and the boxes are as wide as the longest gene in
## them (custom.css), not the toolbar's minimum width.
cerebroRgbGeneFields <- function(
  ids,
  label = "Co-expression (R / G / B)",
  selected = NULL,
  options = list()
) {
  channels <- c("Red", "Green", "Blue")
  options <- utils::modifyList(
    list(
      maxOptions = 1000,
      placeholder = "gene",
      create = FALSE,
      loadThrottle = 300
    ),
    options
  )
  div(
    class = "form-group cerebro-rgb-field",
    tags$label(class = "control-label", label),
    div(
      class = "cerebro-rgb-selects",
      Map(
        function(id, channel) {
          control <- selectizeInput(
            id,
            label = NULL,
            choices = NULL,
            selected = selected,
            options = options
          )
          control <- htmltools::tagQuery(control)$find("select")$addAttrs(
            `aria-label` = paste(channel, "channel gene")
          )$allTags()
          div(
            class = paste0("cerebro-rgb-select is-", tolower(channel)),
            title = paste(channel, "channel"),
            control
          )
        },
        ids,
        channels,
        USE.NAMES = FALSE
      )
    )
  )
}

## The Columns field of a page of panels (Spatial's grid, Linked views): Auto,
## or 1 to `most` columns, at most 8. cell_views.js (gridColumnsChoice())
## reads it; NULL `label` leaves the label to the caller's markup.
## What a page of panels says in a browser without a GPU, which shows 2 panels
## at a time (cell_points_gpu.js): hidden unless the page is limited, and the
## button lifts the limit (also the "Show all panels" preference).
cerebroPanelCapNote <- function() {
  div(
    class = "cerebro-cap-note",
    role = "status",
    tags$span("No GPU in this browser: showing 2 panels at a time."),
    tags$button(
      type = "button",
      class = "btn btn-link",
      `data-cerebro-cap-unlock` = "1",
      "Show all (slow)"
    )
  )
}

cerebroColumnsInput <- function(
  id,
  most = 8L,
  selected = NULL,
  label = "Columns"
) {
  most <- max(1L, min(8L, as.integer(most)))
  selectInput(
    id,
    label = label,
    choices = c(
      "Auto" = "auto",
      stats::setNames(seq_len(most), seq_len(most))
    ),
    selected = selected %||% "auto"
  )
}
