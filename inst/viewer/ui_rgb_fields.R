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
