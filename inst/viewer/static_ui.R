## Render immutable UI branches once per app process. Keep singleton tags and
## deferred tag functions in the tree so Shiny still owns their lifecycle.
viewerStaticUI <- function(ui) {
  protected <- function(node) {
    if (is.function(node) || isTRUE(attr(node, "htmltools.singleton"))) {
      return(TRUE)
    }
    children <- if (inherits(node, "shiny.tag")) node$children else node
    is.list(children) && any(vapply(children, protected, logical(1)))
  }
  compact <- function(node) {
    if (is.function(node) || isTRUE(attr(node, "htmltools.singleton"))) {
      return(node)
    }
    if (!protected(node)) {
      rendered <- htmltools::renderTags(node)
      return(htmltools::attachDependencies(
        htmltools::tagList(
          if (nzchar(rendered$head)) {
            htmltools::tags$head(htmltools::HTML(rendered$head))
          },
          htmltools::HTML(rendered$html)
        ),
        rendered$dependencies
      ))
    }
    if (inherits(node, "shiny.tag")) {
      node$children <- lapply(node$children, compact)
    } else if (is.list(node)) {
      node[] <- lapply(node, compact)
    }
    node
  }
  ## Preserve the document/body tag and its attributes (including language).
  if (inherits(ui, "shiny.tag")) {
    ui$children <- lapply(ui$children, compact)
    ui
  } else {
    compact(ui)
  }
}
