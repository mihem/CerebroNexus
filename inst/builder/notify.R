## Builder feedback in place of Shiny's corner notifications.
##
## Every message goes to the place it is about; builder.js renders it:
##   target = NULL                  one notice bar under the header/stepper
##   target = "status"              a brief flash beside the project status
##   target = "build" / "review"    the Build status card / the Review stage
##   target = list(field = ids)     under the first of these inputs that is on
##                                  the page (red outline for errors)
##   target = list(selector = css)  inside the first matching section
##   target = list(dataset = id)    that dataset's rail row and Configure header
## A contextual target that is not on the page falls back to the notice bar.
## info/success hide by themselves; warnings and errors stay until dismissed
## (or until builder_notify_clear() removes them by id).

builder_notify_types <- c("info", "success", "warning", "error")

builder_notify_html <- function(message) {
  if (inherits(message, c("shiny.tag", "shiny.tag.list", "html"))) {
    return(htmltools::renderTags(message)$html)
  }
  message <- as.character(unlist(message, use.names = FALSE))
  as.character(htmltools::htmlEscape(paste(message, collapse = " ")))
}

builder_notify <- function(
  type = c("info", "success", "warning", "error"),
  message,
  target = NULL,
  id = NULL,
  duration = NULL,
  action = NULL,
  session = shiny::getDefaultReactiveDomain()
) {
  type <- match.arg(type)
  if (is.null(session)) {
    return(invisible(FALSE))
  }
  hide_after <- if (!is.null(duration)) {
    duration <- suppressWarnings(as.numeric(duration))
    if (length(duration) == 1L && is.finite(duration)) duration else NULL
  } else if (type %in% c("info", "success")) {
    if (identical(type, "success")) 4 else 6
  } else {
    NULL
  }
  session$sendCustomMessage(
    "builder_notify",
    list(
      type = type,
      html = builder_notify_html(message),
      target = target,
      id = id,
      hide_after = hide_after,
      action = action
    )
  )
  invisible(TRUE)
}

builder_notify_clear <- function(
  id,
  session = shiny::getDefaultReactiveDomain()
) {
  if (is.null(session)) {
    return(invisible(FALSE))
  }
  session$sendCustomMessage("builder_notify", list(clear = id))
  invisible(TRUE)
}
