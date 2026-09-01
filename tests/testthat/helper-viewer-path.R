viewer_test_path <- function(...) {
  source_path <- testthat::test_path("..", "..", "inst", "viewer", ...)
  if (file.exists(source_path)) {
    return(source_path)
  }

  system.file("viewer", ..., package = "CerebroNexus")
}

viewer_drag_mouse <- function(app, x1, y1, x2, y2) {
  mouse <- app$get_chromote_session()$Input$dispatchMouseEvent
  mouse(
    type = "mousePressed",
    x = x1,
    y = y1,
    button = "left",
    buttons = 1,
    clickCount = 1
  )
  mouse(
    type = "mouseMoved",
    x = x2,
    y = y2,
    button = "left",
    buttons = 1
  )
  mouse(
    type = "mouseReleased",
    x = x2,
    y = y2,
    button = "left",
    buttons = 0,
    clickCount = 1
  )
}
