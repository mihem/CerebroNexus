viewer_test_path <- function(...) {
  source_path <- testthat::test_path("..", "..", "inst", "viewer", ...)
  if (file.exists(source_path)) {
    return(source_path)
  }

  system.file("viewer", ..., package = "CerebroNexus")
}

viewer_app_test_path <- function() {
  source_path <- testthat::test_path("..", "..", "inst")
  if (file.exists(file.path(source_path, "app.R"))) {
    return(source_path)
  }

  system.file(package = "CerebroNexus")
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

viewer_set_selectize <- function(app, input_id, value) {
  input_json <- jsonlite::toJSON(input_id, auto_unbox = TRUE)
  values_json <- jsonlite::toJSON(as.character(value), auto_unbox = FALSE)
  app$wait_for_js(sprintf(
    "typeof document.getElementById(%s)?.selectize?.settings.load === 'function'",
    input_json
  ))
  app$run_js(sprintf(
    paste0(
      "(() => {const s=document.getElementById(%s)?.selectize;",
      "if(!s)throw new Error('Selectize input is not ready');",
      "const v=%s;v.filter(Boolean).forEach(x=>s.addOption({value:x,text:x}));",
      "const out=s.settings.maxItems===1?(v[0]||''):v;s.setValue(out);",
      "Shiny.setInputValue(%s,out,{priority:'event'});})()"
    ),
    input_json,
    values_json,
    input_json
  ))
}
