generated_app_e2e_reset_runtime()

.generated_app_e2e_dom_text <- function(id, contains, timeout = 60000) {
  id_js <- .generated_app_e2e_js_value(id)
  contains_js <- .generated_app_e2e_js_value(as.list(as.character(contains)))
  driver <- generated_app_e2e_driver()
  driver$wait_for_js(
    paste0(
      "(function(){var node=document.getElementById(",
      id_js,
      ");if(!node)return false;var text=node.textContent||'';var expected=",
      contains_js,
      ";return expected.every(function(value){return text.indexOf(value)!==-1;});})()"
    ),
    timeout = timeout
  )
  as.character(driver$get_js(sprintf(
    "document.getElementById(%s).textContent",
    id_js
  )))
}

test_that("Linked views renders Trekker canvas and source-backed QC content", {
  fixture <- generated_app_e2e_select_dataset("trekker")
  expect_identical(
    generated_app_e2e_visible_pages(),
    fixture$expected$visible_pages
  )
  generated_app_e2e_open_linked_views(fixture)
  driver <- generated_app_e2e_driver()

  driver$wait_for_js(
    paste0(
      "(function(){var pane=Array.from(document.querySelectorAll(",
      "'.cv-pane:not(.cv-hidden)')).find(function(node){return ",
      "(node.querySelector('.cv-ptitle').textContent||'')",
      ".indexOf('Trekker')!==-1;});var canvas=pane&&pane.querySelector('canvas');",
      "return !!(canvas&&canvas.width>0&&canvas.height>0);})()"
    ),
    timeout = 60000
  )
  expect_true(any(grepl(
    "Trekker",
    generated_app_e2e_linked_titles(),
    fixed = TRUE
  )))

  stats <- .generated_app_e2e_dom_text("cv-tk-stats", c("180", "100"))
  expect_match(stats, "nuclei", ignore.case = TRUE)
  position_classes <- .generated_app_e2e_dom_text(
    "cv-tk-postbl",
    c("90", "50.00%", "Imported")
  )
  expect_match(position_classes, "salvaged", ignore.case = TRUE)
  moran <- .generated_app_e2e_dom_text(
    "cv-tk-morantbl",
    c("EPCAM", "0.7600")
  )
  expect_match(moran, "LUM", fixed = TRUE)

  expect_false(identical(
    driver$get_js(
      "getComputedStyle(document.getElementById('cv-tk-insights')).display"
    ),
    "none"
  ))
  expect_identical(
    driver$get_js(
      "document.querySelector(\"a[href='#shiny-tab-trekker']\").offsetParent !== null"
    ),
    TRUE
  )
  generated_app_e2e_expect_clean_browser()
})
