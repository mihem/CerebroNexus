state_file <- viewer_test_path("www", "cell_views_state.js")

run_state_node <- function(body) {
  skip_if_no_node()
  expect_true(file.exists(state_file), info = "cell_views_state.js not found")
  run_node_script(c(
      "const fs = require('fs');",
      "global.window = {};",
      js_eval_file(state_file),
      body
  ))
}

test_that("specialist state transitions use semantic identities", {
  output <- run_state_node(paste0(
    "const S = window.CBViewState;",
    "const spaces = {",
    "  a:{id:'single::trekker_projection::trekker',_role:'trekker'},",
    "  b:{id:'single::trekker_projection::umap',_role:'umap'}",
    "};",
    "const saved = [",
    "  {spaceId:'spatial',view:{cx:1}},",
    "  {spaceId:'umap',view:{cx:2}}",
    "];",
    "console.log(JSON.stringify({",
    "  role:S.spaceByRole(spaces,'trekker').id,",
    "  lens:S.lensForSpace(saved,'umap',0).view.cx,",
    "  missing:S.lensForSpace(saved,'missing',0),",
    "  controls:S.trekkerGeneControls('trekker_projection','CD3D'),",
    "  inactive:S.trekkerGeneControls('spatial_projection','CD3D'),",
    "  panel:S.genePanelSpaceId(1,'projection::umap')",
    "}));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(
      role = "single::trekker_projection::trekker",
      lens = 2L,
      missing = NULL,
      controls = list(trekker_mode = "gene", trekker_gene_pick = "CD3D"),
      inactive = NULL,
      panel = "__linked_gene_1::projection::umap"
    )
  )
})

test_that("expression clear transitions remove every dependent payload", {
  output <- run_state_node(paste0(
    "const S = window.CBViewState;",
    "const state = {gene:{v:[1]},genePanels:[1],rgb:{r:[1]}};",
    "S.clearExpression(state,'gene');",
    "S.clearExpression(state,'panels');",
    "S.clearExpression(state,'rgb');",
    "console.log(JSON.stringify(state));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(gene = NULL, genePanels = NULL, rgb = NULL)
  )
})

test_that("shared projections are selected by name without collapsing caches", {
  output <- run_state_node(paste0(
    "const S = window.CBViewState;",
    "const shared = {projections:{",
    "  umap:{x:new Float32Array([1,2]),y:new Float32Array([3,4])},",
    "  tsne:{x:new Float32Array([5,6]),y:new Float32Array([7,8])}",
    "}};",
    "const available = typeof S.sharedProjection === 'function';",
    "const umap = available ? S.sharedProjection(shared,'umap',2) : null;",
    "const tsne = available ? S.sharedProjection(shared,'tsne',2) : null;",
    "const wrong = available ? S.sharedProjection(shared,'umap',3) : null;",
    "console.log(JSON.stringify({",
    "  available,",
    "  umap:umap ? Array.from(umap.x) : null,",
    "  tsne:tsne ? Array.from(tsne.y) : null,",
    "  wrong:wrong",
    "}));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(
      available = TRUE,
      umap = list(1L, 2L),
      tsne = list(7L, 8L),
      wrong = NULL
    )
  )
})

test_that("canonical grouped coordinates accept reused typed arrays", {
  output <- run_state_node(paste0(
    "const S = window.CBViewState;",
    "const groups = new Uint8Array([0,1,0]);",
    "const reused = new Float32Array([10,20,30]);",
    "const direct = S.canonicalGroupedValues(reused,groups,NaN);",
    "const nested = S.canonicalGroupedValues([[10,30],[20]],groups,NaN);",
    "console.log(JSON.stringify({",
    "  same:direct===reused,",
    "  direct:Array.from(direct),",
    "  nested:Array.from(nested)",
    "}));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(
      same = TRUE,
      direct = list(10L, 20L, 30L),
      nested = list(10L, 20L, 30L)
    )
  )
})

test_that("canonical auxiliary values preserve flat barcode order", {
  output <- run_state_node(paste0(
    "const S = window.CBViewState;",
    "const groups = new Uint8Array([0,1,0]);",
    "const flat = ['cell-a','cell-b','cell-c'];",
    "const direct = S.canonicalAuxValues(flat,groups,'');",
    "const nested = S.canonicalAuxValues(",
    "  [['cell-a','cell-c'],['cell-b']],groups,'');",
    "console.log(JSON.stringify({",
    "  same:direct===flat,",
    "  direct:Array.from(direct),",
    "  nested:Array.from(nested)",
    "}));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(
      same = TRUE,
      direct = list("cell-a", "cell-b", "cell-c"),
      nested = list("cell-a", "cell-b", "cell-c")
    )
  )
})

test_that("specialist restore does not overwrite a saved state on its active page", {
  output <- run_state_node(paste0(
    "const S = window.CBViewState;",
    "console.log(JSON.stringify({",
    "  current:S.shouldStashSingleState('overview_projection',",
    "    'overview_projection',true),",
    "  navigating:S.shouldStashSingleState('spatial_projection',",
    "    'overview_projection',true),",
    "  normal:S.shouldStashSingleState('overview_projection',",
    "    'overview_projection',false)",
    "}));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(current = FALSE, navigating = TRUE, normal = TRUE)
  )
})

test_that("specialist selected-cell results wait for stable identities", {
  output <- run_state_node(paste0(
    "const S = window.CBViewState;",
    "const pendingA = S.specialistSelectionReport(new Set([0,2]), [], 3);",
    "const readyA = S.specialistSelectionReport(new Set([0,2]),",
    "  ['cell-a','cell-b','cell-c'],3);",
    "const cleared = S.specialistSelectionReport(null,",
    "  ['cell-a','cell-b','cell-c'],3);",
    "const latest = S.specialistSelectionReport(new Set([1]),",
    "  ['cell-a','cell-b','cell-c'],3);",
    "const readyNow = S.specialistSelectionReport(new Set([2]),",
    "  ['cell-a','cell-b','cell-c'],3);",
    "console.log(JSON.stringify({pendingA,readyA,cleared,latest,readyNow}));"
  ))

  expect_equal(attr(output, "status"), NULL)
  state <- jsonlite::fromJSON(output, simplifyVector = FALSE)

  ## A: local selection/count exist immediately, while results remain gated.
  expect_true(state$pendingA$hasSelection)
  expect_identical(state$pendingA$selectedCells, 2L)
  expect_true(state$pendingA$pending)
  expect_null(state$pendingA$ids)

  ## B: aux readiness publishes the same current selection with stable IDs.
  expect_false(state$readyA$pending)
  expect_identical(unlist(state$readyA$ids), c("cell-a", "cell-c"))

  ## C: clearing before aux stays cleared when identities become available.
  expect_false(state$cleared$hasSelection)
  expect_identical(state$cleared$selectedCells, 0L)
  expect_null(state$cleared$ids)

  ## D: recomputing at aux time uses only the latest live selection.
  expect_identical(unlist(state$latest$ids), "cell-b")

  ## E: an already-ready specialist selection publishes immediately.
  expect_true(state$readyNow$stableKeysReady)
  expect_false(state$readyNow$pending)
  expect_identical(unlist(state$readyNow$ids), "cell-c")
})
