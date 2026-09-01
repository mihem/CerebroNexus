inst_candidates <- c(
  normalizePath("inst", mustWork = FALSE),
  normalizePath("../../inst", mustWork = FALSE),
  normalizePath(testthat::test_path("../../inst"), mustWork = FALSE)
)
inst_dir <- inst_candidates[file.exists(file.path(inst_candidates, "viewer"))][
  1
]
state_file <- file.path(inst_dir, "viewer", "www", "cell_views_state.js")

run_state_node <- function(body) {
  skip_if(Sys.which("node") == "", "node not on PATH")
  expect_true(file.exists(state_file), info = "cell_views_state.js not found")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      "global.window = {};",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(state_file, quote = "\"")
      ),
      body
    ),
    runner
  )
  system2("node", runner, stdout = TRUE, stderr = TRUE)
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
    "  inactive:S.trekkerGeneControls('spatial_projection','CD3D')",
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
      inactive = NULL
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
