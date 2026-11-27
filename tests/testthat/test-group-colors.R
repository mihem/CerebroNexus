test_that("a data set's Group colours are checked by one rule", {
  valid <- CerebroNexus:::.cerebroValidGroupColors
  expect_identical(
    valid(c(A = "#111111", B = "#abcdef")),
    c(A = "#111111", B = "#abcdef")
  )
  ## unusable as a whole
  expect_null(valid(c("#111111", "#222222")))
  expect_null(valid(c(A = "red", B = "#222222")))
  expect_null(valid(c(A = "#111111", B = NA)))
  expect_null(valid(character()))
  expect_null(valid(list(A = "#111111")))
  ## a repeated level keeps its first colour, a nameless entry is dropped
  expect_identical(
    valid(c(A = "#111111", B = "#222222", A = "#444444")),
    c(A = "#111111", B = "#222222")
  )
  expect_identical(
    valid(stats::setNames(c("#111111", "#222222"), c("A", ""))),
    c(A = "#111111")
  )
})

test_that("the exporter keeps the colours the Builder offers", {
  object <- SeuratObject::pbmc_small
  object@misc$group_colors <- list(
    letter.idents = c(A = "#111111", B = "#222222", A = "#444444")
  )
  path <- tempfile(fileext = ".crb")
  on.exit(unlink(path), add = TRUE)
  suppressMessages(suppressWarnings(
    exportFromSeurat(
      object = object,
      assay = "RNA",
      slot = "counts",
      file = path,
      experiment_name = "colours",
      organism = "hg",
      groups = "letter.idents",
      nUMI = "nCount_RNA",
      nGene = "nFeature_RNA",
      verbose = FALSE
    )
  ))
  expect_identical(
    readCerebro(path)$getTechnicalInfo()$group_colors$letter.idents,
    c(A = "#111111", B = "#222222")
  )
})

test_that("a palette past a couple of thousand levels cycles", {
  base <- c("#111111", "#333333")
  extend <- CerebroNexus:::.cerebroExtendPalette
  max_distinct <- CerebroNexus:::.cerebro_palette_max_distinct
  colors <- extend(base, max_distinct + 5L)
  expect_length(colors, max_distinct + 5L)
  expect_false(anyDuplicated(colors[seq_len(max_distinct)]) > 0L)
  expect_identical(colors[max_distinct + 1:5], colors[1:5])
})
