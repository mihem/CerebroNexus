## The CosMx and T-cell demos (data-raw/large-demo), on small synthetic
## stand-ins for the published files, and their entries in the Viewer menu.

large_demo_env <- function(script) {
  scripts <- testthat::test_path(
    "..",
    "..",
    "data-raw",
    "large-demo",
    c("download_helpers.R", script)
  )
  testthat::skip_if_not(
    all(file.exists(scripts)),
    "benchmark tree not present (expected when checking a built package)"
  )
  env <- new.env(parent = globalenv())
  for (path in scripts) {
    sys.source(path, envir = env)
  }
  env
}

## Cell table in the columns of the atlas's panC.freq.all.meta.tb.rds.
tcell_meta <- function() {
  data.frame(
    cellID = c("a1", "a2", "b1", "b2", "b3", "b4"),
    cellID.uniq = paste0("ds1.", c("a1", "a2", "b1", "b2", "b3", "b4")),
    patient.uid = c("P1", "P1", "P2", "P2", "P2", "P2"),
    cancerType = c("LC", "LC", "BRCA", "BRCA", "BRCA", "BRCA"),
    loc = c("T", "N", "T", "T", "P", "T"),
    dataset = "ds1",
    dataset.old = "ds1.old",
    stype = c("CD8", "CD4", "CD8", "CD8", "CD8", "CD4"),
    meta.cluster = c(
      "CD8.c01.Tn.MAL",
      "CD4.c01.Tn.TCF7",
      "CD8.c12.Tex.CXCL13",
      "CD8.c06.Tem.GZMK",
      "CD8.c03.Tm.RPS12",
      "CD4.c01.Tn.TCF7"
    ),
    meta.cluster.coarse = "x",
    miniCluster = c("ds1.C1", "ds1.C1", "ds1.C2", "ds1.C3", "ds1.C4", "ds1.C2"),
    tech = "10X",
    dataSource = "zhangLab",
    TCR = c("Prod", NA, "Prod", "None", NA, NA),
    Phase = "G1",
    S.Score = 0.1,
    G2M.Score = 0.2,
    percent.mito = 0.03,
    stringsAsFactors = FALSE
  )
}

tcell_cells <- function(
  env,
  meta = tcell_meta(),
  expected_cells = 6L,
  patients = c("P1", "P2")
) {
  clinical <- data.frame(
    patient = c("P1", "P2"),
    cancer_name = c("Lung", "Breast"),
    age = c("60", "50"),
    gender = c("Male", "Female"),
    clinical_stage = c("II", "III"),
    stringsAsFactors = FALSE
  )[c("P1", "P2") %in% patients, ]
  env$.viewerTcellReadCells(
    meta,
    clinical,
    expected_cells = expected_cells,
    expected_patients = 2L
  )
}

## Flat files of one section as NanoString writes them: cell_ID 0 rows hold the
## transcripts outside any cell, and the negative probes sit among the genes.
cosmx_flat_files <- function(
  sample,
  dir = withr::local_tempdir(.local_envir = parent.frame())
) {
  metadata <- data.frame(
    fov = c(1, 1, 2),
    cell_ID = c(1, 2, 1),
    Area = c(100, 200, 300),
    AspectRatio = c(1, 1.5, 2),
    CenterX_local_px = 1,
    CenterY_local_px = 1,
    CenterX_global_px = c(10, 20, 30),
    CenterY_global_px = c(100, 200, 300),
    Width = c(10, 20, 30),
    Height = c(11, 21, 31),
    Mean.MembraneStain = 1,
    Max.MembraneStain = 2,
    Mean.PanCK = 3,
    Max.PanCK = 4,
    Mean.CD45 = 5,
    Max.CD45 = 6,
    Mean.CD3 = 7,
    Max.CD3 = 8,
    Mean.DAPI = 9,
    Max.DAPI = 10
  )
  expression <- data.frame(
    fov = c(1, 1, 1, 2, 2),
    cell_ID = c(0, 1, 2, 0, 1),
    GENEA = c(9, 1, 0, 9, 4),
    GENEB = c(9, 0, 2, 9, 0),
    GENEC = c(9, 3, 0, 9, 0),
    NegPrb1 = c(9, 0, 1, 9, 1),
    NegPrb2 = c(9, 1, 1, 9, 0)
  )
  ## Rows in a different order than the metadata, as a check on the join.
  expression <- expression[c(5, 3, 2, 1, 4), ]
  files <- c(
    expression = file.path(dir, paste0(sample, "_exprMat_file.csv")),
    metadata = file.path(dir, paste0(sample, "_metadata_file.csv"))
  )
  utils::write.csv(expression, files[["expression"]], row.names = FALSE)
  utils::write.csv(metadata, files[["metadata"]], row.names = FALSE)
  files
}

test_that("the CosMx and T-cell demos and their compressed twins join the menu", {
  env <- new.env(parent = baseenv())
  sys.source(viewer_test_path("million_cell_demo.R"), envir = env)
  original <- list(
    crb_file_to_load = c(Small = "small.crb"),
    point_size = c(Small = 5),
    point_opacity = c(Small = 1),
    percentage_cells_to_show = 100
  )
  root <- withr::local_tempdir()
  demos <- list(
    cosmx = list(
      add = env$viewerAddCosMxDemo,
      label = "CosMx SMI NSCLC (8 sections)",
      variable = "CEREBRO_COSMX_DEMO_CRB"
    ),
    tcell = list(
      add = env$viewerAddTcellDemo,
      label = "Zheng et al. pan-cancer T cells (398K + TCR)",
      variable = "CEREBRO_TCELL_DEMO_CRB"
    )
  )
  for (stem in names(demos)) {
    demo <- demos[[stem]]
    expect_identical(demo$add(original, ""), original)
    crb <- file.path(root, paste0(stem, ".crb"))
    file.create(crb, file.path(root, paste0(stem, ".h5")))
    configured <- demo$add(original, crb)
    expect_identical(
      unname(configured$crb_file_to_load[demo$label]),
      normalizePath(crb)
    )
    unlink(file.path(root, paste0(stem, ".h5")))
    expect_error(
      demo$add(original, crb),
      paste(demo$variable, "must name a .crb with an adjacent .h5 file."),
      fixed = TRUE
    )
  }

  ## The large demos can be registered together; a compressed twin is offered
  ## once both its files exist.
  cosmx <- file.path(root, "cosmx.crb")
  file.create(file.path(
    root,
    c("cosmx.h5", "tcell.h5", "cosmx_compressed.crb")
  ))
  label <- demos$cosmx$label
  twin <- paste(label, "[compressed]")
  half <- env$viewerAddCosMxDemo(original, cosmx)
  expect_identical(names(half$crb_file_to_load), c("Small", label))
  file.create(file.path(root, "cosmx_compressed.h5"))
  both <- env$viewerAddTcellDemo(
    env$viewerAddCosMxDemo(original, cosmx),
    file.path(root, "tcell.crb")
  )
  expect_identical(
    names(both$crb_file_to_load),
    c("Small", label, twin, demos$tcell$label)
  )
  expect_identical(
    unname(both$crb_file_to_load[twin]),
    normalizePath(file.path(root, "cosmx_compressed.crb"))
  )
})

test_that("a preparation keeps the previous data set until a new one is complete", {
  for (demo in c("CosMx", "Tcell")) {
    env <- large_demo_env(sprintf("prepare_viewer_%s_data.R", tolower(demo)))
    prepare <- env[[sprintf("prepareViewer%sDemoData", demo)]]
    version <- env[[sprintf(".viewer%sVersion", demo)]]
    build <- sprintf(".viewer%sBuildObject", demo)
    root <- withr::local_tempdir()
    paths <- env[[sprintf(".viewer%sPaths", demo)]](root)
    env$.demoRequire <- function(...) NULL
    env[[sprintf(".viewer%sFiles", demo)]] <- function(...) list()
    dir.create(dirname(paths$crb), recursive = TRUE)
    writeLines("previous", paths$crb)

    env[[build]] <- function(paths, work) {
      writeLines("partial", paths$crb)
      stop("simulated preparation failure")
    }
    expect_error(prepare(root), "simulated preparation failure", info = demo)
    expect_identical(readLines(paths$crb), "previous", info = demo)
    expect_length(list.files(paths$root, pattern = "stage-"), 0L)

    env[[build]] <- function(paths, work) {
      writeLines("new", paths$crb)
      file.create(paths$sidecar, if (!is.null(paths$immune)) paths$immune)
      writeLines(as.character(version), paths$stamp)
    }
    expect_identical(prepare(root), paths$crb, info = demo)
    expect_identical(readLines(paths$crb), "new", info = demo)
    ## A finished build of the current version is reused; another stamp is not.
    env[[build]] <- function(...) stop("must reuse the cache")
    expect_identical(prepare(root), paths$crb, info = demo)
    writeLines("0", paths$stamp)
    expect_error(prepare(root), "must reuse the cache", info = demo)
  }
})

test_that("a CosMx section is read into counts, cells and mirrored coordinates", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("Matrix")
  env <- large_demo_env("prepare_viewer_cosmx_data.R")
  section <- env$.viewerCosMxReadSample(
    cosmx_flat_files("Lung5_Rep2"),
    "Lung5_Rep2"
  )

  barcodes <- c("Lung5_Rep2_1_1", "Lung5_Rep2_1_2", "Lung5_Rep2_2_1")
  genes <- c("GENEA", "GENEB", "GENEC")
  expect_s4_class(section$counts, "dgCMatrix")
  expect_equal(
    as.matrix(section$counts),
    matrix(c(1, 0, 3, 0, 2, 0, 4, 0, 0), 3, 3, dimnames = list(genes, barcodes))
  )
  cells <- section$cells
  expect_identical(rownames(cells), barcodes)
  expect_identical(cells$cell_barcode, barcodes)
  ## The tissue name drops the replicate suffix.
  expect_identical(unique(cells$sample), "Lung5_Rep2")
  expect_identical(unique(cells$tissue), "Lung5")
  expect_identical(
    cells$fov,
    c("Lung5_Rep2_F01", "Lung5_Rep2_F01", "Lung5_Rep2_F02")
  )
  expect_identical(cells$nUMI, c(4L, 2L, 4L))
  expect_identical(cells$nGene, c(2L, 1L, 1L))
  ## The negative probes are not genes; their totals describe each cell.
  expect_identical(cells$neg_probe_counts, c(1L, 2L, 1L))
  expect_equal(cells$mean_dapi, rep(9, 3))
  expect_equal(cells$area, c(100, 200, 300))
  ## The slide's y axis grows downwards, the Viewer's upwards.
  expect_identical(rownames(section$coordinates), barcodes)
  expect_equal(section$coordinates$x, c(10, 20, 30))
  expect_equal(section$coordinates$y, c(-100, -200, -300))

  ## Inconsistent flat files are rejected.
  for (change in list(
    list(column = "cell_ID", value = c(1, 2, 99), error = "different cells"),
    list(
      column = "CenterX_global_px",
      value = NULL,
      error = "CenterX_global_px"
    )
  )) {
    files <- cosmx_flat_files("Lung1")
    metadata <- utils::read.csv(files[["metadata"]])
    metadata[[change$column]] <- change$value
    utils::write.csv(metadata, files[["metadata"]], row.names = FALSE)
    expect_error(env$.viewerCosMxReadSample(files, "Lung1"), change$error)
  }

  ## A spatial entry keeps the genes but not a second copy of the cell names.
  entry <- env$.viewerCosMxSpatialEntry(section$coordinates, genes[1:2])
  expect_named(entry, c("coordinates", "expression", "histology_images"))
  expect_identical(dim(entry$expression), c(2L, 3L))
  expect_identical(rownames(entry$expression), genes[1:2])
  expect_null(colnames(entry$expression))
  expect_length(entry$expression@x, 0L)
  object <- Cerebro$new()
  object$addSpatialData("section", entry)
  expect_identical(object$availableSpatial(), "section")
})

test_that("the T-cell cell table, pseudotime and trajectory follow the atlas", {
  env <- large_demo_env("prepare_viewer_tcell_data.R")
  meta <- tcell_meta()
  metadata <- tcell_cells(env)
  expect_identical(rownames(metadata), meta$cellID.uniq)
  expect_identical(metadata$cell_barcode, meta$cellID.uniq)
  expect_identical(
    as.character(metadata$gender),
    rep(c("Male", "Female"), c(2, 4))
  )
  expect_identical(as.character(metadata$lineage), meta$stype)
  expect_error(
    tcell_cells(env, expected_cells = 7L),
    "must describe 7 unique cells of 2 patients"
  )
  repeated <- meta
  repeated$cellID.uniq[[2]] <- repeated$cellID.uniq[[1]]
  expect_error(tcell_cells(env, repeated), "unique cells")
  expect_error(tcell_cells(env, patients = "P1"), "no clinical record")

  pseudotime <- data.frame(
    mini_cluster = c("ds1.C1", "ds1.C2", "ds1.C3"),
    meta.cluster = c(
      "CD8.c01.Tn.MAL",
      "CD8.c12.Tex.CXCL13",
      "CD8.c06.Tem.GZMK"
    ),
    dpt_pseudotime = c(0.1, 0.9, 0.5),
    stringsAsFactors = FALSE
  )
  published <- env$.viewerTcellCellPseudotime(meta, pseudotime)
  expect_identical(names(published), meta$cellID.uniq)
  expect_equal(unname(published), c(0.1, NA, 0.9, 0.5, NA, NA))
  projection <- data.frame(
    Dim_1 = 1:6,
    Dim_2 = 6:1,
    row.names = meta$cellID.uniq
  )
  trajectory <- env$.viewerTcellTrajectory(meta, pseudotime, projection)
  ## The CD4 cells of C1 and C2 and the CD8 cell of C4 (not on the path) stay out.
  expect_identical(
    rownames(trajectory$meta),
    paste0("ds1.", c("a1", "b1", "b2"))
  )
  ## Rank scaled to (0, 1]: the order of the published values is kept.
  expect_equal(trajectory$meta$pseudotime, c(1 / 3, 1, 2 / 3))
  states <- c("CD8.c01.Tn.MAL", "CD8.c06.Tem.GZMK", "CD8.c12.Tex.CXCL13")
  expect_identical(levels(trajectory$meta$state), states)
  expect_equal(trajectory$meta$DR_1, c(1, 3, 4))
  expect_true(all(c("DR_1", "DR_2") %in% names(trajectory$meta)))
  expect_identical(trajectory$provenance$type, "published_pseudotime")
  ## The line follows the state order of the pseudotime.
  expect_identical(trajectory$edges$source, states[1:2])
  expect_identical(trajectory$edges$target, states[2:3])
  expect_equal(trajectory$edges$source_dim_1, c(1, 4))
  expect_equal(trajectory$edges$target_dim_1, c(4, 3))
})

test_that("TCR rows become clone calls, split by patient and tissue site", {
  env <- large_demo_env("prepare_viewer_tcell_data.R")
  tcr <- data.frame(
    Cell_Name = c("a1", "b1", "b2", "b3"),
    dataset.old = "ds1.old",
    CDR3.A1 = c("CAAF", NA, NA, "CXXF"),
    CDR3.A2 = c(NA, "CDDF", NA, NA),
    CDR3.B1 = c("CBBF", "CEEF", NA, NA),
    CDR3.B2 = NA_character_,
    Identifier.A1 = c("TRAV1_AAA(T)CC_TRAJ2", NA, NA, "TRAV9_G_TRAJ1"),
    Identifier.A2 = c(NA, "TRAV3_TTT_TRAJ4", NA, NA),
    Identifier.B1 = c("TRBV5-1_GGG_TRBJ2-3", "TRBV6_CCC_TRBJ1-1", NA, NA),
    Identifier.B2 = NA_character_,
    stringsAsFactors = FALSE
  )
  cells <- env$.viewerTcellTcrCells(tcr, tcell_meta())
  expect_identical(cells, paste0("ds1.", c("a1", "b1", "b2", "b3")))
  ## A cell name shared by two cells of the table is not assigned.
  shared <- tcell_meta()
  shared$cellID[[2]] <- "a1"
  expect_identical(
    env$.viewerTcellTcrCells(tcr[1:2, ], shared),
    c(NA, "ds1.b1")
  )

  repertoire <- env$.viewerTcellRepertoire(tcr, cells)
  expect_named(repertoire, c("barcode", "CTgene", "CTnt", "CTaa", "CTstrict"))
  ## b2 carries no chain at all and is dropped.
  expect_identical(repertoire$barcode, cells[c(1, 2, 4)])
  expect_identical(
    repertoire$CTgene,
    c("TRAV1.TRAJ2_TRBV5-1.TRBJ2-3", "TRAV3.TRAJ4_TRBV6.TRBJ1-1", "TRAV9.TRAJ1")
  )
  expect_identical(repertoire$CTaa, c("CAAF_CBBF", "CDDF_CEEF", "CXXF"))
  ## "_" only separates the chains, also in the nucleotide-level call.
  expect_identical(
    repertoire$CTnt[[1]],
    "TRAV1:AAA(T)CC:TRAJ2_TRBV5-1:GGG:TRBJ2-3"
  )
  expect_identical(repertoire$CTstrict[[3]], "TRAV9.TRAJ1;TRAV9:G:TRAJ1")
  expect_false(any(grepl("NA", unlist(repertoire[-1]), fixed = TRUE)))

  metadata <- tcell_cells(env)
  calls <- data.frame(
    barcode = c("ds1.a1", "ds1.b1", "ds1.b2", "ds1.b3", NA),
    CTgene = "g",
    CTnt = "n",
    CTaa = "a",
    CTstrict = "s",
    stringsAsFactors = FALSE
  )
  samples <- env$.viewerTcellRepertoireBySample(calls, metadata)
  expect_identical(names(samples), c("P1 (T)", "P2 (P)", "P2 (T)"))
  expect_identical(samples[["P2 (T)"]]$barcode, c("ds1.b1", "ds1.b2"))
  expect_identical(samples[["P2 (P)"]]$barcode, "ds1.b3")
  expect_identical(rownames(samples[["P1 (T)"]]), "1")
  calls$barcode[[2]] <- "ds1.a1"
  expect_error(
    env$.viewerTcellRepertoireBySample(calls, metadata),
    "unique cells"
  )
})

test_that("10x TCR identifiers give the same clone calls as Smart-seq2 ones", {
  env <- large_demo_env("prepare_viewer_tcell_data.R")
  expect_identical(
    env$.viewerTcellGenePair(c(
      "TRAV38-2DV8|None|TRAJ42|TRAC|TGTGCTTTC",
      "TRBV20-1|TRBD1|TRBJ2-3|TRBC2|TGCAGTGCT",
      "TRAV26-1_AGAGC(TT)CAGGA_TRAJ45",
      "TRAV29_DV5_AAGCGT_TRAJ36",
      NA
    )),
    c(
      "TRAV38-2DV8.TRAJ42",
      "TRBV20-1.TRBJ2-3",
      "TRAV26-1.TRAJ45",
      "TRAV29.TRAJ36",
      NA
    )
  )
  ## A 10x cell (two rows: second alpha "-" or present) next to a Smart-seq2
  ## cell; "-" marks an absent chain and never stands in for one.
  tcr <- data.frame(
    Cell_Name = c("t1", "t2", "s1"),
    CDR3.A1 = c("CAVF", "-", "CAAF"),
    CDR3.A2 = c("-", "CDDF", NA),
    CDR3.B1 = c("CASF", "CEEF", "CBBF"),
    CDR3.B2 = c("-", "-", NA),
    Identifier.A1 = c(
      "TRAV38-2DV8|None|TRAJ42|TRAC|TGTGCTTTC",
      "-",
      "TRAV1_AAA(T)CC_TRAJ2"
    ),
    Identifier.A2 = c("-", "TRAV3|None|TRAJ4|TRAC|TGTGAC", NA),
    Identifier.B1 = c(
      "TRBV20-1|TRBD1|TRBJ2-3|TRBC2|TGCAGTGCT",
      "TRBV6|None|TRBJ1-1|None|TGCGCC",
      "TRBV5-1_GGG_TRBJ2-3"
    ),
    Identifier.B2 = c("-", "-", NA),
    stringsAsFactors = FALSE
  )
  repertoire <- env$.viewerTcellRepertoire(tcr, tcr$Cell_Name)
  expect_identical(
    repertoire$CTgene,
    c(
      "TRAV38-2DV8.TRAJ42_TRBV20-1.TRBJ2-3",
      "TRAV3.TRAJ4_TRBV6.TRBJ1-1",
      "TRAV1.TRAJ2_TRBV5-1.TRBJ2-3"
    )
  )
  expect_identical(
    repertoire$CTnt,
    c(
      "TRAV38-2DV8:TGTGCTTTC:TRAJ42_TRBV20-1:TGCAGTGCT:TRBJ2-3",
      "TRAV3:TGTGAC:TRAJ4_TRBV6:TGCGCC:TRBJ1-1",
      "TRAV1:AAA(T)CC:TRAJ2_TRBV5-1:GGG:TRBJ2-3"
    )
  )
  expect_identical(repertoire$CTaa, c("CAVF_CASF", "CDDF_CEEF", "CAAF_CBBF"))
  expect_false(any(grepl("|", unlist(repertoire), fixed = TRUE)))
  expect_false(any(grepl("-_|_-|^-$", unlist(repertoire))))
})

test_that("T-cell expression objects are read by symbol and joined on their genes", {
  skip_if_not_installed("SingleCellExperiment")
  env <- large_demo_env("prepare_viewer_tcell_data.R")
  sparse <- function(i, j, x, genes, cells) {
    Matrix::sparseMatrix(
      i = i,
      j = j,
      x = x,
      dims = c(length(genes), length(cells)),
      dimnames = list(genes, cells)
    )
  }
  read <- function(
    norm,
    cells = c("u1", "u2"),
    symbols = NULL,
    assay = "norm_exprs"
  ) {
    sce <- SingleCellExperiment::SingleCellExperiment(
      assays = stats::setNames(list(norm), assay),
      colData = S4Vectors::DataFrame(cellID.uniq = cells)
    )
    if (!is.null(symbols)) {
      SummarizedExperiment::rowData(sce)$display.name <- symbols
    }
    path <- withr::local_tempfile(fileext = ".rds")
    saveRDS(sce, path)
    env$.viewerTcellReadDataset(path)
  }

  norm <- sparse(
    c(1, 2, 3, 3),
    c(1, 2, 2, 1),
    c(1, 2, 3, 4),
    c("A", "B", "C"),
    c("x", "y")
  )
  read_back <- read(norm)
  expect_identical(colnames(read_back), c("u1", "u2"))
  expect_identical(sort(rownames(read_back)), c("A", "B", "C"))
  expect_equal(read_back["C", "u2"], 3)
  expect_error(read(norm, assay = "logcounts"), "norm_exprs")
  expect_error(read(norm, cells = c("u1", "u1")), "unique cell names")

  ## Ensembl-keyed objects are renamed through display.name. ENSG2 and ENSG3
  ## share a symbol; ENSG3, detected in two cells, is kept.
  ensembl <- sparse(
    c(1, 2, 3, 3),
    c(1, 2, 1, 2),
    c(1, 2, 3, 4),
    paste0("ENSG0000000", 1:3),
    c("x", "y")
  )
  renamed <- read(ensembl, symbols = c("CD3E", "CD8A", "CD8A"))
  expect_setequal(rownames(renamed), c("CD3E", "CD8A"))
  expect_equal(renamed["CD3E", "u1"], 1)
  expect_equal(renamed["CD8A", ], c(u1 = 3, u2 = 4))
  expect_error(
    read(ensembl, symbols = rownames(ensembl)),
    "other than a symbol"
  )

  ## Joined on the union of the genes, in the order of the cell table; a gene
  ## expressed nowhere (G9) or in fewer than min_cells cells is dropped.
  first <- sparse(c(1, 2), c(1, 2), c(1, 2), c("G2", "G1"), c("c1", "c2"))
  second <- sparse(c(1, 2), c(1, 1), c(5, 6), c("G3", "G2"), "c3")
  never <- sparse(integer(), integer(), numeric(), "G9", "c4")
  combined <- env$.viewerTcellCombine(
    list(first, second, never),
    c("c3", "c1", "c4", "c2")
  )
  expect_identical(rownames(combined), c("G1", "G2", "G3"))
  expect_identical(colnames(combined), c("c3", "c1", "c4", "c2"))
  expect_equal(as.matrix(combined)["G2", ], c(c3 = 6, c1 = 1, c4 = 0, c2 = 0))
  expect_equal(as.matrix(combined)["G3", "c3"], 5)
  expect_equal(as.matrix(combined)["G1", "c2"], 2)
  expect_error(
    env$.viewerTcellCombine(list(first), c("c1", "missing")),
    "lack 1 cells"
  )
  expect_identical(
    rownames(env$.viewerTcellCombine(
      list(first, second),
      c("c1", "c2", "c3"),
      min_cells = 2L
    )),
    "G2"
  )
})

test_that("the T-cell embedding covers every cell from batch-wise variable genes", {
  skip_if_not_installed("irlba")
  skip_if_not_installed("harmony")
  skip_if_not_installed("uwot")
  env <- large_demo_env("prepare_viewer_tcell_data.R")
  set.seed(1)
  batch <- rep(c("d1", "d2"), each = 150)
  group <- rep(rep(1:2, each = 75), 2)
  dense <- matrix(
    stats::rpois(120 * 300, 0.5),
    120,
    300,
    dimnames = list(paste0("g", 1:120), paste0("c", 1:300))
  )
  dense[1:20, group == 2] <- dense[1:20, group == 2] + 3
  expression <- methods::as(
    methods::as(log1p(dense), "CsparseMatrix"),
    "generalMatrix"
  )
  genes <- env$.viewerTcellVariableGenes(
    expression,
    batch,
    n = 30L,
    bins = 4L,
    min_cells = 20L
  )
  expect_length(genes, 30L)
  expect_true(any(paste0("g", 1:20) %in% genes))
  embedding <- env$.viewerTcellEmbedding(
    expression,
    batch,
    n_genes = 30L,
    n_pcs = 5L,
    threads = 1L
  )
  expect_identical(rownames(embedding), colnames(expression))
  expect_named(embedding, c("Dim_1", "Dim_2"))
  expect_false(anyNA(embedding))
})

test_that("a compressed twin holds the same counts and is rebuilt when stale", {
  scripts <- testthat::test_path(
    "..",
    "..",
    "data-raw",
    "large-demo",
    "h5_compressed_variant.R"
  )
  skip_if_not(file.exists(scripts), "repository-only script")
  env <- new.env(parent = globalenv())
  sys.source(scripts, envir = env)

  ## Next to the CRB, under the stem the Viewer pairs it with.
  root <- withr::local_tempdir()
  crb <- file.path(root, "demo.crb")
  writeLines("source", file.path(root, "demo.h5"))
  paths <- env$.demoCompressedPaths(crb)
  expect_identical(paths$crb, file.path(root, "demo_compressed.crb"))
  expect_identical(paths$h5, file.path(root, "demo_compressed.h5"))
  expect_identical(paths$source_h5, file.path(root, "demo.h5"))
  ## Reused only with both files and a stamp of the same settings and source.
  stamp <- function(level = 3L) {
    env$.demoCompressedStamp(paths, level = level, chunk = 65536L)
  }
  current <- stamp()
  expect_false(env$.demoCompressedComplete(paths, current))
  file.create(paths$crb, paths$h5)
  expect_false(env$.demoCompressedComplete(paths, current))
  writeLines(current, paths$stamp)
  expect_true(env$.demoCompressedComplete(paths, current))
  expect_false(env$.demoCompressedComplete(paths, stamp(6L)))
  writeLines("a longer source", paths$source_h5)
  expect_false(env$.demoCompressedComplete(paths, stamp()))

  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("rhdf5")
  set.seed(3)
  genes_by_cells <- abs(Matrix::rsparsematrix(40L, 700L, 0.1))
  genes_by_cells@x <- round(genes_by_cells@x * 10) + 1
  dimnames(genes_by_cells) <- list(paste0("g", 1:40), paste0("c", 1:700))
  original <- withr::local_tempfile(fileext = ".h5")
  HDF5Array::writeTENxMatrix(
    methods::as(Matrix::t(genes_by_cells), "CsparseMatrix"),
    original,
    group = "expression",
    level = 0L
  )
  ## A step shorter than the data exercises the piecewise copy.
  compressed <- withr::local_tempfile(fileext = ".h5")
  env$.demoRecompressH5(
    original,
    compressed,
    level = 3L,
    chunk = 1024L,
    step = 1000,
    verbose = FALSE
  )
  for (dataset in c(
    "data",
    "indices",
    "indptr",
    "shape",
    "genes",
    "barcodes"
  )) {
    expect_identical(
      as.vector(rhdf5::h5read(compressed, paste0("expression/", dataset))),
      as.vector(rhdf5::h5read(original, paste0("expression/", dataset))),
      info = dataset
    )
  }
  ## The long datasets are chunked at the requested length.
  handle <- rhdf5::H5Fopen(compressed)
  withr::defer(rhdf5::H5Fclose(handle))
  values <- rhdf5::H5Dopen(handle, "expression/data")
  withr::defer(rhdf5::H5Dclose(values))
  plist <- rhdf5::H5Dget_create_plist(values)
  withr::defer(rhdf5::H5Pclose(plist))
  expect_identical(as.numeric(rhdf5::H5Pget_chunk(plist)), 1024)
})
