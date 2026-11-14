## The encoded repertoire (immune_repertoire/encode.R) answers what the string
## parsers of the Immune repertoire tabs answered.

ir_encode_scope <- function() ir_scope()

ir_encode_fixture <- function() {
  list(
    s1 = data.frame(
      barcode = paste0("a", 1:9),
      CTgene = c(
        "TRAV1.TRAJ1_TRBV2.TRBJ1",
        "TRAV1.TRAJ1_TRBV2.TRBJ1",
        "TRAV3.TRAJ1_TRBV2.TRBJ1",
        "TRAV3.TRAJ1_NA",
        "NA_TRBV9.TRBJ1",
        "IGHV1.IGHJ1.IGHM_IGKV2.IGKJ1.IGKC",
        "TRAV5.TRAJ1_TRBV2.TRBJ1;TRBV4.TRBJ1",
        "TRBV2.TRBJ1_TRAV1.TRAJ1",
        NA
      ),
      CTaa = c(
        "CAA_CBB",
        "CAA_CBB",
        "CAC_CBB",
        "CAC_NA",
        "NA_CDD",
        "CHH_CKK",
        "CAE_CBB;CBF",
        "CBB_CAA",
        ""
      ),
      CTnt = c("x_y", "x_y", "z_y", "z_NA", "NA_w", "h_k", "e_y;f", "y_x", ""),
      CTstrict = c(
        "TRAV1.TRAJ1;x_TRBV2.TRBJ1;y",
        "TRAV1.TRAJ1;x_TRBV2.TRBJ1;y",
        "TRAV3.TRAJ1;z_TRBV2.TRBJ1;y",
        "TRAV3.TRAJ1;z_NA;NA",
        "NA;NA_TRBV9.TRBJ1;w",
        "IGHV1.IGHJ1.IGHM;h_IGKV2.IGKJ1.IGKC;k",
        "TRAV5.TRAJ1;e_TRBV2.TRBJ1;y;f",
        "TRBV2.TRBJ1;y_TRAV1.TRAJ1;x",
        ""
      ),
      group = c("x", "y", "x", "y", "x", "x", NA, "y", "x"),
      kind = factor(c("p", "q", "p", "q", "p", "p", "p", "q", "q")),
      stringsAsFactors = FALSE
    ),
    s2 = data.frame(
      barcode = paste0("b", 1:5),
      CTgene = c(
        "TRAV1.TRAJ1_TRBV2.TRBJ1",
        "TRAV1.TRAJ1_TRBV2.TRBJ1",
        "TRAV1.TRAJ1_TRBV2.TRBJ1",
        "TRAV7.TRAJ1_TRBV8.TRBJ1",
        "IGHV3.IGHJ2.IGHG1_IGLV1.IGLJ1.IGLC"
      ),
      CTaa = c("CAA_CBB", "CAA_CBB", "CAA_CBB", "CAG_CBH", "CHG_CLL"),
      CTnt = c("x_y", "x_y", "x2_y", "g_h", "hg_ll"),
      group = c("x", "x", "y", "y", "y"),
      kind = factor(c("p", "p", "q", "q", "q")),
      stringsAsFactors = FALSE
    )
  )
}

test_that("the encoded clones are scRepertoire's clones of every chain", {
  scope <- ir_encode_scope()
  data <- ir_encode_fixture()
  cells <- c(paste0("b", 5:1), paste0("a", 1:8))
  enc <- scope$ir_encode_repertoire(data, cells)

  expect_identical(enc$samples, c("s1", "s2"))
  expect_identical(enc$sample, rep(1:2, c(9L, 5L)))
  expect_identical(enc$cell, c(match(paste0("a", 1:9), cells), 5:1))
  expect_identical(enc$chains, c("TRA", "TRB", "IGH", "IGK", "IGL"))
  expect_identical(enc$receptors, c("TCR", "BCR"))
  ## CTstrict names the chains; s2 has no CTstrict, so CTgene does.
  expect_identical(
    enc$receptor,
    c(1L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 2L)
  )
  ## All chains: the strings, the blank one included.
  aa <- enc$clones$CTaa
  expect_identical(
    enc$names$CTaa$both[aa$both],
    c(data$s1$CTaa, data$s2$CTaa)
  )
  expect_identical(
    enc$blank,
    c(CTgene = NA, CTnt = 8L, CTaa = 8L, CTstrict = 8L)
  )
  ## TRA is the first part, TRB the second; "NA" is no clone, and a part
  ## whose gene names another chain (the BCR rows, the reversed row 8) is
  ## dropped.
  expect_identical(
    enc$names$CTaa$TRA[aa$TRA],
    c(
      "CAA",
      "CAA",
      "CAC",
      "CAC",
      NA,
      NA,
      "CAE",
      NA,
      NA,
      "CAA",
      "CAA",
      "CAA",
      "CAG",
      NA
    )
  )
  expect_identical(
    enc$names$CTaa$TRB[aa$TRB],
    c(
      "CBB",
      "CBB",
      "CBB",
      NA,
      "CDD",
      NA,
      "CBB;CBF",
      NA,
      NA,
      "CBB",
      "CBB",
      "CBB",
      "CBH",
      NA
    )
  )
  expect_identical(enc$names$CTaa$IGK[aa$IGK], c(rep(NA, 5), "CKK", rep(NA, 8)))
  expect_identical(enc$other$TRA, c(6L, 8L, 14L))
  expect_identical(enc$other$IGH, c(1:4, 7L, 8L, 10:13))
  ## A table without the column has no clone in it.
  expect_identical(enc$clones$CTstrict$both[10:14], rep(NA_integer_, 5))
})

test_that("encoded sharing clonotypes, isotypes and columns", {
  scope <- ir_encode_scope()
  data <- ir_encode_fixture()
  enc <- scope$ir_encode_repertoire(data)
  expect_identical(enc$cell, rep(NA_integer_, 14L))
  ## The V;J;CDR3 clonotype of the chain wherever its gene stands.
  expect_identical(
    enc$sharing$TRB,
    c(1L, 1L, 1L, NA, 2L, NA, 1L, 1L, NA, 1L, 1L, 1L, 3L, NA)
  )
  expect_identical(
    enc$sharing$TRA,
    c(1L, 1L, 2L, 2L, NA, NA, 3L, 1L, NA, 1L, 1L, 1L, 4L, NA)
  )
  expect_identical(enc$isotype_levels, c("IGHM", "IGHG1"))
  expect_identical(
    enc$isotype,
    c(NA, NA, NA, NA, NA, 1L, NA, NA, NA, NA, NA, NA, NA, 2L)
  )
  expect_identical(names(enc$columns), c("group", "kind"))
  expect_identical(
    as.character(enc$columns$group),
    c(data$s1$group, data$s2$group)
  )
  expect_true(attr(enc$columns$group, "character"))
  expect_identical(levels(enc$columns$kind), c("p", "q"))
  expect_false(attr(enc$columns$kind, "character"))
})

test_that("sharing clonotypes take the first V and J and the chain's CDR3", {
  scope <- ir_encode_scope()
  parts <- function(genes, aa, chain) {
    scope$ir_encode_sharing_parts(
      strsplit(genes, "_", fixed = TRUE),
      strsplit(aa, "_", fixed = TRUE),
      chain
    )
  }
  expect_identical(
    parts(
      c(
        "TRAV8-6.TRAJ8.TRAC_TRBV6-2..TRBJ2-6.TRBC2",
        "NA_TRBV18..TRBJ2-5.TRBC2",
        "TRAV3.TRAJ26.TRAC;TRAV16.TRAJ5.TRAC_TRBV14..TRBJ2-3.TRBC2",
        "TRAV8-6.TRAJ8.TRAC_NA"
      ),
      c(
        "CAVSAFFQKLVF_CASSYLPRRQDRESSGANVLTF",
        "NA_CASSPMEPIGTQYF",
        "CAVTHYGQNFVF;CASYTGRRALTF_CASSPGGQNTQYF",
        "CAVSAFFQKLVF_NA"
      ),
      "TRB"
    ),
    c(
      "TRBV6-2;TRBJ2-6;CASSYLPRRQDRESSGANVLTF",
      "TRBV18;TRBJ2-5;CASSPMEPIGTQYF",
      "TRBV14;TRBJ2-3;CASSPGGQNTQYF",
      NA
    )
  )
  expect_identical(
    parts(
      "TRAV3.TRAJ26.TRAC;TRAV16.TRAJ5.TRAC_NA",
      "CAVTHYGQNFVF;CASY_NA",
      "TRA"
    ),
    "TRAV3;TRAJ26;CAVTHYGQNFVF"
  )
})

test_that("an empty or column-less repertoire encodes without rows", {
  scope <- ir_encode_scope()
  enc <- scope$ir_encode_repertoire(list(
    a = data.frame(barcode = character(), CTgene = character())
  ))
  expect_identical(enc$sample, integer())
  expect_identical(enc$chains, character())
  expect_identical(enc$clones$CTaa$both, integer())
  expect_identical(enc$receptors, character())
})

test_that("a thin CRB's cell index encodes the cells its names would", {
  scope <- ir_encode_scope()
  repertoire <- ir_encode_fixture()
  cells <- c("b1", rev(paste0("a", 1:9)))
  named <- scope$ir_encode_repertoire(repertoire, cells)
  thin <- lapply(repertoire, function(frame) {
    frame$barcode <- match(frame$barcode, cells)
    attr(frame, "cerebro_cell_index_column") <- "barcode"
    frame
  })
  expect_identical(scope$ir_encode_repertoire(thin, cells), named)
  ## the cell names are not needed
  expect_identical(scope$ir_encode_repertoire(thin)$cell, named$cell)
})

test_that("the Viewer Pack stores the encoded repertoire and its clone names", {
  root <- tempfile("viewer-pack-ir-encoded-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  cells <- sprintf("cell-%03d", 1:4)
  object <- Cerebro$new()
  object$setMetaData(data.frame(
    cell_barcode = cells,
    group = factor(c("A", "B", "A", "B")),
    stringsAsFactors = FALSE
  ))
  object$addProjection("umap", data.frame(x = 1:4, y = 4:1, row.names = cells))
  object$addImmuneRepertoire(list(
    sample_1 = data.frame(
      barcode = cells[c(3L, 1L, 4L)],
      CTgene = c(
        "TRAV1.TRAJ1_TRBV1.TRBJ1",
        "TRAV1.TRAJ1_TRBV1.TRBJ1",
        "NA_TRBV2.TRBJ1"
      ),
      CTaa = c("CAA_CASSQ", "CAA_CASSQ", "NA_CASSR"),
      CTnt = c("tt_gg", "tt_gg", "NA_ga"),
      CTstrict = c(
        "TRAV1.TRAJ1;tt_TRBV1.TRBJ1;gg",
        "TRAV1.TRAJ1;tt_TRBV1.TRBJ1;gg",
        "NA;NA_TRBV2.TRBJ1;ga"
      ),
      stringsAsFactors = FALSE
    )
  ))
  crb <- file.path(root, "dataset.crb")
  saveCerebro(object, crb, codec = "rds")
  pack <- buildViewerPack(crb, viewer_binary = "always")
  object <- readCerebro(crb)
  manifest <- jsonlite::read_json(
    file.path(pack, "manifest.json"),
    simplifyVector = TRUE
  )
  expect_identical(
    manifest$immune_encoding,
    ir_encode_scope()$IR_ENCODING_VERSION
  )

  scope <- ir_encode_scope()
  expected <- scope$ir_encode_repertoire(
    object$getImmuneRepertoire(),
    object$getMetaData()$cell_barcode
  )
  runtime <- new.env(parent = globalenv())
  sys.source(viewer_test_path("core", "viewer_pack.R"), envir = runtime)
  descriptor <- runtime$viewerPackOpen(crb)
  encoded <- runtime$viewerPackImmuneEncoded(
    descriptor,
    ir_encode_scope()$IR_ENCODING_VERSION
  )
  blocks <- encoded$name_blocks
  encoded$name_blocks <- NULL
  expect_identical(encoded, expected[setdiff(names(expected), "names")])
  expect_identical(encoded$cell, c(3L, 1L, 4L))
  ## The names are read block by block, only the requested ones.
  for (chain in names(expected$names$CTaa)) {
    names <- expected$names$CTaa[[chain]]
    ids <- rev(seq_along(names))
    expect_identical(
      runtime$viewerPackImmuneCloneNames(
        descriptor,
        "CTaa",
        chain,
        ids,
        blocks
      ),
      names[ids]
    )
  }
  ## Several blocks per chain.
  root <- tempfile("name-blocks-")
  values <- list(both = paste0("clone", 1:7), TRB = c("x", "y"))
  written <- CerebroNexus:::.viewerPackWriteNameBlocks(
    root,
    "n.bin",
    values,
    3L
  )
  fake <- list(
    path = root,
    cache = new.env(),
    manifest = list(assets = written$asset)
  )
  fake$manifest$assets$path <- "immune/names/CTnt.bin"
  dir.create(file.path(root, "immune", "names"), recursive = TRUE)
  file.copy(file.path(root, "n.bin"), file.path(root, "immune/names/CTnt.bin"))
  read <- function(chain, ids) {
    runtime$viewerPackImmuneCloneNames(
      fake,
      "CTnt",
      chain,
      ids,
      list(size = 3L, offsets = list(CTnt = written$offsets))
    )
  }
  expect_identical(
    read("both", c(7L, 1L, 4L, 3L)),
    paste0("clone", c(7, 1, 4, 3))
  )
  expect_identical(read("TRB", 2:1), c("y", "x"))
  ## Ranks order the names.
  expect_identical(
    expected$names$CTaa$both[order(expected$rank$CTaa$both)],
    sort(expected$names$CTaa$both, method = "radix")
  )
  version <- ir_encode_scope()$IR_ENCODING_VERSION
  expect_null(runtime$viewerPackImmuneEncoded(descriptor, version + 1L))
  ## A manifest without the encoding has no answer; the page encodes in memory.
  descriptor$manifest$immune_encoding <- NULL
  expect_null(runtime$viewerPackImmuneEncoded(descriptor, version))
})

test_that("clone ranks sort the names in byte order, from the Pack or in memory", {
  scope <- ir_encode_scope()
  names <- c("TRBV10", "TRBV1", "TRAV2", "b", "B", "TRBV2.TRBJ1;CASS", "")
  expected <- integer(length(names))
  expected[order(names, method = "radix")] <- seq_along(names)
  encoded <- list(names = list(CTgene = list(TRB = names)))
  expect_identical(scope$ir_clone_rank(encoded, "CTgene", "TRB"), expected)
  ## Upper case before lower case, whatever the machine's locale.
  expect_lt(expected[[5L]], expected[[4L]])
  ## A Pack's stored ranks win; a chain without names has none.
  encoded$rank <- list(CTgene = list(TRB = rev(expected)))
  expect_identical(
    scope$ir_clone_rank(encoded, "CTgene", "TRB"),
    rev(expected)
  )
  expect_null(scope$ir_clone_rank(encoded, "CTgene", "TRA"))
})
