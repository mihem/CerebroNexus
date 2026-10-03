skip_if_not_installed("Seurat")
skip_if_not_installed("SeuratObject")

write_contig_paths <- function(object, directory) {
  barcodes <- split(colnames(object), object$orig.ident)
  vapply(
    names(barcodes),
    function(sample_name) {
      bc <- barcodes[[sample_name]]
      chain <- rep(c("TRA", "TRB"), each = length(bc))
      cell <- rep(bc, times = 2L)
      clone <- rep(seq_along(bc), times = 2L)
      is_tra <- chain == "TRA"
      contigs <- data.frame(
        barcode = cell,
        is_cell = "True",
        contig_id = paste0(cell, "_", chain),
        high_confidence = "True",
        length = 500L,
        chain = chain,
        v_gene = ifelse(is_tra, "TRAV1", "TRBV3"),
        d_gene = "None",
        j_gene = ifelse(is_tra, "TRAJ2", "TRBJ1"),
        c_gene = ifelse(is_tra, "TRAC", "TRBC1"),
        full_length = "True",
        productive = "True",
        cdr3 = ifelse(is_tra, "CAVRF", "CASSLF"),
        cdr3_nt = ifelse(is_tra, "TGTGCC", "TGTGCA"),
        reads = 100L,
        umis = 5L,
        raw_clonotype_id = paste0("clonotype", clone),
        raw_consensus_id = paste0("clonotype", clone, "_consensus_1"),
        stringsAsFactors = FALSE
      )
      path <- file.path(directory, paste0(sample_name, ".csv"))
      utils::write.csv(contigs, path, row.names = FALSE)
      path
    },
    character(1)
  )
}

## ---------------------------------------------------------------------------
## Metadata extraction
## ---------------------------------------------------------------------------

test_that("repertoire columns already in meta.data are picked up", {
  object <- make_ir_seurat(with_columns = TRUE)

  result <- addImmuneRepertoire(object, verbose = FALSE)
  repertoire <- result@misc$immune_repertoire

  expect_type(repertoire, "list")
  expect_setequal(names(repertoire), unique(object$orig.ident))
  expect_true(all(vapply(repertoire, is.data.frame, logical(1))))
  expect_true(all(vapply(
    repertoire,
    function(df) "barcode" %in% names(df),
    logical(1)
  )))
  ## the barcodes have to be the object's cell names, or nothing joins
  expect_true(all(
    unlist(lapply(repertoire, `[[`, "barcode")) %in% colnames(object)
  ))
})

test_that("an object with no repertoire columns is returned untouched", {
  object <- make_ir_seurat(with_columns = FALSE)
  result <- addImmuneRepertoire(object, verbose = FALSE)
  expect_null(result@misc$immune_repertoire)
})

test_that("from_metadata = FALSE does not scan meta.data", {
  object <- make_ir_seurat(with_columns = TRUE)
  result <- addImmuneRepertoire(object, from_metadata = FALSE, verbose = FALSE)
  expect_null(result@misc$immune_repertoire)
})

test_that("public flags require one non-missing logical value", {
  object <- make_ir_seurat(with_columns = TRUE)

  for (bad_value in list(NA, c(TRUE, FALSE), "yes")) {
    expect_error(
      addImmuneRepertoire(
        object,
        from_metadata = bad_value,
        verbose = FALSE
      ),
      regexp = "from_metadata.*TRUE or FALSE",
      ignore.case = TRUE
    )
    expect_error(
      addImmuneRepertoire(object, verbose = bad_value),
      regexp = "verbose.*TRUE or FALSE",
      ignore.case = TRUE
    )
  }
})

## ---------------------------------------------------------------------------
## Passing repertoire data in
## ---------------------------------------------------------------------------

test_that("a combineTCR-style list is stored as given", {
  object <- make_ir_seurat()
  combined <- make_ir_repertoire(object)

  result <- addImmuneRepertoire(object, tcr = combined, verbose = FALSE)
  repertoire <- result@misc$immune_repertoire

  expect_setequal(names(repertoire), names(combined))
  expect_equal(repertoire[["donorA"]], combined[["donorA"]])

  ## and the chain detector, which the HLA page depends on, sees the chains
  expect_true(all(c("TRA", "TRB") %in% hla_detect_chains(repertoire)))
})

test_that("orphan repertoire rows are removed before storage and analysis", {
  object <- make_ir_seurat()
  combined <- make_ir_repertoire(object)
  orphan <- combined[["donorA"]][1, , drop = FALSE]
  orphan$barcode <- "filtered_out_cell"
  combined[["donorA"]] <- rbind(combined[["donorA"]], orphan)

  expect_warning(
    result <- addImmuneRepertoire(object, tcr = combined, verbose = FALSE),
    regexp = "removed.*barcode|barcode.*removed",
    ignore.case = TRUE
  )
  stored <- result@misc$immune_repertoire

  expect_false("filtered_out_cell" %in% stored[["donorA"]]$barcode)
  expect_equal(nrow(stored[["donorA"]]), 4L)
})

test_that("orphan rows do not inflate scRepertoire abundance", {
  skip_if_not_installed("scRepertoire")
  object <- make_ir_seurat()
  combined <- make_ir_repertoire(object)
  orphan <- combined[["donorA"]][1, , drop = FALSE]
  orphan$barcode <- "filtered_out_cell"
  combined[["donorA"]] <- rbind(combined[["donorA"]], orphan)
  result <- suppressWarnings(
    addImmuneRepertoire(object, tcr = combined, verbose = FALSE)
  )
  stored <- result@misc$immune_repertoire

  abundance <- scRepertoire::clonalAbundance(
    stored,
    cloneCall = "gene",
    chain = "both",
    exportTable = TRUE
  )
  donor_a <- abundance$Abundance[abundance$values == "donorA"]
  expect_equal(donor_a, 4)
})

test_that("TCR and BCR for the same samples are row-bound, not concatenated", {
  ## A name identifies one biological sample, not one receptor type. Simply
  ## concatenating the two lists gives two entries called `donorA`, and
  ## `x[["donorA"]]` returns the first -- so the B cells would vanish while the
  ## app went on reading the names as sample identifiers.
  object <- make_ir_seurat()
  combined <- make_ir_repertoire(object)
  tcr <- lapply(combined, function(df) {
    df[seq_len(floor(nrow(df) / 2)), , drop = FALSE]
  })
  bcr <- lapply(combined, function(df) {
    df <- df[seq.int(floor(nrow(df) / 2) + 1L, nrow(df)), , drop = FALSE]
    df$CTgene <- "IGHV1.IGHJ4.IGHM_IGKV3.IGKJ1.IGKC"
    df
  })

  result <- addImmuneRepertoire(object, tcr = tcr, bcr = bcr, verbose = FALSE)
  repertoire <- result@misc$immune_repertoire

  expect_setequal(names(repertoire), names(tcr))
  expect_false(anyDuplicated(names(repertoire)) > 0)

  donor_a <- repertoire[["donorA"]]
  expect_equal(nrow(donor_a), nrow(tcr[["donorA"]]) + nrow(bcr[["donorA"]]))
  ## both receptor types survive, which is the whole point
  expect_true(any(grepl("^TRAV", donor_a$CTgene)))
  expect_true(any(grepl("^IGHV", donor_a$CTgene)))
  expect_true(all(
    c("TRA", "TRB", "IGH", "IGK") %in% hla_detect_chains(repertoire)
  ))
})

test_that("a sample present in only one receptor list is carried through", {
  object <- make_ir_seurat()
  tcr <- make_ir_repertoire(object)
  bcr <- lapply(make_ir_repertoire(object)["donorA"], function(df) {
    df[nrow(df), , drop = FALSE]
  })
  tcr[["donorA"]] <- tcr[["donorA"]][
    tcr[["donorA"]]$barcode != bcr[["donorA"]]$barcode,
    ,
    drop = FALSE
  ]
  bcr[["donorA"]]$CTgene <- "IGHV1.IGHJ4.IGHM_IGKV3.IGKJ1.IGKC"

  result <- addImmuneRepertoire(object, tcr = tcr, bcr = bcr, verbose = FALSE)
  repertoire <- result@misc$immune_repertoire

  expect_setequal(names(repertoire), c("donorA", "donorB"))
  expect_equal(nrow(repertoire[["donorB"]]), nrow(tcr[["donorB"]]))
})

test_that("an empty receptor side does not disturb the non-empty side", {
  ## A receptor input can legitimately contain no cells after filtering. It is
  ## absence, not a sample with zero analysis units.
  object <- make_ir_seurat()

  tcr <- make_ir_repertoire(object)
  bcr <- lapply(make_ir_repertoire(object), function(df) {
    df[0, , drop = FALSE]
  })

  expect_no_error(
    result <- addImmuneRepertoire(object, tcr = tcr, bcr = bcr, verbose = FALSE)
  )
  repertoire <- result@misc$immune_repertoire

  expect_setequal(names(repertoire), names(tcr))
  expect_equal(repertoire, tcr)
  expect_equal(nrow(repertoire[["donorA"]]), nrow(tcr[["donorA"]]))
  expect_true("CTstrict" %in% names(repertoire[["donorA"]]))
  expect_false(anyNA(repertoire[["donorA"]]$CTstrict))
})

test_that("an all-empty repertoire is treated as no repertoire", {
  ## Explicit empty input is an answer, not permission to resurrect stale
  ## receptor columns from metadata.
  object <- make_ir_seurat(with_columns = TRUE)
  empty <- lapply(make_ir_repertoire(object), function(df) {
    df[0, , drop = FALSE]
  })

  expect_no_error(
    result <- addImmuneRepertoire(
      object,
      tcr = empty,
      verbose = FALSE
    )
  )
  expect_true(
    is.null(result@misc$immune_repertoire) ||
      length(result@misc$immune_repertoire) == 0L
  )
})

test_that("explicit empty input clears an existing unified repertoire", {
  object <- make_ir_seurat(with_columns = TRUE)
  object@misc$immune_repertoire <- make_ir_repertoire(object)
  empty <- lapply(make_ir_repertoire(object), function(df) {
    df[0, , drop = FALSE]
  })

  result <- addImmuneRepertoire(
    object,
    tcr = empty,
    verbose = FALSE
  )

  expect_null(result@misc$immune_repertoire)
})

test_that("an existing unified repertoire outranks metadata extraction", {
  object <- make_ir_seurat(with_columns = TRUE)
  unified <- make_ir_repertoire(object)
  object@misc$immune_repertoire <- unified
  object$CTgene <- "STALE_METADATA_VALUE"

  result <- addImmuneRepertoire(object, verbose = FALSE)

  expect_identical(result@misc$immune_repertoire, unified)
  expect_false(any(vapply(
    result@misc$immune_repertoire,
    function(df) any(df$CTgene == "STALE_METADATA_VALUE"),
    logical(1)
  )))
})

test_that("the same cell cannot appear in both TCR and BCR inputs", {
  object <- make_ir_seurat()
  tcr <- make_ir_repertoire(object)["donorA"]
  tcr[["donorA"]] <- tcr[["donorA"]][1, , drop = FALSE]
  bcr <- tcr
  bcr[["donorA"]]$CTgene <- "IGHV1.IGHJ4.IGHM_IGKV3.IGKJ1.IGKC"

  expect_error(
    addImmuneRepertoire(
      object,
      tcr = tcr,
      bcr = bcr,
      from_metadata = FALSE,
      verbose = FALSE
    ),
    regexp = "same cell.*TCR.*BCR|both TCR and BCR",
    ignore.case = TRUE
  )
})

test_that("metadata repertoire rows require a sample identity", {
  object <- make_ir_seurat(with_columns = TRUE)
  object$orig.ident <- as.character(object$orig.ident)
  object$orig.ident[1] <- NA_character_

  expect_error(
    addImmuneRepertoire(object, from_metadata = TRUE, verbose = FALSE),
    regexp = "missing or empty sample identities|orig.ident",
    ignore.case = TRUE
  )
})

test_that("an explicit sample_col is strict while NULL requests detection", {
  object <- make_ir_seurat(with_columns = TRUE)

  expect_error(
    addImmuneRepertoire(
      object,
      sample_col = "typo_sample",
      verbose = FALSE
    ),
    regexp = "sample_col.*typo_sample|typo_sample.*metadata",
    ignore.case = TRUE
  )

  expect_no_error(
    detected <- addImmuneRepertoire(
      object,
      sample_col = NULL,
      verbose = FALSE
    )
  )
  expect_setequal(
    names(detected@misc$immune_repertoire),
    unique(object$orig.ident)
  )
})

test_that("conversion follows a renamed metadata sample column", {
  object <- make_ir_seurat(with_columns = TRUE)
  output_dir <- withr::local_tempdir()

  expect_no_error(convertSeuratToCerebro(
    seurat_file = object,
    result_dir = output_dir,
    assay = "RNA",
    slot = "data",
    experiment_name = "renamed sample",
    organism = "hg",
    groups = c("orig.ident", "cluster"),
    groups_naming = c(orig.ident = "donor_id"),
    nUMI = "nCount_RNA",
    nGene = "nFeature_RNA",
    add_most_expressed_genes = FALSE,
    verbose = FALSE
  ))

  path <- list.files(output_dir, pattern = "\\.crb$", full.names = TRUE)
  expect_length(path, 1L)
  repertoire <- readCerebro(path)$getImmuneRepertoire()
  expect_setequal(names(repertoire), unique(object$orig.ident))
})

test_that("an .rds path is read", {
  object <- make_ir_seurat()
  combined <- make_ir_repertoire(object)
  path <- tempfile(fileext = ".rds")
  saveRDS(combined, path)

  result <- addImmuneRepertoire(object, tcr = path, verbose = FALSE)
  expect_setequal(names(result@misc$immune_repertoire), names(combined))
})

test_that("a bad shape is rejected at the entry point, not at export", {
  object <- make_ir_seurat()
  flat <- do.call(rbind, make_ir_repertoire(object))

  expect_error(
    addImmuneRepertoire(object, tcr = flat, verbose = FALSE),
    regexp = "named list"
  )
})

## ---------------------------------------------------------------------------
## End to end
## ---------------------------------------------------------------------------

test_that("the slot survives export as samples, not as column names", {
  object <- make_ir_seurat()
  combined <- make_ir_repertoire(object)
  object <- addImmuneRepertoire(object, tcr = combined, verbose = FALSE)

  crb_path <- tempfile(fileext = ".crb")
  exportFromSeurat(
    object = object,
    file = crb_path,
    experiment_name = "repertoire api",
    organism = "hg",
    groups = "sample",
    nUMI = "nCount_RNA",
    nGene = "nFeature_RNA",
    verbose = FALSE
  )

  loaded <- readCerebro(crb_path)$getImmuneRepertoire()
  expect_setequal(names(loaded), names(combined))
  ## the failure this guards against: column names standing in for samples
  expect_false(any(c("barcode", "CTgene", "CTaa") %in% names(loaded)))
})

## ---------------------------------------------------------------------------
## Cell Ranger contigs
## ---------------------------------------------------------------------------

test_that("CSV paths accept only explicit sample mappings", {
  skip_if_not_installed("scRepertoire")

  object <- make_ir_seurat()
  paths <- write_contig_paths(object, withr::local_tempdir())
  sample_names <- names(paths)

  ## `combineTCR(samples = )` prefixes every barcode with the sample name, so
  ## the cell names have to carry the same prefix -- the trap both vignettes
  ## warn about. Do it, so this exercises the path that actually works rather
  ## than accepting a repertoire that reaches no cell.
  object <- SeuratObject::RenameCells(
    object,
    new.names = paste0(object$orig.ident, "_", colnames(object))
  )

  from_named_paths <- addImmuneRepertoire(
    object,
    tcr = paths,
    verbose = FALSE
  )
  from_explicit_names <- addImmuneRepertoire(
    object,
    tcr = unname(paths),
    sample_names = sample_names,
    verbose = FALSE
  )
  repertoire <- from_named_paths@misc$immune_repertoire

  expect_true(length(repertoire) > 0)
  expect_equal(
    from_explicit_names@misc$immune_repertoire,
    repertoire
  )
  expect_true(all(vapply(repertoire, is.data.frame, logical(1))))
  expect_true(all(vapply(
    repertoire,
    function(df) all(c("barcode", "CTgene", "CTaa") %in% names(df)),
    logical(1)
  )))

  ## the assertion that matters: the barcodes reach the cells. Well-named
  ## columns over an empty join are exactly the failure this entry point exists
  ## to prevent.
  repertoire_barcodes <- unique(unlist(
    lapply(repertoire, `[[`, "barcode"),
    use.names = FALSE
  ))
  expect_true(all(repertoire_barcodes %in% colnames(object)))
  expect_gt(length(repertoire_barcodes), 0)
})

test_that("CSV paths never guess biological sample names", {
  skip_if_not_installed("scRepertoire")

  object <- make_ir_seurat()
  paths <- write_contig_paths(object, withr::local_tempdir())

  invalid_names <- list(
    absent = NULL,
    incomplete = c("donorA", ""),
    missing = c("donorA", NA_character_),
    duplicated = c("donorA", "donorA")
  )
  for (bad_names in invalid_names) {
    bad_paths <- unname(paths)
    names(bad_paths) <- bad_names
    expect_error(
      addImmuneRepertoire(
        object,
        tcr = bad_paths,
        verbose = FALSE
      ),
      "Name the path vector|sample_names"
    )
  }
  expect_error(
    addImmuneRepertoire(
      object,
      tcr = paths,
      sample_names = rev(names(paths)),
      verbose = FALSE
    ),
    "do not match"
  )
})

test_that("a repertoire whose barcodes reach no cell is refused at the entry", {
  ## The same CSV path without RenameCells: combineTCR prefixed the barcodes
  ## and nothing on the object matches them.
  skip_if_not_installed("scRepertoire")

  object <- make_ir_seurat()
  paths <- write_contig_paths(object, withr::local_tempdir())

  err <- tryCatch(
    addImmuneRepertoire(
      object,
      tcr = paths,
      verbose = FALSE
    ),
    error = function(e) conditionMessage(e)
  )
  expect_true(grepl("shares no barcode", err, fixed = TRUE))
  expect_true(grepl("RenameCells", err, fixed = TRUE))
})

## ---------------------------------------------------------------------------
## Export contract
## ---------------------------------------------------------------------------

test_that("addImmuneRepertoire is part of the package's public surface", {
  expect_true(
    "addImmuneRepertoire" %in% getNamespaceExports("CerebroNexus")
  )
})
