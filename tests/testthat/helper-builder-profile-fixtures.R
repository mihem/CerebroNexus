builder_profile_source_runtime <- function(local = parent.frame()) {
  files <- c(
    builder_profile_inst_path(
      "viewer",
      "core",
      "viewer_content_contract.R"
    ),
    builder_profile_inst_path(
      "viewer",
      "core",
      "spatial_coordinate_contract.R"
    ),
    builder_profile_inst_path("builder", "spatial.R")
  )
  for (file in files[nzchar(files) & file.exists(files)]) {
    sys.source(file, envir = local)
  }
  hla_core <- builder_test_source_hla_core(local)
  content <- c(
    builder_profile_inst_path("builder", "manifest.R"),
    builder_profile_inst_path("builder", "content_tables.R"),
    builder_profile_inst_path("builder", "content_immune.R"),
    builder_profile_inst_path("builder", "content_spatial.R"),
    builder_profile_inst_path("builder", "content.R"),
    builder_profile_inst_path("builder", "profile.R")
  )
  for (file in content[nzchar(content) & file.exists(content)]) {
    sys.source(file, envir = local)
  }
  invisible(c(files, hla_core, content))
}

builder_profile_source_fixture <- function() {
  list(
    type = "example",
    location = "pbmc_small",
    fingerprint = "fixture-pbmc-small",
    format = "rds"
  )
}

builder_profile_pbmc <- function() {
  SeuratObject::pbmc_small
}

builder_profile_matrix <- function(cells, features = paste0("G", seq_len(5L))) {
  counts <- Matrix::Matrix(
    matrix(
      seq_len(length(features) * length(cells)),
      nrow = length(features),
      dimnames = list(features, cells)
    ),
    sparse = TRUE
  )
  counts
}

builder_profile_add_layer <- function(assay, name, cells) {
  data <- SeuratObject::LayerData(assay, layer = "counts")[,
    cells,
    drop = FALSE
  ]
  SeuratObject::LayerData(assay, layer = name) <- data
  assay
}

builder_profile_partition_assay <- function(mode = "complete") {
  cells <- paste0("cell", seq_len(6L))
  counts <- builder_profile_matrix(cells)
  assay <- SeuratObject::CreateAssay5Object(counts = counts)
  assay@meta.data <- data.frame(row.names = rownames(assay))

  if (identical(mode, "complete")) {
    assay <- split(
      assay,
      f = rep(c("one", "two"), each = 3L),
      layers = "counts"
    )
  } else {
    if (identical(mode, "missing")) {
      assay <- builder_profile_add_layer(assay, "data.one", cells[1:3])
      assay <- builder_profile_add_layer(assay, "data.two", cells[4:5])
    } else if (identical(mode, "overlap")) {
      assay <- builder_profile_add_layer(assay, "data.one", cells[1:4])
      assay <- builder_profile_add_layer(assay, "data.two", cells[4:6])
    } else if (identical(mode, "noise")) {
      assay <- builder_profile_add_layer(assay, "data.one", cells[1:3])
      assay <- builder_profile_add_layer(assay, "data.two", cells[4:6])
      assay <- builder_profile_add_layer(
        assay,
        "data.noise",
        c("cell1", "cell4")
      )
      assay <- builder_profile_add_layer(
        assay,
        "dataBackup",
        cells
      )
    } else if (identical(mode, "ambiguous")) {
      assay <- builder_profile_add_layer(assay, "data.a", cells[1:3])
      assay <- builder_profile_add_layer(assay, "data.b", cells[4:6])
      assay <- builder_profile_add_layer(
        assay,
        "data.c",
        c("cell1", "cell4", "cell5")
      )
      assay <- builder_profile_add_layer(
        assay,
        "data.d",
        c("cell2", "cell3", "cell6")
      )
    } else if (identical(mode, "heterogeneous")) {
      one <- SeuratObject::LayerData(assay, layer = "counts")[
        1:4,
        cells[1:3],
        drop = FALSE
      ]
      two <- SeuratObject::LayerData(assay, layer = "counts")[
        2:5,
        cells[4:6],
        drop = FALSE
      ]
      SeuratObject::LayerData(assay, layer = "data.one") <- one
      SeuratObject::LayerData(assay, layer = "data.two") <- two
    } else if (identical(mode, "same_subset")) {
      one <- SeuratObject::LayerData(assay, layer = "counts")[
        1:4,
        cells[1:3],
        drop = FALSE
      ]
      two <- SeuratObject::LayerData(assay, layer = "counts")[
        1:4,
        cells[4:6],
        drop = FALSE
      ]
      SeuratObject::LayerData(assay, layer = "data.one") <- one
      SeuratObject::LayerData(assay, layer = "data.two") <- two
    } else if (identical(mode, "nested_only")) {
      assay <- builder_profile_add_layer(
        assay,
        "data.imputed.one",
        cells[1:3]
      )
      assay <- builder_profile_add_layer(
        assay,
        "data.imputed.two",
        cells[4:6]
      )
    } else if (identical(mode, "direct_and_nested")) {
      assay <- builder_profile_add_layer(assay, "data.one", cells[1:3])
      assay <- builder_profile_add_layer(assay, "data.two", cells[4:6])
      assay <- builder_profile_add_layer(
        assay,
        "data.imputed.one",
        cells[1:3]
      )
      assay <- builder_profile_add_layer(
        assay,
        "data.imputed.two",
        cells[4:6]
      )
    }
    suppressWarnings(
      SeuratObject::LayerData(assay, layer = "counts") <- NULL
    )
  }
  stopifnot(isTRUE(methods::validObject(assay, test = TRUE, complete = TRUE)))
  assay
}

builder_profile_wrong_assay <- function() {
  expected <- paste0("cell", seq_len(6L))
  actual <- c(expected[1:5], "ghost")
  assay <- SeuratObject::CreateAssay5Object(
    counts = builder_profile_matrix(actual)
  )
  assay@meta.data <- data.frame(row.names = rownames(assay))
  stopifnot(isTRUE(methods::validObject(assay, test = TRUE, complete = TRUE)))
  list(assay = assay, expected = expected)
}

builder_profile_embeddings_fixture <- function(mode = "valid") {
  cells <- paste0("cell", seq_len(6L))
  embeddings <- matrix(
    seq_len(12L),
    ncol = 2L,
    dimnames = list(cells, c("RED_1", "RED_2"))
  )
  if (identical(mode, "shuffled")) {
    embeddings <- embeddings[rev(seq_len(nrow(embeddings))), , drop = FALSE]
  } else if (identical(mode, "missing")) {
    embeddings <- embeddings[-1L, , drop = FALSE]
  } else if (identical(mode, "extra")) {
    extra <- embeddings[1L, , drop = FALSE]
    rownames(extra) <- "ghost"
    embeddings <- rbind(embeddings, extra)
  } else if (identical(mode, "wrong_same_count")) {
    rownames(embeddings)[nrow(embeddings)] <- "ghost"
  } else if (identical(mode, "duplicate")) {
    rownames(embeddings)[2L] <- rownames(embeddings)[1L]
  } else if (identical(mode, "blank")) {
    rownames(embeddings)[2L] <- ""
  } else if (identical(mode, "non_numeric")) {
    embeddings <- matrix(
      as.character(embeddings),
      nrow = nrow(embeddings),
      dimnames = dimnames(embeddings)
    )
  } else if (identical(mode, "one_dimension")) {
    embeddings <- embeddings[, 1L, drop = FALSE]
  } else if (identical(mode, "na")) {
    embeddings[1L, 1L] <- NA_real_
  } else if (identical(mode, "nan")) {
    embeddings[1L, 1L] <- NaN
  } else if (identical(mode, "inf")) {
    embeddings[1L, 1L] <- Inf
  }
  list(embeddings = embeddings, expected = cells)
}

builder_profile_legacy_assay <- function() {
  SeuratObject::CreateAssayObject(
    counts = builder_profile_matrix(paste0("cell", seq_len(6L)))
  )
}

builder_profile_reduction_object <- function(names) {
  cells <- paste0("cell", seq_len(6L))
  object <- SeuratObject::CreateSeuratObject(builder_profile_matrix(cells))
  object$group <- rep(c("A", "B"), each = 3L)
  for (name in names) {
    key <- paste0(toupper(name), "_")
    embeddings <- matrix(
      seq_len(12L),
      ncol = 2L,
      dimnames = list(cells, paste0(key, seq_len(2L)))
    )
    object[[name]] <- SeuratObject::CreateDimReducObject(
      embeddings = embeddings,
      key = key,
      assay = "RNA"
    )
  }
  stopifnot(isTRUE(methods::validObject(object, test = TRUE, complete = TRUE)))
  object
}
