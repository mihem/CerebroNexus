#' @keywords internal
#' @noRd
.getExpressionMatrix <- function(
  seurat,
  assay = "RNA",
  slot = "data",
  join_samples = TRUE,
  verbose = FALSE
) {
  seurat_version <- as.character(utils::packageVersion("Seurat"))
  is_seurat_v5 <- utils::compareVersion(seurat_version, "5.0.0") >= 0

  if (!is_seurat_v5) {
    expr_matrix <- tryCatch(
      {
        Seurat::GetAssayData(seurat, assay = assay, slot = slot)
      },
      error = function(e) {
        stop(
          "Failed to get expression matrix from Seurat v4 object.\n",
          "  Assay: ",
          assay,
          "\n",
          "  Slot: ",
          slot,
          "\n",
          "  Error: ",
          e$message,
          "\n",
          "Suggestions:\n",
          "  1. Check if the assay '",
          assay,
          "' exists in your Seurat object using: names(seurat@assays)\n",
          "  2. Check if the slot '",
          slot,
          "' exists in the assay using: names(seurat@assays$",
          assay,
          "@layers)\n",
          "  3. Try using a different assay or slot (e.g., assay='RNA', slot='data')"
        )
      }
    )
  } else {
    if (!assay %in% names(seurat@assays)) {
      stop(
        "Assay '",
        assay,
        "' not found in Seurat v5 object.\n",
        "Available assays: ",
        paste(names(seurat@assays), collapse = ", "),
        "\n",
        "Suggestions:\n",
        "  1. Use one of the available assays listed above\n",
        "  2. Check your Seurat object structure using: names(seurat@assays)"
      )
    }

    layer_names <- SeuratObject::Layers(seurat[[assay]])
    if (join_samples && any(grepl("^counts\\.[0-9]+", layer_names))) {
      if (verbose) {
        message(
          "[",
          format(Sys.time(), "%H:%M:%S"),
          "] Merging multi-sample layers using JoinLayers..."
        )
      }
      seurat <- SeuratObject::JoinLayers(seurat, assay = assay)
      layer_names <- SeuratObject::Layers(seurat[[assay]])
    }

    layer_name <- switch(
      slot,
      "data" = "data",
      "counts" = "counts",
      "scale.data" = "scale.data",
      slot
    )

    available_layers <- SeuratObject::Layers(seurat[[assay]])

    expression_data <- try(
      Seurat::GetAssayData(seurat, assay = assay, layer = layer_name),
      silent = TRUE
    )

    if (inherits(expression_data, "try-error")) {
      if (verbose) {
        message(
          "[",
          format(Sys.time(), "%H:%M:%S"),
          "] Layer `",
          layer_name,
          "` not found in `",
          assay,
          "` assay."
        )
      }

      fallback_layers <- c("data", "counts", "scale.data")
      for (fallback_layer in fallback_layers) {
        if (
          fallback_layer %in% available_layers && fallback_layer != layer_name
        ) {
          if (verbose) {
            message(
              "[",
              format(Sys.time(), "%H:%M:%S"),
              "] Falling back to layer `",
              fallback_layer,
              "`"
            )
          }
          expression_data <- Seurat::GetAssayData(
            seurat,
            assay = assay,
            layer = fallback_layer
          )
          break
        }
      }

      if (
        inherits(expression_data, "try-error") && length(available_layers) > 0
      ) {
        if (verbose) {
          message(
            "[",
            format(Sys.time(), "%H:%M:%S"),
            "] Using first available layer: `",
            available_layers[1],
            "`"
          )
        }
        expression_data <- Seurat::GetAssayData(
          seurat,
          assay = assay,
          layer = available_layers[1]
        )
      }

      if (inherits(expression_data, "try-error")) {
        stop(
          paste0(
            "Layer `",
            layer_name,
            "` could not be found in `",
            assay,
            "` assay.\n",
            "Available layers: ",
            paste(available_layers, collapse = ", "),
            "\n",
            "Suggestions:\n",
            "  1. Use one of the available layers listed above\n",
            "  2. Check if the assay has been properly initialized\n",
            "  3. Verify the assay structure using: Layers(seurat[[\"",
            assay,
            "\"]])"
          ),
          call. = FALSE
        )
      }
    }

    expr_matrix <- expression_data
  }

  if (is.null(expr_matrix)) {
    stop(
      "Expression matrix is NULL.\n",
      "  Assay: ",
      assay,
      "\n",
      "  Slot/Layer: ",
      slot,
      "\n",
      "  Seurat version: ",
      seurat_version,
      "\n",
      "Suggestions:\n",
      "  1. Verify that the assay contains data\n",
      "  2. Check the assay structure using: ",
      if (is_seurat_v5) {
        paste0('Layers(seurat[["', assay, '"]])')
      } else {
        paste0("names(seurat@assays$", assay, "@layers)")
      },
      "\n",
      "  3. Try using a different assay or slot"
    )
  }

  if (is.matrix(expr_matrix) || inherits(expr_matrix, "dgCMatrix")) {
    if (nrow(expr_matrix) == 0 || ncol(expr_matrix) == 0) {
      stop(
        "Expression matrix is empty (0 rows or 0 columns).\n",
        "  Assay: ",
        assay,
        "\n",
        "  Slot/Layer: ",
        slot,
        "\n",
        "  Matrix dimensions: ",
        nrow(expr_matrix),
        " rows x ",
        ncol(expr_matrix),
        " columns\n",
        "Suggestions:\n",
        "  1. Check if your Seurat object contains cells and genes\n",
        "  2. Verify using: ncol(seurat) (cells) and nrow(seurat) (genes)\n",
        "  3. Ensure the assay has been properly populated with data"
      )
    }
  } else {
    stop(
      "Expression matrix is not a valid matrix type.\n",
      "  Expected: matrix or dgCMatrix\n",
      "  Received: ",
      class(expr_matrix)[1],
      "\n",
      "  Assay: ",
      assay,
      "\n",
      "  Slot/Layer: ",
      slot,
      "\n",
      "Suggestions:\n",
      "  1. Check the assay structure in your Seurat object\n",
      "  2. Verify the data type using: class(GetAssayData(seurat, assay = \"",
      assay,
      "\"",
      if (is_seurat_v5) {
        paste0(', layer = "', slot, '"))')
      } else {
        paste0(', slot = "', slot, '"))')
      }
    )
  }

  return(expr_matrix)
}

# Internal utilities shared across extraction helpers ---------------------------
#' @keywords internal
#' @noRd

.spx_msg <- function(..., verbose = FALSE) {
  if (isTRUE(verbose)) {
    message("[", format(Sys.time(), "%H:%M:%S"), "] ", ...)
  }
}

.spx_try <- function(expr) {
  suppressWarnings(suppressMessages(try(expr, silent = TRUE)))
}

.spx_is_try_error <- function(x) inherits(x, "try-error")

.spx_collapse <- function(x) {
  if (length(x) == 0) "none" else paste(x, collapse = ", ")
}

.spx_escape_regex <- function(x) {
  gsub("([\\^$.|?*+()\\[\\]{}\\\\\\-])", "\\\\\\1", x, perl = TRUE)
}

.spx_is_matrix_like <- function(x) {
  if (is.null(x)) {
    return(FALSE)
  }
  d <- .spx_try(dim(x))
  if (.spx_is_try_error(d) || is.null(d) || length(d) != 2) {
    return(FALSE)
  }
  TRUE
}

.spx_has_slot <- function(obj, slot_name) {
  if (!isS4(obj)) {
    return(FALSE)
  }
  slot_name %in% methods::slotNames(obj)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# Extract spatial coordinates and expression from a Seurat object --------------
#
# Multi-strategy extraction supporting Visium, FOV, Xenium, and generic images:
#   1. Image-based: GetTissueCoordinates with centroids/segmentation/molecules
#      fallback chain + direct slot access (S4)
#   2. Metadata: scan meta.data columns for coordinate-like columns
#   3. Automatic x/y column detection from 70+ common naming conventions
#   4. Duplicate-cell summarisation (vectorised rowsum, not per-cell rbind)
#   5. Returns list(coordinates, expression, assay, layer, image, coordinate_source)
#
#' @keywords internal
#' @noRd
.getSpatialData <- function(
  object,
  image = NULL,
  layer = "data",
  assay = NULL,
  slot = NULL,
  coord_source = c(
    "auto",
    "centroids",
    "segmentation",
    "metadata",
    "molecules"
  ),
  coord_cols = NULL,
  join_samples = TRUE,
  image_policy = c("first", "all"),
  allow_molecule_fallback = FALSE,
  warn_on_image_overlap = TRUE,
  verbose = FALSE
) {
  coord_source <- match.arg(coord_source)
  image_policy <- match.arg(image_policy)

  if (!requireNamespace("Seurat", quietly = TRUE)) {
    stop("Seurat package is required but not installed.", call. = FALSE)
  }
  if (!requireNamespace("SeuratObject", quietly = TRUE)) {
    stop("SeuratObject package is required but not installed.", call. = FALSE)
  }

  if (is.null(slot)) {
    slot <- layer
  }
  if (is.null(assay)) {
    assay <- Seurat::DefaultAssay(object)
  }

  msg <- function(...) .spx_msg(..., verbose = verbose)

  get_gtc_fun <- function() {
    for (pkg in c("Seurat", "SeuratObject")) {
      if (
        requireNamespace(pkg, quietly = TRUE) &&
          exists(
            "GetTissueCoordinates",
            envir = asNamespace(pkg),
            inherits = FALSE
          )
      ) {
        return(get("GetTissueCoordinates", envir = asNamespace(pkg)))
      }
    }
    NULL
  }
  GetTC <- function(x, ...) {
    fun <- get_gtc_fun()
    if (is.null(fun)) {
      return(structure("GetTissueCoordinates not found", class = "try-error"))
    }
    .spx_try(fun(x, ...))
  }

  as_df <- function(x) {
    if (is.null(x) || .spx_is_try_error(x)) {
      return(NULL)
    }
    if (is.data.frame(x)) {
      return(x)
    }
    if (is.matrix(x)) {
      return(as.data.frame(x, stringsAsFactors = FALSE))
    }
    out <- .spx_try(as.data.frame(x, stringsAsFactors = FALSE))
    if (.spx_is_try_error(out) || is.null(out)) {
      return(NULL)
    }
    out
  }

  clean_name <- function(x) tolower(gsub("[^a-z0-9]+", "", x))

  find_col <- function(df, candidates) {
    nms <- colnames(df)
    if (is.null(nms) || length(nms) == 0) {
      return(NULL)
    }
    idx <- match(clean_name(candidates), clean_name(nms), nomatch = 0)
    idx <- idx[idx > 0]
    if (length(idx) > 0) nms[idx[1]] else NULL
  }

  X_CANDIDATES <- c(
    "x",
    "X",
    "coord_x",
    "coordinate_x",
    "spatial_x",
    "spatial_1",
    "sdimx",
    "center_x",
    "centroid_x",
    "x_centroid",
    "x_center",
    "global_x",
    "x_global",
    "aligned_x",
    "x_aligned",
    "cell_x",
    "cell.global.x",
    "cell_global_x",
    "nucleus_x",
    "nucleus.global.x",
    "nucleus_global_x",
    "imagecol",
    "image_col",
    "pxl_col_in_fullres",
    "pixel_col",
    "col",
    "column"
  )
  Y_CANDIDATES <- c(
    "y",
    "Y",
    "coord_y",
    "coordinate_y",
    "spatial_y",
    "spatial_2",
    "sdimy",
    "center_y",
    "centroid_y",
    "y_centroid",
    "y_center",
    "global_y",
    "y_global",
    "aligned_y",
    "y_aligned",
    "cell_y",
    "cell.global.y",
    "cell_global_y",
    "nucleus_y",
    "nucleus.global.y",
    "nucleus_global_y",
    "imagerow",
    "image_row",
    "pxl_row_in_fullres",
    "pixel_row",
    "row"
  )

  find_xy_cols <- function(df, user_cols = NULL, hard_error = FALSE) {
    if (!is.null(user_cols)) {
      if (length(user_cols) != 2) {
        if (hard_error) {
          stop("`coord_cols` must be length 2.", call. = FALSE)
        }
        return(NULL)
      }
      if (!all(user_cols %in% colnames(df))) {
        if (hard_error) {
          stop(
            "`coord_cols` not found: ",
            paste(setdiff(user_cols, colnames(df)), collapse = ", "),
            call. = FALSE
          )
        }
        return(NULL)
      }
      return(list(x = user_cols[1], y = user_cols[2]))
    }
    x_col <- find_col(df, X_CANDIDATES)
    y_col <- find_col(df, Y_CANDIDATES)
    if (is.null(x_col) || is.null(y_col)) {
      return(NULL)
    }
    list(x = x_col, y = y_col)
  }

  find_best_cell_col <- function(df, valid_cells) {
    if (is.null(valid_cells) || length(valid_cells) == 0) {
      return(NULL)
    }
    cell_name_candidates <- c(
      "cell",
      "cells",
      "cell_id",
      "cellid",
      "cell.id",
      "barcode",
      "barcodes",
      "Barcode",
      "CELL",
      "Cell",
      "object",
      "object_id",
      "ObjectID",
      "ID",
      "id",
      "name"
    )
    cand <- intersect(cell_name_candidates, colnames(df))
    if (length(cand) == 0) {
      return(NULL)
    }
    overlaps <- vapply(
      cand,
      function(cc) {
        sum(as.character(df[[cc]]) %in% valid_cells, na.rm = TRUE)
      },
      numeric(1)
    )
    if (max(overlaps, na.rm = TRUE) == 0) {
      return(NULL)
    }
    cand[which.max(overlaps)]
  }

  summarise_duplicate_cells <- function(df) {
    if (nrow(df) == 0) {
      return(df)
    }
    cell_ids <- rownames(df)
    if (!anyDuplicated(cell_ids)) {
      return(df)
    }

    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    other_cols <- setdiff(names(df), num_cols)
    f <- factor(cell_ids, levels = unique(cell_ids))

    num_summary <- if (length(num_cols) > 0) {
      mat <- as.matrix(df[, num_cols, drop = FALSE])
      mat[!is.finite(mat)] <- NA
      not_na <- !is.na(mat)
      mat0 <- mat
      mat0[is.na(mat0)] <- 0
      sums <- rowsum(mat0, group = f, reorder = FALSE)
      cnts <- rowsum(not_na + 0, group = f, reorder = FALSE)
      means <- sums / cnts
      means[!is.finite(means)] <- NA
      as.data.frame(means, stringsAsFactors = FALSE)
    } else {
      data.frame(row.names = levels(f))
    }

    if (length(other_cols) > 0) {
      idx_first <- ave(seq_len(nrow(df)), f, FUN = function(i) i[1])
      first_rows <- !duplicated(f)
      other_summary <- df[first_rows, other_cols, drop = FALSE]
      rownames(other_summary) <- as.character(f[first_rows])
      other_summary <- other_summary[rownames(num_summary), , drop = FALSE]
      out <- cbind(num_summary, other_summary)
    } else {
      out <- num_summary
    }
    rownames(out) <- levels(f)
    out
  }

  standardize_coord_df <- function(
    df,
    valid_cells,
    source = NA_character_,
    image_name = NA_character_,
    user_cols = NULL
  ) {
    df <- as_df(df)
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }

    xy <- find_xy_cols(df, user_cols = user_cols, hard_error = FALSE)
    if (is.null(xy)) {
      return(NULL)
    }

    df$x <- suppressWarnings(as.numeric(df[[xy$x]]))
    df$y <- suppressWarnings(as.numeric(df[[xy$y]]))
    if (all(is.na(df$x)) || all(is.na(df$y))) {
      return(NULL)
    }

    current_overlap <- if (!is.null(rownames(df))) {
      sum(rownames(df) %in% valid_cells)
    } else {
      0
    }

    best_col <- find_best_cell_col(df, valid_cells)
    if (!is.null(best_col)) {
      best_overlap <- sum(as.character(df[[best_col]]) %in% valid_cells)
      if (best_overlap >= current_overlap) {
        rownames(df) <- as.character(df[[best_col]])
      }
    }

    keep <- !is.na(rownames(df)) & rownames(df) != ""
    df <- df[keep, , drop = FALSE]
    if (nrow(df) == 0) {
      return(NULL)
    }

    df <- summarise_duplicate_cells(df)
    if (sum(rownames(df) %in% valid_cells) == 0) {
      return(NULL)
    }

    df$.coordinate_source <- source
    df$.image <- image_name
    df$.cell <- rownames(df)
    df
  }

  rbind_fill <- function(dfs) {
    dfs <- Filter(Negate(is.null), dfs)
    if (length(dfs) == 0) {
      return(NULL)
    }
    all_cols <- unique(unlist(lapply(dfs, colnames)))
    dfs <- lapply(dfs, function(d) {
      for (cc in setdiff(all_cols, colnames(d))) {
        d[[cc]] <- NA
      }
      d[, all_cols, drop = FALSE]
    })
    out <- do.call(rbind, dfs)

    if (".cell" %in% colnames(out)) {
      dup <- duplicated(out$.cell)
      if (any(dup) && isTRUE(warn_on_image_overlap)) {
        warning(
          "Duplicate cells found across images; keeping first occurrence. ",
          "n duplicates = ",
          sum(dup),
          call. = FALSE
        )
      }
      out <- out[!dup, , drop = FALSE]
      rownames(out) <- out$.cell
    }
    out
  }

  # 1. Expression matrix ------------------------------------------------------
  if (exists(".getExpressionMatrix", mode = "function")) {
    expr_data <- .getExpressionMatrix(
      seurat = object,
      assay = assay,
      slot = slot,
      join_samples = join_samples,
      verbose = verbose
    )
  } else {
    seurat_version <- as.character(utils::packageVersion("Seurat"))
    is_v5 <- utils::compareVersion(seurat_version, "5.0.0") >= 0
    expr_data <- if (is_v5) {
      Seurat::GetAssayData(object, assay = assay, layer = slot)
    } else {
      Seurat::GetAssayData(object, assay = assay, slot = slot)
    }
  }

  valid_cells <- colnames(expr_data)
  if (is.null(valid_cells) || length(valid_cells) == 0) {
    stop("Expression matrix has no cell names.", call. = FALSE)
  }

  # 2. Resolve images ---------------------------------------------------------
  img_avail <- .spx_try(Seurat::Images(object))
  if (.spx_is_try_error(img_avail) || is.null(img_avail)) {
    img_avail <- character(0)
  }

  if (is.null(image)) {
    image_names <- if (length(img_avail) == 0) {
      character(0)
    } else if (image_policy == "all") {
      img_avail
    } else {
      img_avail[1]
    }
  } else if (length(image) == 1 && identical(image, "all")) {
    image_names <- img_avail
  } else {
    image_names <- image
  }

  # 3. Image-based coordinate extraction --------------------------------------
  extract_from_one_image <- function(image_name) {
    msg("Extracting coords from image: ", image_name)
    image_obj <- .spx_try(object[[image_name]])
    if (.spx_is_try_error(image_obj) || is.null(image_obj)) {
      msg("Image object not found: ", image_name)
      return(NULL)
    }

    candidates <- list()
    candidates[["object.GetTissueCoordinates"]] <-
      GetTC(object, image = image_name)
    candidates[["image.GetTissueCoordinates.default"]] <- GetTC(image_obj)

    which_values <- switch(
      coord_source,
      "auto" = c("centroids", "cells", "segmentation"),
      "centroids" = c("centroids"),
      "segmentation" = c("segmentation"),
      "metadata" = character(0),
      "molecules" = c("molecules")
    )
    if (isTRUE(allow_molecule_fallback) && coord_source == "auto") {
      which_values <- unique(c(which_values, "molecules"))
    }

    for (ww in which_values) {
      candidates[[paste0("image.GetTissueCoordinates.", ww)]] <-
        GetTC(image_obj, which = ww)
    }

    if (isS4(image_obj)) {
      sn <- methods::slotNames(image_obj)
      direct_slots <- intersect(
        c(
          "coordinates",
          "coords",
          "centroids",
          "cells",
          "cell.centroids",
          "cell_centroids"
        ),
        sn
      )
      for (ss in direct_slots) {
        candidates[[paste0("slot.", ss)]] <- .spx_try(methods::slot(
          image_obj,
          ss
        ))
      }

      if ("boundaries" %in% sn) {
        boundaries <- .spx_try(methods::slot(image_obj, "boundaries"))
        if (!.spx_is_try_error(boundaries) && !is.null(boundaries)) {
          if (is.environment(boundaries)) {
            boundaries <- as.list(boundaries)
          }
          if (is.list(boundaries) && length(boundaries) > 0) {
            for (bn in names(boundaries)) {
              bobj <- boundaries[[bn]]
              candidates[[paste0(
                "boundary.",
                bn,
                ".GetTissueCoordinates"
              )]] <- GetTC(bobj)
              if (isS4(bobj)) {
                for (ss in intersect(
                  c("coordinates", "coords"),
                  methods::slotNames(bobj)
                )) {
                  candidates[[paste0("boundary.", bn, ".slot.", ss)]] <-
                    .spx_try(methods::slot(bobj, ss))
                }
              }
            }
          }
        }
      }
    }

    processed <- list()
    for (nm in names(candidates)) {
      processed[[nm]] <- standardize_coord_df(
        candidates[[nm]],
        valid_cells = valid_cells,
        source = nm,
        image_name = image_name,
        user_cols = coord_cols
      )
      if (!is.null(processed[[nm]])) {
        msg(
          "Match for `",
          image_name,
          "`: ",
          nm,
          " (cells=",
          nrow(processed[[nm]]),
          ")"
        )
        if (coord_source == "auto") return(processed[[nm]])
      }
    }
    rbind_fill(processed)
  }

  coords_from_images <- NULL
  if (length(image_names) > 0 && coord_source != "metadata") {
    coords_from_images <- rbind_fill(lapply(
      image_names,
      extract_from_one_image
    ))
  }

  # 4. Metadata fallback ------------------------------------------------------
  extract_from_metadata <- function() {
    msg("Trying metadata coordinate fallback.")
    meta <- .spx_try(object[[]])
    if (.spx_is_try_error(meta) || is.null(meta) || nrow(meta) == 0) {
      return(NULL)
    }

    meta$.cell <- rownames(meta)
    if (!is.null(image) && length(image) == 1 && !identical(image, "all")) {
      image_cols <- intersect(
        c(
          "image",
          "Image",
          "slice",
          "Slice",
          "fov",
          "FOV",
          "field",
          "field_of_view",
          "sample",
          "sample_id"
        ),
        colnames(meta)
      )
      for (ic in image_cols) {
        vals <- as.character(meta[[ic]])
        if (any(vals == image, na.rm = TRUE)) {
          meta <- meta[vals == image, , drop = FALSE]
          break
        }
      }
    }

    standardize_coord_df(
      meta,
      valid_cells = valid_cells,
      source = "metadata",
      image_name = if (is.null(image)) {
        NA_character_
      } else {
        paste(image, collapse = ",")
      },
      user_cols = coord_cols
    )
  }

  coords <- coords_from_images
  if (is.null(coords) || nrow(coords) == 0) {
    coords <- extract_from_metadata()
  }

  if ((is.null(coords) || nrow(coords) == 0) && !is.null(coord_cols)) {
    stop(
      "Could not find `coord_cols` (",
      paste(coord_cols, collapse = ", "),
      ") in any candidate coordinate source.",
      call. = FALSE
    )
  }

  if (is.null(coords) || nrow(coords) == 0) {
    stop(
      "Could not retrieve spatial coordinates from the Seurat object.\n",
      "Available images: ",
      .spx_collapse(img_avail),
      "\n",
      "Try: Seurat::Images(object), class(object[[image]]), or pass coord_cols=.",
      call. = FALSE
    )
  }

  # 5. Match expression and coords --------------------------------------------
  common_cells <- intersect(rownames(coords), colnames(expr_data))
  if (length(common_cells) == 0) {
    stop(
      "No common cells between coords (",
      nrow(coords),
      ") and expression (",
      ncol(expr_data),
      "). assay=",
      assay,
      ", layer=",
      slot,
      call. = FALSE
    )
  }

  coords <- coords[common_cells, , drop = FALSE]
  expr_data <- expr_data[, common_cells, drop = FALSE]

  front <- intersect(
    c(".cell", ".image", ".coordinate_source", "x", "y"),
    colnames(coords)
  )
  coords <- coords[, c(front, setdiff(colnames(coords), front)), drop = FALSE]

  list(
    coordinates = coords,
    expression = expr_data,
    assay = assay,
    layer = slot,
    image = unique(coords$.image),
    coordinate_source = unique(coords$.coordinate_source)
  )
}
