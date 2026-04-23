#' @keywords internal
#' @noRd
.getExpressionMatrix <- function(seurat, assay = "RNA", slot = "data", join_samples = TRUE, verbose = FALSE) {
  seurat_version <- as.character(utils::packageVersion("Seurat"))
  is_seurat_v5 <- utils::compareVersion(seurat_version, "5.0.0") >= 0

  if (!is_seurat_v5) {
    expr_matrix <- tryCatch({
      Seurat::GetAssayData(seurat, assay = assay, slot = slot)
    }, error = function(e) {
      stop(
        "Failed to get expression matrix from Seurat v4 object.\n",
        "  Assay: ", assay, "\n",
        "  Slot: ", slot, "\n",
        "  Error: ", e$message, "\n",
        "Suggestions:\n",
        "  1. Check if the assay '", assay, "' exists in your Seurat object using: names(seurat@assays)\n",
        "  2. Check if the slot '", slot, "' exists in the assay using: names(seurat@assays$", assay, "@layers)\n",
        "  3. Try using a different assay or slot (e.g., assay='RNA', slot='data')"
      )
    })
  } else {
    if (!assay %in% names(seurat@assays)) {
      stop(
        "Assay '", assay, "' not found in Seurat v5 object.\n",
        "Available assays: ", paste(names(seurat@assays), collapse = ", "), "\n",
        "Suggestions:\n",
        "  1. Use one of the available assays listed above\n",
        "  2. Check your Seurat object structure using: names(seurat@assays)"
      )
    }

    layer_names <- Layers(seurat[[assay]])
    if (join_samples && any(grepl("^counts\\.[0-9]+", layer_names))) {
      if (verbose) {
        message("[", format(Sys.time(), "%H:%M:%S"), "] Merging multi-sample layers using JoinLayers...")
      }
      seurat <- SeuratObject::JoinLayers(seurat, assay = assay)
      layer_names <- Layers(seurat[[assay]])
    }

    layer_name <- switch(slot,
                         "data" = "data",
                         "counts" = "counts",
                         "scale.data" = "scale.data",
                         slot)

    available_layers <- SeuratObject::Layers(seurat[[assay]])

    expression_data <- try(
      Seurat::GetAssayData(seurat, assay = assay, layer = layer_name),
      silent = TRUE
    )

    if (inherits(expression_data, "try-error")) {
      if (verbose) {
        message(
          "[", format(Sys.time(), "%H:%M:%S"), "] Layer `", layer_name,
          "` not found in `", assay, "` assay."
        )
      }

      fallback_layers <- c("data", "counts", "scale.data")
      for (fallback_layer in fallback_layers) {
        if (fallback_layer %in% available_layers && fallback_layer != layer_name) {
          if (verbose) {
            message(
              "[", format(Sys.time(), "%H:%M:%S"), "] Falling back to layer `",
              fallback_layer, "`"
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

      if (inherits(expression_data, "try-error") && length(available_layers) > 0) {
        if (verbose) {
          message(
            "[", format(Sys.time(), "%H:%M:%S"), "] Using first available layer: `",
            available_layers[1], "`"
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
            "Layer `", layer_name, "` could not be found in `", assay, "` assay.\n",
            "Available layers: ", paste(available_layers, collapse = ", "), "\n",
            "Suggestions:\n",
            "  1. Use one of the available layers listed above\n",
            "  2. Check if the assay has been properly initialized\n",
            "  3. Verify the assay structure using: Layers(seurat[[\"", assay, "\"]])"
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
      "  Assay: ", assay, "\n",
      "  Slot/Layer: ", slot, "\n",
      "  Seurat version: ", seurat_version, "\n",
      "Suggestions:\n",
      "  1. Verify that the assay contains data\n",
      "  2. Check the assay structure using: ",
      ifelse(is_seurat_v5, "Layers(seurat[[\"", assay, "\"]])", "names(seurat@assays$", assay, "@layers)"), "\n",
      "  3. Try using a different assay or slot"
    )
  }

  if (is.matrix(expr_matrix) || inherits(expr_matrix, "dgCMatrix")) {
    if (nrow(expr_matrix) == 0 || ncol(expr_matrix) == 0) {
      stop(
        "Expression matrix is empty (0 rows or 0 columns).\n",
        "  Assay: ", assay, "\n",
        "  Slot/Layer: ", slot, "\n",
        "  Matrix dimensions: ", nrow(expr_matrix), " rows x ", ncol(expr_matrix), " columns\n",
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
      "  Received: ", class(expr_matrix)[1], "\n",
      "  Assay: ", assay, "\n",
      "  Slot/Layer: ", slot, "\n",
      "Suggestions:\n",
      "  1. Check the assay structure in your Seurat object\n",
      "  2. Verify the data type using: class(GetAssayData(seurat, assay = \"", assay, "\"",
      ifelse(is_seurat_v5, ", layer = \"", slot, "\")", ", slot = \"", slot, "\")"), ")"
    )
  }

  return(expr_matrix)
}

##----------------------------------------------------------------------------##
## Get spatial data from Seurat object.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## Get spatial data from Seurat object.
##----------------------------------------------------------------------------##
.getSpatialData <- function(object, image = NULL, layer = "data", assay = NULL) {
  # Check if Seurat is installed
  if (!requireNamespace("Seurat", quietly = TRUE)) {
    stop("Seurat package is required but not installed.")
  }

  # Default assay
  if (is.null(assay)) {
    assay <- Seurat::DefaultAssay(object)
  }

  # Default image
  if (is.null(image)) {
    images <- Seurat::Images(object)
    if (length(images) == 0) {
      stop("No images found in the Seurat object.")
    }
    image <- images[[1]]
  }

  # Get coordinates
  # Returns a data frame with cell names as rownames
  # Enhanced logic to handle different Seurat image types (Visium, FOV, Xenium)
  image_obj <- object[[image]]
  coords <- NULL

  if ( inherits(image_obj, "VisiumV1") || inherits(image_obj, "VisiumV2") ) {
    coords <- try(Seurat::GetTissueCoordinates(object, image = image), silent = TRUE)
  } else if ( inherits(image_obj, "FOV") || inherits(image_obj, "Xenium") ) {
    # Try 1: centroids (standard v5 for segmentation-based)
    coords <- try(Seurat::GetTissueCoordinates(image_obj, which = "centroids"), silent = TRUE)

    # Try 2: default via object
    if ( inherits(coords, "try-error") || is.null(coords) ) {
      coords <- try(Seurat::GetTissueCoordinates(object, image = image), silent = TRUE)
    }

    # Try 3: Direct access to coordinates slot (legacy/specific objects)
    if ( (inherits(coords, "try-error") || is.null(coords)) && !is.null(image_obj@coordinates) ) {
      coords <- image_obj@coordinates
    }
  } else {
    # Fallback for generic/other types
    coords <- try(Seurat::GetTissueCoordinates(object, image = image), silent = TRUE)
  }

  if ( inherits(coords, "try-error") || is.null(coords) || nrow(coords) == 0 ) {
    stop(paste0("Could not retrieve coordinates for image: ", image))
  }

  coords <- as.data.frame(coords)

  # Ensure we have cell names as rownames if 'cell' column exists
  if ( "cell" %in% colnames(coords) ) {
    rownames(coords) <- coords$cell
  }

  # Get full expression data
  # GetAssayData returns a sparse matrix (features x cells)
  expr_data <- Seurat::GetAssayData(object, layer = layer, assay = assay)

  # Subset expression data to include only cells found in the image coordinates
  cells_in_image <- rownames(coords)

  # Ensure intersection of cells (in case expression matrix is missing some cells, though unlikely in valid object)
  common_cells <- intersect(cells_in_image, colnames(expr_data))

  if (length(common_cells) == 0) {
    stop("No common cells found between spatial coordinates and expression data.")
  }

  # Subset both to be safe and match order
  coords <- coords[common_cells, , drop = FALSE]
  expr_data <- expr_data[, common_cells, drop = FALSE]

  # Return a list containing both coordinates and the sparse expression matrix
  # We avoid merging into a single data frame to preserve sparsity and memory efficiency
  return(list(
    coordinates = coords,
    expression = expr_data
  ))
}
