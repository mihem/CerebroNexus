## Argument checks shared by the functions that read a Seurat object. Their
## messages are user-facing; keep them unchanged.

## Seurat is a Suggests dependency and must be at least v3.0.
.checkSeuratAvailable <- function() {
  if (!requireNamespace("Seurat", quietly = TRUE)) {
    stop(
      "The 'Seurat' package is needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  if (utils::packageVersion('Seurat') < "3") {
    stop(
      paste0(
        "The installed Seurat package is of version `",
        utils::packageVersion('Seurat'),
        "`, but at least v3.0 is required."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.checkSeuratObject <- function(object) {
  if (!inherits(object, "Seurat")) {
    stop(
      paste0(
        "Provided object is of class `",
        paste(class(object), collapse = ", "),
        "` but must be of class 'Seurat'."
      ),
      call. = FALSE
    )
  }
  if (object@version < "3") {
    stop(
      paste0(
        "Provided Seurat object has version `",
        object@version,
        "` but must be at least 3.0."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.checkSeuratAssay <- function(object, assay) {
  if (assay %in% names(object@assays) == FALSE) {
    stop(
      paste0(
        'Specified assay slot `',
        assay,
        '` could not be found in provided Seurat object.'
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.checkSeuratGroups <- function(object, groups) {
  missing_groups <- groups[!groups %in% colnames(object@meta.data)]
  if (length(missing_groups)) {
    stop(
      paste0(
        "Group(s) `",
        paste0(missing_groups, collapse = '`, `'),
        "` were not ",
        "found in meta data of provided Seurat object. Only grouping variables ",
        "that are present in the meta data can be used."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}
