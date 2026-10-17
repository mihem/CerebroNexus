##----------------------------------------------------------------------------##
## Coordinated views (Linked Views) — per-dataset bundle builders.
##
## Pure functions that turn a loaded Cerebro object into ONE serialisable bundle
## (cells, categorical groups, every 2-D "space", per-cell clone identity). They
## carry NO input/output/session/reactive dependency, so this file is sourced by
## server.R (source(..., local = TRUE)) at runtime AND unit-tested in isolation
## (tests/testthat/test-coordinated-views.R). cv_build_bundle() tolerates the
## app-only helpers (cerebro_group_colors / Cerebro.options) being absent — each
## is guarded — so it produces the same structure outside the running app.
##
## The I()/AsIs wrapping in cv_group / cv_space / cv_clone is the load-bearing
## invariant: shiny serialises the bundle with auto_unbox = TRUE, so any array
## field that happens to be length 1 (a single-level group, a single-clonotype
## data set) must be forced to a JSON array here, or the client indexes a bare
## scalar and throws mid-update, leaving the previous data set on screen.
##----------------------------------------------------------------------------##

## Null-coalescing helper (local, so we don't depend on rlang/shiny exporting it).
`%||%` <- function(a, b) if (is.null(a)) b else a

cv_cell_ids <- function(ids, context = "data set") {
  ids <- enc2utf8(as.character(ids))
  if (anyNA(ids) || any(!nzchar(ids))) {
    stop(context, " contains a missing cell ID.", call. = FALSE)
  }
  if (anyDuplicated(ids)) {
    stop(context, " contains duplicate cell IDs.", call. = FALSE)
  }
  ids
}

cv_canonical_metadata <- function(metadata) {
  if (is.null(metadata) || !is.data.frame(metadata)) {
    return(NULL)
  }
  ids <- if ("cell_barcode" %in% colnames(metadata)) {
    metadata$cell_barcode
  } else {
    rownames(metadata)
  }
  metadata$cell_barcode <- cv_cell_ids(ids, "metadata")
  metadata
}

cv_selected_metadata <- function(metadata, cells) {
  metadata <- cv_canonical_metadata(metadata)
  if (is.null(metadata)) {
    return(NULL)
  }
  metadata[metadata$cell_barcode %in% as.character(cells), , drop = FALSE]
}

cv_cell_metadata <- function(metadata, cell) {
  metadata <- cv_canonical_metadata(metadata)
  if (is.null(metadata)) {
    return(NULL)
  }
  index <- match(as.character(cell), metadata$cell_barcode)
  if (is.na(index)) {
    return(NULL)
  }
  metadata[index, , drop = FALSE]
}

cv_cells_at_indices <- function(cells, indices) {
  if (is.null(indices) || !length(indices)) {
    return(NULL)
  }
  indices <- suppressWarnings(as.integer(indices))
  keep <- !is.na(indices) & indices >= 0L & indices < length(cells)
  if (!any(keep)) {
    return(NULL)
  }
  as.character(cells[indices[keep] + 1L])
}

## Categorical palette (mirrors the app's plotly categorical colours).
cv_palette <- c(
  "#636EFA",
  "#EF553B",
  "#00CC96",
  "#AB63FA",
  "#FFA15A",
  "#19D3F3",
  "#FF6692",
  "#B6E880",
  "#FF97FF",
  "#FECB52",
  "#2f6fd6",
  "#f97316",
  "#16a34a",
  "#9a5cd0",
  "#e05780",
  "#38b2ac",
  "#d97706",
  "#7bb0e8"
)
cv_colors_for <- function(levels) {
  n <- length(levels)
  cv_palette[((seq_len(n) - 1) %% length(cv_palette)) + 1]
}

## Per-cell clone identity + human label (CTaa) from the IR list, aligned to
## `cells`. NA where a cell carries no receptor of the selected class.
##
## The clone column and the receptor scoping both come from clone_contract.R,
## not from here. This used to read CTstrict and take every receptor at once,
## while the Clonal UMAP read CTgene within one receptor -- so the two pages
## disagreed about which cells share a clone, and the stricter column split
## clones the other page reported as one. `receptor` defaults to whichever class
## the Clonal UMAP would offer first, so the default views match.
cv_clone_per_cell <- function(ir, cells, receptor = NULL) {
  if (is.null(ir) || !length(ir)) {
    return(NULL)
  }
  present <- cerebro_receptors_present(ir)
  if (is.null(receptor)) {
    receptor <- if (length(present)) present[1] else "TCR"
  }
  clone_col <- cerebro_clonecall_col()
  rows <- do.call(
    rbind,
    lapply(ir, function(df) {
      if (
        is.null(df) ||
          !("barcode" %in% names(df)) ||
          !(clone_col %in% names(df))
      ) {
        return(NULL)
      }
      keep <- cerebro_rows_in_receptor(df, receptor, clone_col)
      df <- df[keep, , drop = FALSE]
      if (!nrow(df)) {
        return(NULL)
      }
      data.frame(
        barcode = as.character(df$barcode),
        clone = as.character(df[[clone_col]]),
        CTaa = if ("CTaa" %in% names(df)) {
          as.character(df$CTaa)
        } else {
          as.character(df[[clone_col]])
        },
        stringsAsFactors = FALSE
      )
    })
  )
  if (is.null(rows) || !nrow(rows)) {
    return(NULL)
  }
  rows <- rows[!is.na(rows$clone) & nzchar(rows$clone), , drop = FALSE]
  rows <- rows[!duplicated(rows$barcode), , drop = FALSE]
  idx <- match(cells, rows$barcode)
  list(clone = rows$clone[idx], ctaa = rows$CTaa[idx], receptor = receptor)
}

## Resolve a configured image only inside the app's public image roots. The
## canonical containment check is deliberately performed after symlink
## resolution: a configured `spatial-assets/link.png` must not become a read
## primitive for a file outside the generated app.
cv_authorized_external_image_path <- function(path, cerebro_root) {
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !is.character(cerebro_root) ||
      length(cerebro_root) != 1L ||
      is.na(cerebro_root)
  ) {
    return(NULL)
  }
  canonicalize <- function(candidate) {
    tryCatch(
      suppressWarnings(
        normalizePath(candidate, winslash = "/", mustWork = TRUE)
      ),
      error = function(error) NULL
    )
  }
  canonical_root <- canonicalize(cerebro_root)
  image_path <- canonicalize(file.path(cerebro_root, path))
  trusted_roots <- Filter(
    Negate(is.null),
    lapply(
      c("spatial-assets", "extdata"),
      function(root) canonicalize(file.path(cerebro_root, root))
    )
  )
  if (
    is.null(canonical_root) ||
      is.null(image_path) ||
      !length(trusted_roots)
  ) {
    return(NULL)
  }
  comparison_path <- image_path
  comparison_root <- canonical_root
  if (.Platform$OS.type == "windows") {
    comparison_path <- tolower(comparison_path)
    comparison_root <- tolower(comparison_root)
    trusted_roots <- lapply(trusted_roots, tolower)
  }
  ## A trusted directory may itself be a symlink. Canonicalising only the image
  ## and that directory would then bless the symlink target as a new public
  ## root. Require every trusted root to remain below the canonical app root.
  trusted_roots <- Filter(
    function(root) {
      startsWith(root, paste0(sub("/+$", "", comparison_root), "/"))
    },
    trusted_roots
  )
  if (!length(trusted_roots)) {
    return(NULL)
  }
  inside <- any(vapply(
    trusted_roots,
    function(root) {
      startsWith(comparison_path, paste0(sub("/+$", "", root), "/"))
    },
    logical(1)
  ))
  if (!inside) NULL else image_path
}

## Resolve the stable dataset id used by all per-dataset Viewer configuration.
cv_selected_dataset_name <- function() {
  nm <- if (exists("viewer_current_dataset_key", mode = "function")) {
    tryCatch(viewer_current_dataset_key(), error = function(error) NULL)
  } else if (exists("available_crb_files")) {
    viewerDatasetName(
      available_crb_files$files,
      available_crb_files$selected
    )
  } else {
    NULL
  }
  if (is.null(nm) || !length(nm) || is.na(nm) || !nzchar(nm)) {
    NULL
  } else {
    nm
  }
}

## Builder freezes per-dataset Viewer defaults into createShinyApp()'s
## `viewer_content` option. Linked views sits alongside Projection, so both
## should start from the same configured projection and point appearance.
cv_selected_viewer_content <- function() {
  if (!exists("Cerebro.options")) {
    return(list())
  }
  dataset <- cv_selected_dataset_name()
  configured <- Cerebro.options[["viewer_content"]]
  if (
    is.null(dataset) ||
      !is.list(configured) ||
      !(dataset %in% names(configured)) ||
      !is.list(configured[[dataset]])
  ) {
    return(list())
  }
  configured[[dataset]]
}

cv_alignment_appearance <- function(alignment) {
  if (!is.list(alignment)) {
    return(list(
      image_opacity = NULL,
      point_opacity = NULL,
      point_size = NULL
    ))
  }
  number <- function(key, lower, upper, lower_open = FALSE) {
    value <- suppressWarnings(as.numeric(alignment[[key]]))
    lower_bad <- if (lower_open) value <= lower else value < lower
    if (
      length(value) != 1L ||
        is.na(value) ||
        !is.finite(value) ||
        lower_bad ||
        value > upper
    ) {
      NULL
    } else {
      unname(value)
    }
  }
  list(
    image_opacity = number("image_opacity", 0, 1),
    point_opacity = number("point_opacity", 0, 1),
    point_size = number("point_size", 0, 20, lower_open = TRUE)
  )
}

cv_viewport_bounds <- function(value) {
  required <- c("xmin", "xmax", "ymin", "ymax")
  if (
    is.null(value) || is.null(names(value)) || !all(required %in% names(value))
  ) {
    return(NULL)
  }
  numbers <- suppressWarnings(as.numeric(unlist(
    value[required],
    use.names = FALSE
  )))
  if (
    length(numbers) != 4L ||
      anyNA(numbers) ||
      any(!is.finite(numbers)) ||
      numbers[[1L]] >= numbers[[2L]] ||
      numbers[[3L]] >= numbers[[4L]]
  ) {
    return(NULL)
  }
  stats::setNames(as.list(numbers), required)
}

## Convert one public per-image settings leaf to the JavaScript transform
## contract. The same helper is used for embedded and external backgrounds:
## createShinyApp() intentionally allows a setting to target either kind.
cv_image_preset <- function(spatial_name, image_label) {
  spatialImagePreset(
    if (exists("Cerebro.options")) Cerebro.options else NULL,
    cv_selected_dataset_name(),
    spatial_name,
    image_label
  )
}

## Resolve EXTERNAL histology images for one spatial entry of the selected data
## set. createShinyApp() stores them as dataset -> FOV -> image, with each leaf
## either a relative path or a descriptor containing path + coordinate bounds.
## The output is base64-encoded so the browser never receives a filesystem path.
## Linked Views defers that encoding until the selected image is requested.
cv_external_images <- function(spatial_name = NULL, defer = FALSE, roi_value = NULL) {
  if (
    !exists("Cerebro.options") ||
      is.null(Cerebro.options[["spatial_images"]])
  ) {
    return(list())
  }
  dataset <- cv_selected_dataset_name()
  images_by_dataset <- Cerebro.options[["spatial_images"]]
  if (
    is.null(dataset) ||
      !(dataset %in% names(images_by_dataset)) ||
      !is.list(images_by_dataset[[dataset]])
  ) {
    return(list())
  }
  images_by_spatial <- images_by_dataset[[dataset]]
  if (is.null(spatial_name)) {
    if (length(images_by_spatial) != 1L) {
      return(list())
    }
    spatial_name <- names(images_by_spatial)[[1L]]
  }
  if (
    !is.character(spatial_name) ||
      length(spatial_name) != 1L ||
      is.na(spatial_name) ||
      !(spatial_name %in% names(images_by_spatial))
  ) {
    return(list())
  }
  configured <- configured_spatial_images(
    Cerebro.options, dataset, spatial_name, roi_value
  )
  if (is.null(configured) || !length(configured)) {
    return(list())
  }
  labels <- names(configured)
  root <- Cerebro.options[["cerebro_root"]]
  normalize_bounds <- function(value) {
    required <- c("xmin", "xmax", "ymin", "ymax")
    if (
      is.null(value) ||
        is.null(names(value)) ||
        !all(required %in% names(value))
    ) {
      return(NULL)
    }
    numbers <- suppressWarnings(as.numeric(unlist(
      value[required],
      use.names = FALSE
    )))
    if (
      length(numbers) != 4L ||
        anyNA(numbers) ||
        any(!is.finite(numbers)) ||
        numbers[[1L]] >= numbers[[2L]] ||
        numbers[[3L]] >= numbers[[4L]]
    ) {
      return(NULL)
    }
    stats::setNames(as.list(numbers), required)
  }
  out <- list()
  image_mime <- function(image_path) {
    if (!isTRUE(file_test("-f", image_path))) {
      return(NULL)
    }
    ext <- tolower(tools::file_ext(image_path))
    if (!(ext %in% c("png", "jpg", "jpeg"))) {
      return(NULL)
    }
    bytes <- tryCatch(
      readBin(image_path, what = "raw", n = 8L),
      error = function(error) raw()
    )
    png_magic <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
    jpeg_magic <- as.raw(c(0xff, 0xd8, 0xff))
    if (identical(ext, "png") && identical(bytes, png_magic)) {
      return("image/png")
    }
    if (
      ext %in%
        c("jpg", "jpeg") &&
        length(bytes) >= length(jpeg_magic) &&
        identical(bytes[seq_along(jpeg_magic)], jpeg_magic)
    ) {
      return("image/jpeg")
    }
    NULL
  }
  for (i in seq_along(configured)) {
    descriptor <- configured[[i]]
    path <- if (is.list(descriptor)) descriptor[["path"]] else descriptor
    bounds <- if (is.list(descriptor)) {
      normalize_bounds(descriptor[["bounds"]])
    } else {
      NULL
    }
    img_path <- cv_authorized_external_image_path(path, root)
    if (is.null(img_path)) {
      next
    }
    mime <- image_mime(img_path)
    if (is.null(mime)) {
      next
    }
    base <- basename(path)
    key <- if (!is.null(labels) && nzchar(labels[[i]] %||% "")) {
      labels[[i]]
    } else {
      base
    }
    label <- if (is.list(descriptor)) descriptor$label %||% key else key
    image <- list(
      ## Section + position + label keep equal basenames and equal labels on
      ## different FOVs distinct, while remaining stable across bundle pushes.
      id = paste0("external:", spatial_name, ":", i, ":", key),
      label = label,
      bounds = bounds,
      viewport = if (is.list(descriptor)) {
        normalize_bounds(descriptor$viewport_bounds)
      } else {
        NULL
      },
      preset = cv_image_preset(spatial_name, key)
    )
    if (isTRUE(defer)) {
      image$asset_path <- img_path
      image$asset_mime <- mime
    } else {
      if (!requireNamespace("base64enc", quietly = TRUE)) {
        next
      }
      image$uri <- viewerPrivateImageUrl(img_path) %||%
        paste0(
          "data:",
          mime,
          ";base64,",
          base64enc::base64encode(img_path)
        )
    }
    out[[length(out) + 1L]] <- image
  }
  out
}

## Bundle constructors — the ONE place the "which fields are JS arrays" contract
## lives. shiny serialises the bundle with auto_unbox = TRUE, which is correct for
## the genuine scalars (n, K, defaults, max, ...) but wrong for any vector the
## client indexes as an array: a length-1 vector (a single-level group, a
## single-clonotype data set) would serialise as a bare JSON scalar, the client
## does g.levels.map()/D.clone.size[r] and throws, aborting onData mid-update and
## leaving the previous data set on screen. I() forces array serialisation. Wrap
## every array-typed field HERE so the invariant is structural — impossible to
## forget when a new field is added — instead of a scattered, easy-to-miss I().
cv_group <- function(values, levels, colors) {
  list(values = I(values), levels = I(levels), colors = I(colors))
}

cv_gene_message <- function(gene, values, maximum) {
  list(gene = gene, ok = TRUE, v = I(values), max = maximum)
}

cv_gene_panels_message <- function(genes, values, maximum) {
  list(
    ok = TRUE,
    genes = I(genes),
    values = lapply(values, I),
    max = maximum
  )
}

cv_gene_panels_payload <- function(genes, values) {
  maximum <- suppressWarnings(max(
    unlist(values, use.names = FALSE),
    na.rm = TRUE
  ))
  if (!is.finite(maximum)) {
    maximum <- 0
  }
  scaled <- lapply(values, function(value) {
    if (maximum > 0) {
      as.integer(round(value / maximum * 255))
    } else {
      rep(0L, length(value))
    }
  })
  c(
    list(mode = "__gene_panels__"),
    cv_gene_panels_message(genes, scaled, round(maximum, 3))
  )
}

cv_rgb_message <- function(red, green, blue) {
  list(
    ok = TRUE,
    r = I(red$v),
    g = I(green$v),
    b = I(blue$v),
    genes = I(c(red$gene, green$gene, blue$gene))
  )
}

## Colour management changes labels, not cells or coordinates. Send that small
## delta separately so recolouring never rebuilds the per-dataset bundle.
cv_color_patch <- function(bundle, color_map = NULL) {
  patch_groups <- function(groups) {
    stats::setNames(
      lapply(names(groups), function(group_name) {
        group <- groups[[group_name]]
        colors <- as.character(group$colors)
        configured <- if (
          is.list(color_map) && group_name %in% names(color_map)
        ) {
          color_map[[group_name]]
        } else {
          NULL
        }
        if (!is.null(configured) && !is.null(names(configured))) {
          replacement <- unname(configured[match(
            group$levels,
            names(configured)
          )])
          present <- !is.na(replacement)
          colors[present] <- replacement[present]
        }
        I(colors)
      }),
      names(groups)
    )
  }
  patch <- list(
    dataset_id = bundle$dataset_id,
    groups = patch_groups(bundle$groups),
    cat_extra = patch_groups(bundle$cat_extra)
  )
  if (!is.null(bundle$dataset_context)) {
    patch$dataset_context <- bundle$dataset_context
  }
  patch
}

cv_apply_color_patch <- function(bundle, patch) {
  if (is.null(bundle) || !is.null(bundle$error) || !is.list(patch)) {
    return(bundle)
  }
  for (kind in intersect(c("groups", "cat_extra"), names(patch))) {
    for (group_name in intersect(names(bundle[[kind]]), names(patch[[kind]]))) {
      bundle[[kind]][[group_name]]$colors <- patch[[kind]][[group_name]]
    }
  }
  bundle
}
cv_space <- function(id, label, x, y) {
  list(id = id, label = label, x = I(x), y = I(y))
}

## Trajectories are ordinary linked spaces with one extra drawing layer: their
## fitted graph. Cells outside a trajectory stay in the shared index with NA
## coordinates, so a selection still has one identity across every panel.
cv_build_trajectories <- function(crb, cells) {
  methods <- tryCatch(
    crb$getMethodsForTrajectories(),
    error = function(e) character(0)
  )
  spaces <- list()
  edge_cols <- c(
    "source_dim_1",
    "source_dim_2",
    "target_dim_1",
    "target_dim_2"
  )
  for (method in methods) {
    trajectory_names <- tryCatch(
      crb$getNamesOfTrajectories(method),
      error = function(e) character(0)
    )
    for (trajectory_name in trajectory_names) {
      trajectory <- tryCatch(
        crb$getTrajectory(method, trajectory_name),
        error = function(e) NULL
      )
      meta <- trajectory$meta
      if (
        is.null(meta) ||
          !all(c("DR_1", "DR_2") %in% colnames(meta))
      ) {
        next
      }
      trajectory_cells <- rownames(meta)
      if ("cell_barcode" %in% colnames(meta)) {
        trajectory_cells <- as.character(meta$cell_barcode)
      }
      trajectory_cells <- cv_cell_ids(
        trajectory_cells,
        paste0("Trajectory `", trajectory_name, "`")
      )
      idx <- match(cells, trajectory_cells)
      if (!any(!is.na(idx))) {
        next
      }
      space <- cv_space(
        paste("trajectory", method, trajectory_name, sep = "::"),
        paste0(gsub("_", " ", trajectory_name, fixed = TRUE), " (trajectory)"),
        round(as.numeric(meta$DR_1)[idx], 3),
        round(as.numeric(meta$DR_2)[idx], 3)
      )
      space$trajectory <- TRUE
      space$method <- method
      space$name <- trajectory_name
      edges <- trajectory$edges
      if (!is.null(edges) && all(edge_cols %in% colnames(edges))) {
        edges <- edges[stats::complete.cases(edges[, edge_cols]), edge_cols]
        space$edges <- I(lapply(seq_len(nrow(edges)), function(i) {
          as.list(unname(as.numeric(edges[i, edge_cols])))
        }))
      } else {
        space$edges <- I(list())
      }
      spaces[[length(spaces) + 1L]] <- space
    }
  }
  spaces
}

## A continuous colouring. `v` is quantised to 0..scale (an integer vector keeps
## the bundle small); `min`/`max` carry the TRUE range so the client can render a
## real-valued colourbar and hover value instead of the quantised index. Trekker's
## own fields arrive pre-quantised to 0-255, hence the per-field `scale` rather
## than one global constant. Must match FIELD_PREFIX in www/cell_views.js.
cv_field_mode <- "__field__"
cv_field_scale <- 1000L
cv_field <- function(
  label,
  v,
  min,
  max,
  scale = cv_field_scale,
  source = NULL,
  desc = NULL,
  by_type = NULL
) {
  list(
    label = label,
    v = I(v),
    min = min,
    max = max,
    scale = scale,
    source = source,
    desc = desc,
    by_type = by_type
  )
}

cv_trekker_by_type <- function(value) {
  if (is.null(value) || !length(value)) {
    return(NULL)
  }
  if (is.list(value) && all(vapply(value, is.list, logical(1)))) {
    return(I(unname(value)))
  }
  labels <- names(value) %||% dimnames(value)[[1]]
  if (is.null(labels) || length(labels) != length(value)) {
    return(NULL)
  }
  I(Map(
    function(type, median) list(type = type, median = as.numeric(median)),
    as.character(labels),
    as.numeric(value)
  ))
}
cv_clone <- function(
  id,
  label,
  size,
  n_clones,
  n_receptor,
  receptor = NA_character_,
  n_cdr3 = NULL
) {
  ## id/label/size are arrays; n_clones/n_receptor are true scalars (left bare).
  ## The expansion bins travel WITH the data: the client re-lays-out the clone
  ## space between representations without a server round-trip, and a second
  ## copy of the thresholds in JavaScript is a second thing to keep in step with
  ## clone_contract.R. `tiers` are the upper bounds, `tier_labels` their names.
  list(
    id = I(id),
    label = I(label),
    size = I(size),
    ## How many distinct CDR3s each clone covers. 1 means the label names the
    ## clone exactly; more means it names the clone's dominant sequence, and the
    ## client has to say so rather than present it as the clonotype.
    n_cdr3 = I(if (is.null(n_cdr3)) rep(1L, length(size)) else n_cdr3),
    n_clones = n_clones,
    n_receptor = n_receptor,
    receptor = receptor,
    tiers = I(utils::head(CEREBRO_CLONE_BINS[-1], -1)),
    tier_labels = I(CEREBRO_CLONE_LABELS)
  )
}

## Categorical groupings: each md group column -> {values, levels, colors}.
cv_build_groups <- function(crb, md, colors_fn, only = NULL) {
  group_names <- tryCatch(crb$getGroups(), error = function(e) character(0))
  if (!is.null(only)) {
    group_names <- intersect(group_names, only)
  }
  groups <- list()
  for (g in group_names) {
    v <- md[[g]]
    if (is.null(v)) {
      next
    }
    lev <- if (is.factor(v)) levels(v) else sort(unique(as.character(v)))
    lev <- lev[!is.na(lev)]
    if (!length(lev)) {
      next
    }
    groups[[g]] <- cv_group(
      match(as.character(v), lev) - 1L,
      lev,
      colors_fn(g, lev)
    )
  }
  groups
}

## Encode numeric metadata as a continuous field for specialist payloads. The
## main Viewer Colour by controls deliberately do not call this helper: their
## metadata choices come only from registered groups.
cv_build_fields <- function(md, skip = "cell_barcode", only = NULL) {
  fields <- list()
  field_names <- if (is.null(only)) {
    colnames(md)
  } else {
    intersect(only, colnames(md))
  }
  for (mc in field_names) {
    if (mc %in% skip) {
      next
    }
    v <- md[[mc]]
    if (!is.numeric(v)) {
      next
    }
    rng <- suppressWarnings(range(v, na.rm = TRUE))
    if (!all(is.finite(rng)) || rng[2] <= rng[1]) {
      next
    }
    q <- as.integer(round((v - rng[1]) / (rng[2] - rng[1]) * cv_field_scale))
    fields[[paste0("meta:", mc)]] <- cv_field(
      mc,
      q,
      round(rng[1], 4),
      round(rng[2], 4)
    )
  }
  fields
}

## Encode unregistered categorical metadata for specialist payloads. The main
## Linked views bundle keeps `cat_extra` empty so Builder group registration is
## authoritative for metadata colouring.
cv_build_extra_groups <- function(md, group_names, colors_fn, only = NULL) {
  n <- nrow(md)
  max_levels <- max(2L, min(60L, as.integer(n / 2)))
  extra <- list()
  skipped <- list()
  extra_names <- if (is.null(only)) {
    colnames(md)
  } else {
    intersect(only, colnames(md))
  }
  for (mc in extra_names) {
    if (mc == "cell_barcode" || mc %in% group_names) {
      next
    }
    v <- md[[mc]]
    if (!(is.character(v) || is.factor(v) || is.logical(v))) {
      next
    }
    lev <- if (is.factor(v)) levels(v) else sort(unique(as.character(v)))
    lev <- lev[!is.na(lev)]
    if (!length(lev)) {
      next
    }
    if (length(lev) > max_levels) {
      skipped[[mc]] <- length(lev)
      next
    }
    extra[[mc]] <- cv_group(
      match(as.character(v), lev) - 1L,
      lev,
      colors_fn(mc, lev)
    )
  }
  list(groups = extra, skipped = skipped)
}

## Materialise exactly one deferred colour attribute. The first-frame transport
## carries descriptors for every metadata column, but choosing one must not
## rebuild projections, spatial images, clones, Trekker data, or gene names.
cv_build_attribute <- function(
  crb,
  md,
  kind,
  name,
  colors_fn = function(group_name, levels) cv_colors_for(levels)
) {
  if (
    !is.character(kind) ||
      length(kind) != 1L ||
      is.na(kind) ||
      !is.character(name) ||
      length(name) != 1L ||
      is.na(name) ||
      !nzchar(name)
  ) {
    return(NULL)
  }
  if (identical(kind, "groups")) {
    return(cv_build_groups(crb, md, colors_fn, only = name)[[name]])
  }
  if (identical(kind, "cat_extra")) {
    group_names <- tryCatch(crb$getGroups(), error = function(error) {
      character()
    })
    return(cv_build_extra_groups(
      md,
      group_names,
      colors_fn,
      only = name
    )$groups[[name]])
  }
  if (identical(kind, "fields")) {
    column <- sub("^meta:", "", name)
    return(cv_build_fields(md, only = column)[[name]])
  }
  NULL
}

## Every projection's coordinates travel in the bundle, keyed by name, so the
## expression panel can switch between UMAP / tSNE / PCA client-side with no
## server round-trip (the "one bundle per dataset, instant" contract).
##
## A 3-D embedding also sends its third dimension, so the client can orbit it
## rather than show a flattened shadow of it. `ndim` travels either way — the
## client needs to know which panels can rotate and which are flat.
cv_build_projections <- function(
  crb,
  cells,
  only = NULL,
  preloaded = NULL,
  canonical_projections = character(),
  aligned_cache = NULL
) {
  # Only unverified thin projections need canonical barcodes. Keep this lazy
  # so ordinary canonical first frames never hydrate the full metadata.
  canonical_cells <- NULL
  proj_names <- if (is.list(preloaded)) {
    names(preloaded)
  } else {
    tryCatch(crb$availableProjections(), error = function(e) NULL)
  }
  if (!is.null(only)) {
    proj_names <- intersect(proj_names, only)
  }
  projections <- list()
  for (pn in proj_names) {
    pj <- if (is.list(preloaded)) {
      preloaded[[pn]]
    } else {
      tryCatch(crb$getProjection(pn), error = function(e) NULL)
    }
    if (is.null(pj)) {
      next
    }
    aligned <- FALSE
    if (is.list(preloaded)) {
      if (nrow(pj) != length(cells)) {
        stop(
          "Preloaded projection does not match the canonical cell count.",
          call. = FALSE
        )
      }
      if (!pn %in% canonical_projections) {
        cached <- if (is.environment(aligned_cache)) {
          aligned_cache[[pn]]
        } else {
          NULL
        }
        if (is.null(cached) && is.null(canonical_cells)) {
          canonical_cells <- crb$getMetaData()$cell_barcode
        }
        pj <- if (!is.null(cached)) {
          cached
        } else {
          viewerAlignProjection(pj, canonical_cells)
        }
        if (is.null(cached) && is.environment(aligned_cache)) {
          aligned_cache[[pn]] <- pj
        }
      }
      # Reaching this branch means schema-verified or barcode-aligned, not
      # merely the same row count. `cells` is the thin frame's integer index.
      aligned <- TRUE
    }
    projection_cells <- if (aligned) {
      NULL
    } else {
      cv_cell_ids(rownames(pj), paste0("Projection `", pn, "`"))
    }
    aligned <- aligned || identical(cells, projection_cells)
    pjidx <- if (aligned) NULL else match(cells, projection_cells)
    coordinate <- function(index) {
      if (aligned) pj[, index] else pj[pjidx, index]
    }
    nd <- as.integer(ncol(pj))
    entry <- list(
      x = I(round(as.numeric(coordinate(1L)), 4)),
      y = I(round(as.numeric(coordinate(2L)), 4)),
      ndim = nd
    )
    ## I() so a single-cell data set still serialises z as an array, the same
    ## invariant cv_space() enforces for x/y.
    if (nd >= 3) {
      entry$z <- I(round(as.numeric(coordinate(3L)), 4))
      ## The three axis names, for the tripod the client draws on a rotated
      ## cloud. Its own column names rather than a generic X/Y/Z: on a PCA those
      ## carry which components are being shown, which is the whole question
      ## when only three of many are drawn.
      ax <- colnames(pj)[1:3]
      entry$axes <- I(
        if (is.null(ax) || anyNA(ax)) {
          paste0("dim ", 1:3)
        } else {
          as.character(ax)
        }
      )
    }
    projections[[pn]] <- entry
  }
  projections
}

## One spatial SAMPLE: its per-cell x/y (aligned to `cells`, NA off-sample) plus
## its histology image. Two image sources, ONE contract: an image reference + the
## data-space bounds it covers + an alignment preset (offset in DATA units, scale
## unitless, flip). The client maps the bounds to screen with the same transform
## as the cells, so the image aligns; preset/user transforms adjust on top.
##   - EMBEDDED (Xenium/MERFISH): image + its own bounds travel in the .crb.
##   - EXTERNAL (Visium H&E): separate files configured for this exact dataset
##     and FOV, with an optional per-image alignment preset and explicit bounds.
## Returns list(name, x, y, image) or NULL.
cv_spatial_data <- function(crb, nm) {
  getter <- crb$getSpatialData
  if ("hydrate_molecules" %in% names(formals(getter))) {
    return(crb$getSpatialData(nm, hydrate_molecules = FALSE))
  }
  stored <- tryCatch(crb$spatial[[nm]], error = function(error) NULL)
  if (
    is.list(stored) &&
      inherits(stored[["molecules"]], "CerebroSpatialMoleculeRef")
  ) {
    return(stored)
  }
  getter(nm)
}

cv_spatial_one <- function(crb, cells, nm, allow_external, roi_value = NULL,
                           metadata = NULL, spatial_data = NULL) {
  sd <- tryCatch(
    spatial_data %||% cv_spatial_data(crb, nm),
    error = function(e) NULL
  )
  co <- if (!is.null(sd)) sd$coordinates else NULL
  if (is.null(co)) {
    return(NULL)
  }
  rotation <- spatialPlotRotation(
    if (exists("Cerebro.options")) Cerebro.options else NULL,
    cv_selected_dataset_name(),
    nm
  )
  if (is.null(metadata)) {
    metadata <- tryCatch(crb$getMetaData(), error = function(error) NULL)
  }
  context <- spatial_roi_transform_context(co, metadata, degrees = rotation)
  rois <- unique(as.character(context$roi_by_cell))
  rois <- rois[!is.na(rois) & nzchar(rois)]
  ## A single-ROI FOV has the same default scene as Spatial's single-ROI view.
  ## Multi-ROI overview coordinates stay intact; separately selectable ROI
  ## scenes below use the saved rotations and their own image declarations.
  if (is.null(roi_value)) roi_value <- if (length(rois) == 1L) rois else ""
  roi_value <- spatial_roi_value(roi_value)
  if (nzchar(roi_value)) {
    keep <- !is.na(context$roi_by_cell) & context$roi_by_cell == roi_value
    co <- co[keep, , drop = FALSE]
    if (!nrow(co)) return(NULL)
    co <- rotateSpatialCoordinatesByRoi(
      co, as.character(context$roi_by_cell[rownames(co)]),
      spatialRoiSettings(
        if (exists("Cerebro.options")) Cerebro.options else NULL,
        cv_selected_dataset_name(), nm
      ), rotation, pivots = context$pivots
    )
  } else {
    co <- rotateSpatialCoordinates(co, rotation)
  }
  spatial_cells <- cv_cell_ids(
    rownames(co),
    paste0("Spatial section `", nm, "`")
  )
  sidx <- match(cells, spatial_cells)
  xr <- range(co[, 1], na.rm = TRUE)
  yr <- range(co[, 2], na.rm = TRUE)
  ## Every background this section can be shown against, as objects with their
  ## own identity and calibration -- not one image tucked into the section.
  ## Embedded and external used to be exclusive, so an object carrying its own
  ## histology silently dropped whatever the deployment had configured, and only
  ## the first configured file was read at all.
  bounds_default <- list(
    xmin = xr[1],
    xmax = xr[2],
    ymin = yr[1],
    ymax = yr[2]
  )
  span <- c(diff(xr), diff(yr))
  images <- list()
  ## Use Spatial's canonical histology_images/legacy-image normalization and
  ## ROI precedence, so both pages offer the same set of eligible images.
  embedded <- embedded_spatial_images(sd, roi_value)
  alignment <- sd[["histology_alignment", exact = TRUE]]
  appearance <- cv_alignment_appearance(alignment)
  section_appearance <- spatialPointAppearance(
    if (exists("Cerebro.options")) Cerebro.options else NULL,
    cv_selected_dataset_name(),
    nm
  )
  roi_appearance <- spatialRoiSetting(
    if (exists("Cerebro.options")) Cerebro.options else NULL,
    cv_selected_dataset_name(), nm, roi_value
  )
  if (!is.null(roi_appearance)) {
    section_appearance <- roi_appearance[c("point_opacity", "point_size")]
  }
  if (is.null(section_appearance) && isTRUE(alignment$builder_managed)) {
    if (
      length(appearance$point_opacity) == 1L &&
        length(appearance$point_size) == 1L
    ) {
      section_appearance <- appearance[c("point_opacity", "point_size")]
    }
  }
  spatial_defaults <- viewerScatterDefaults(
    if (exists("Cerebro.options")) Cerebro.options else list(),
    cv_selected_dataset_name(), "spatial")
  for (embedded_index in seq_along(embedded)) {
    entry <- embedded[[embedded_index]]
    embedded_names <- names(embedded)
    entry_name <- if (
      !is.null(embedded_names) && length(embedded_names) >= embedded_index
    ) {
      embedded_names[[embedded_index]]
    } else {
      ""
    }
    if (is.list(entry)) {
      emb <- entry$histology_image %||%
        entry$image %||%
        entry$uri %||%
        entry$data
      b <- entry$histology_image_bounds %||%
        entry$bounds %||%
        sd[["histology_image_bounds", exact = TRUE]]
      label <- entry$image_label %||% entry$label %||% entry_name
    } else {
      emb <- entry
      b <- sd[["histology_image_bounds", exact = TRUE]]
      label <- entry_name
    }
    if (!is.character(emb) || length(emb) != 1L || is.na(emb) || !nzchar(emb)) {
      next
    }
    if (is.null(b)) {
      b <- bounds_default
    }
    if (is.null(label) || !length(label) || is.na(label) || !nzchar(label)) {
      label <- if (length(embedded) == 1L) {
        "Embedded histology"
      } else {
        paste("Embedded histology", embedded_index)
      }
    }
    preset <- cv_image_preset(nm, entry_name)
    entry_alignment <- if (is.list(entry)) {
      entry$histology_alignment %||% entry$alignment
    } else {
      NULL
    }
    preset <- spatialEmbeddedImagePreset(preset, entry_alignment)
    ## A section-level alignment names the image it was saved for.
    alignment_source <- if (is.list(alignment)) {
      as.character(alignment$source %||% character())
    } else {
      character()
    }
    image_opacity <- appearance$image_opacity
    if (
      length(alignment_source) == 1L &&
        !is.na(alignment_source) &&
        identical(label, basename(alignment_source)) &&
        length(image_opacity) == 1L
    ) {
      preset$opacity <- unname(image_opacity)
    }
    ## Settings may target embedded labels too. This is the same public
    ## per-image contract createShinyApp() validates for external backgrounds.
    images[[length(images) + 1]] <- list(
      id = if (embedded_index == 1L) {
        "embedded"
      } else {
        paste0("embedded-", embedded_index)
      },
      label = label,
      uri = emb,
      bounds = list(
        xmin = as.numeric(b[["xmin"]]),
        xmax = as.numeric(b[["xmax"]]),
        ymin = as.numeric(b[["ymin"]]),
        ymax = as.numeric(b[["ymax"]])
      ),
      viewport = cv_viewport_bounds(entry_alignment$viewport_bounds),
      preset = preset,
      coord_span = span
    )
  }
  if (allow_external) {
    for (ex in cv_external_images(nm, defer = TRUE, roi_value = roi_value)) {
      images[[length(images) + 1]] <- list(
        id = ex$id,
        label = ex$label,
        uri = ex$uri,
        asset_path = ex$asset_path,
        asset_mime = ex$asset_mime,
        bounds = ex$bounds %||% bounds_default,
        viewport = ex$viewport,
        preset = ex$preset,
        coord_span = span
      )
    }
  }
  ## The default background, as a REFERENCE rather than a copy. A histology
  ## image reference can point to a multi-megabyte payload; carrying the same one
  ## under both `image` and
  ## `images[1]` doubled it, and once more again for the space's own default --
  ## measured at 1.6 MB per copy on the Xenium demo, in a 3.6 MB bundle. Older
  ## readers of the singular field get the id and can look it up.
  image <- if (length(images)) {
    list(
      id = images[[1]]$id,
      label = images[[1]]$label,
      bounds = images[[1]]$bounds,
      viewport = images[[1]]$viewport,
      preset = images[[1]]$preset,
      coord_span = images[[1]]$coord_span
    )
  } else {
    NULL
  }

  viewport <- if (length(images)) images[[1L]]$viewport else NULL
  list(
    name = nm,
    label = paste0(nm, if (nzchar(roi_value)) {
      paste0(" · ", roi_value)
    } else if (length(rois) > 1L) {
      " · All ROIs"
    } else {
      ""
    }, " (spatial)"),
    roi = if (nzchar(roi_value)) roi_value else NULL,
    is_roi_overview = !nzchar(roi_value) && length(rois) > 1L,
    x = as.numeric(co[sidx, 1]),
    y = as.numeric(co[sidx, 2]),
    x_range = if (is.null(viewport)) {
      NULL
    } else {
      unname(unlist(viewport[c(
        "xmin",
        "xmax"
      )]))
    },
    y_range = if (is.null(viewport)) {
      NULL
    } else {
      unname(unlist(viewport[c(
        "ymin",
        "ymax"
      )]))
    },
    builder_point_opacity = section_appearance$point_opacity %||% spatial_defaults$point_opacity,
    builder_point_size = section_appearance$point_size %||% spatial_defaults$point_size,
    builder_percentage_cells = spatial_defaults$percentage_cells_to_show,
    ## `image` is the default one, kept so anything reading the older singular
    ## contract still works; `images` is the list the picker is built from.
    image = image,
    images = images
  )
}

## Standard spatial space (id "spatial"). Its default coords/image are the first
## sample; when the object carries MORE than one spatial sample, every sample also
## travels in `$samples` so Linked views can switch between them client-side (the
## "Spatial data" picker), each donor's tissue section being its own coordinate
## system + image. Returns the space or NULL when there is no spatial.
cv_build_spatial <- function(crb, cells) {
  sp_names <- tryCatch(crb$availableSpatial(), error = function(e) NULL)
  if (!length(sp_names)) {
    return(NULL)
  }
  metadata <- tryCatch(crb$getMetaData(), error = function(error) NULL)
  built <- list()
  for (nm in sp_names) {
    sd <- tryCatch(cv_spatial_data(crb, nm), error = function(error) NULL)
    if (is.null(sd)) next
    scene <- cv_spatial_one(crb, cells, nm, TRUE, metadata = metadata, spatial_data = sd)
    if (is.null(scene)) next
    built[[length(built) + 1L]] <- scene
    rois <- spatial_metadata_facet(
      metadata, rownames(sd$coordinates),
      c("sample_roi", "roi", "roi_id", "region_of_interest")
    )$values
    if (length(rois) > 1L) {
      for (roi in rois) {
        scene <- cv_spatial_one(crb, cells, nm, TRUE, roi, metadata, sd)
        if (is.null(scene)) next
        scene$name <- paste0(nm, "::roi::", roi)
        built[[length(built) + 1L]] <- scene
      }
    }
  }
  built <- Filter(Negate(is.null), built)
  if (!length(built)) {
    return(NULL)
  }
  first <- built[[1]]
  space <- cv_space(
    "spatial",
    first$label,
    first$x,
    first$y
  )
  ## Labels describe FOV/ROI scope; the persisted selection key stays stable.
  space$name <- first$name
  space$is_roi_overview <- first$is_roi_overview
  if (!is.null(first$image)) {
    space$image <- first$image
  }
  if (!is.null(first$x_range)) {
    space$x_range <- I(first$x_range)
    space$y_range <- I(first$y_range)
  }
  space$builder_point_opacity <- first$builder_point_opacity
  space$builder_point_size <- first$builder_point_size
  space$builder_percentage_cells <- first$builder_percentage_cells
  ## Only when there is no `samples` list to hold them: with one, the space's
  ## default section IS samples[[1]] and repeating its images here would send
  ## every one of them twice.
  if (length(first$images) && length(built) == 1) {
    space$images <- I(first$images)
  }
  if (length(built) > 1) {
    space$samples <- lapply(built, function(s) {
      list(
        name = s$name,
        is_roi_overview = s$is_roi_overview,
        label = s$label %||% paste0(s$name, " (spatial)"),
        x = I(s$x),
        y = I(s$y),
        x_range = if (is.null(s$x_range)) NULL else I(s$x_range),
        y_range = if (is.null(s$y_range)) NULL else I(s$y_range),
        builder_point_opacity = s$builder_point_opacity,
        builder_point_size = s$builder_point_size,
        builder_percentage_cells = s$builder_percentage_cells,
        image = s$image,
        images = I(s$images)
      )
    })
  }
  space
}

## Trekker single-cell spatial mapping: its physical coordinates live in the
## `trekker` slot, not `spatial`, so availableSpatial() misses them. Exposed as
## its OWN space (id "trekker"), distinct from a standard `spatial` space, so a
## data set carrying BOTH keeps both — the right panel switches between them
## rather than one silently swallowing the other. Aligned to `cells` via barcodes
## (NA where a cell was not positioned). Returns list(space, bundle) or NULL.
cv_build_trekker <- function(crb, cells, md) {
  tk <- tryCatch(crb$getTrekker(), error = function(e) NULL)
  if (
    is.null(tk) ||
      is.null(tk$x) ||
      is.null(tk$y) ||
      is.null(tk$barcodes)
  ) {
    return(NULL)
  }
  trekker_cells <- cv_cell_ids(tk$barcodes, "Trekker data")
  tk_idx <- match(cells, trekker_cells)
  if (!any(!is.na(tk_idx))) {
    return(NULL)
  }
  space <- cv_space(
    "trekker",
    "Physical (Trekker)",
    round(as.numeric(tk$x)[tk_idx], 2),
    round(as.numeric(tk$y)[tk_idx], 2)
  )
  alignment <- tk[["histology_alignment", exact = TRUE]]
  appearance <- cv_alignment_appearance(alignment)
  external_histology <- viewerTrekkerExternalImage()
  histology <- external_histology$uri %||%
    tk[["histology_image", exact = TRUE]]
  if (
    is.character(histology) &&
      length(histology) == 1L &&
      !is.na(histology) &&
      nzchar(histology)
  ) {
    positioned_x <- as.numeric(tk$x)[tk_idx]
    positioned_y <- as.numeric(tk$y)[tk_idx]
    xr <- suppressWarnings(range(positioned_x, na.rm = TRUE))
    yr <- suppressWarnings(range(positioned_y, na.rm = TRUE))
    if (!all(is.finite(xr)) || diff(xr) <= 0) {
      finite_x <- positioned_x[is.finite(positioned_x)]
      centre <- if (length(finite_x)) finite_x[[1L]] else 0
      xr <- c(centre - 0.5, centre + 0.5)
    }
    if (!all(is.finite(yr)) || diff(yr) <= 0) {
      finite_y <- positioned_y[is.finite(positioned_y)]
      centre <- if (length(finite_y)) finite_y[[1L]] else 0
      yr <- c(centre - 0.5, centre + 0.5)
    }
    bounds <- external_histology$bounds %||%
      tk[["histology_image_bounds", exact = TRUE]]
    required_bounds <- c("xmin", "xmax", "ymin", "ymax")
    valid_bounds <- !is.null(bounds) &&
      !is.null(names(bounds)) &&
      all(required_bounds %in% names(bounds))
    if (valid_bounds) {
      bounds <- suppressWarnings(as.numeric(bounds[required_bounds]))
      valid_bounds <- length(bounds) == 4L &&
        !anyNA(bounds) &&
        all(is.finite(bounds)) &&
        bounds[[1L]] < bounds[[2L]] &&
        bounds[[3L]] < bounds[[4L]]
    }
    if (!valid_bounds) {
      bounds <- c(xr[[1L]], xr[[2L]], yr[[1L]], yr[[2L]])
    }
    names(bounds) <- required_bounds
    source <- if (is.list(alignment)) {
      as.character(alignment[["source"]] %||% character())
    } else {
      character()
    }
    label <- if (
      is.character(external_histology$label) &&
        length(external_histology$label) == 1L &&
        !is.na(external_histology$label) &&
        nzchar(external_histology$label)
    ) {
      external_histology$label
    } else if (length(source) == 1L && !is.na(source) && nzchar(source)) {
      basename(source)
    } else {
      "Trekker background"
    }
    preset <- external_histology$preset %||%
      cv_image_preset("trekker", label)
    if (is.null(external_histology) && length(appearance$image_opacity) == 1L) {
      preset$opacity <- appearance$image_opacity
    }
    image_entry <- list(
      id = if (is.null(external_histology)) {
        "trekker-embedded"
      } else {
        "trekker-external"
      },
      label = label,
      uri = histology,
      bounds = as.list(bounds),
      viewport = cv_viewport_bounds(external_histology$viewport_bounds),
      preset = preset,
      coord_span = c(diff(xr), diff(yr))
    )
    space$image <- image_entry[c(
      "id",
      "label",
      "bounds",
      "viewport",
      "preset",
      "coord_span"
    )]
    space$images <- I(list(image_entry))
    space$background_scope <- "Trekker"
  }
  ## Bring the Trekker page's non-metadata controls into Linked views: continuous
  ## physical fields to colour by, per-cell positioning confidence (dissolve),
  ## and a positioning-evidence flag (nuclei markers). All aligned to `cells`;
  ## positioned-only fields are NA where unpositioned. Numeric metadata columns
  ## are deliberately not added to the Linked views Colour by picker.
  flds <- list()
  for (fn in names(tk$fields)) {
    f <- tk$fields[[fn]]
    if (is.null(f$v)) {
      next
    }
    ## Trekker's own fields arrive pre-quantised to 0-255.
    flds[[fn]] <- cv_field(
      f$label %||% fn,
      as.integer(f$v)[tk_idx],
      f$min %||% 0,
      f$max %||% 1,
      scale = 255L,
      source = "trekker",
      desc = f$desc %||% NULL,
      by_type = cv_trekker_by_type(f$by_type %||% NULL)
    )
  }
  conf_v <- if (!is.null(tk$conf) && !is.null(tk$conf$prop_top)) {
    I(round(as.numeric(tk$conf$prop_top)[tk_idx], 3))
  } else {
    NULL
  }
  ## The rest of what Trekker recorded about a position, per cell. `conf` stays a
  ## bare vector because the dissolve slider indexes it directly; these travel
  ## beside it. Without them the workspace could say how confident a placement
  ## was but not how noisy the beads under it were or how many spatial barcodes
  ## it rested on -- the two numbers the dedicated page shows next to it, and the
  ## ones that say whether the confidence is worth anything.
  conf_extra <- list()
  if (!is.null(tk$conf)) {
    for (k in c("prop_noise", "sb_total", "sb_umi_top")) {
      v <- tk$conf[[k]]
      if (is.null(v)) {
        next
      }
      conf_extra[[k]] <- I(round(as.numeric(v)[tk_idx], 4))
    }
  }
  ev_flag <- NULL
  ev_img <- NULL
  if (length(tk$evidence)) {
    ev_bc <- vapply(
      tk$evidence,
      function(e) e$bc %||% "",
      character(1)
    )
    ev_flag <- I(as.integer(cells %in% ev_bc))
    ## Keep the evidence aligned to the bundle's cell order. Most entries are
    ## NULL (the vendor ships images for only a small subset), so this adds the
    ## actual explanation to the detail card without duplicating barcodes or
    ## forcing a server round-trip for every click.
    ev_img <- vector("list", length(cells))
    for (e in tk$evidence) {
      at <- match(as.character(e$bc %||% ""), cells)
      img <- e$img %||% NULL
      if (!is.na(at) && !is.null(img) && nzchar(img)) {
        ev_img[at] <- list(as.character(img))
      }
    }
    ev_img <- I(ev_img)
  }
  bundle <- list(
    conf = conf_v,
    conf_noise = conf_extra$prop_noise,
    conf_sb = conf_extra$sb_total,
    conf_sb_umi = conf_extra$sb_umi_top,
    evidence = ev_flag,
    evidence_img = ev_img,
    ## Dataset-level (not per-cell, no `tk_idx` re-indexing needed): the
    ## same coordinate-source / QC / Moran's I detail the Trekker page
    ## shows, surfaced here via a modal (see cell_views.js `cv-tk-info-btn`).
    qc = tk$qc,
    moran = tk$moran
  )
  ## `fields` goes to the bundle's TOP-LEVEL field list, not into $trekker: the
  ## client reads one list of continuous colourings regardless of where each came
  ## from, so there is a single place to add, look up and render them.
  list(space = space, bundle = bundle, fields = flds)
}

## Immune axis: clone identity, sizes, ranks, a clone "space", expansion level.
## Returns list(space, group, bundle) or NULL when there is no receptor data.
cv_build_clone_rows <- function(
  clone,
  ctaa,
  receptor,
  receptor_index,
  n,
  sparse
) {
  valid <- !is.na(clone) &
    nzchar(clone) &
    !is.na(receptor_index) &
    receptor_index >= 1L &
    receptor_index <= n
  clone <- clone[valid]
  ctaa <- ctaa[valid]
  receptor_index <- as.integer(receptor_index[valid])
  if (!length(clone)) {
    return(NULL)
  }
  unordered_keys <- unique(clone)
  unordered_size <- tabulate(
    match(clone, unordered_keys),
    nbins = length(unordered_keys)
  )
  clone_order <- order(-unordered_size, unordered_keys, method = "radix")
  clone_keys <- unordered_keys[clone_order]
  clone_size <- unordered_size[clone_order]
  cell_clone <- match(clone, clone_keys)
  K <- length(clone_keys)
  clone_cdr3 <- rep(NA_integer_, K)
  clone_label <- clone_keys
  valid_aa <- !is.na(ctaa) & nzchar(ctaa)
  if (any(valid_aa)) {
    aa_clone <- cell_clone[valid_aa]
    aa_value <- ctaa[valid_aa]
    aa_order <- order(aa_clone, aa_value, method = "radix")
    aa_clone <- aa_clone[aa_order]
    aa_value <- aa_value[aa_order]
    pair_start <- c(
      TRUE,
      aa_clone[-1L] != aa_clone[-length(aa_clone)] |
        aa_value[-1L] != aa_value[-length(aa_value)]
    )
    pair_at <- which(pair_start)
    pair_count <- diff(c(pair_at, length(aa_clone) + 1L))
    pair_clone <- aa_clone[pair_at]
    pair_aa <- aa_value[pair_at]
    clone_cdr3 <- tabulate(pair_clone, nbins = K)
    clone_cdr3[clone_cdr3 == 0L] <- NA_integer_
    dominant_order <- order(
      pair_clone,
      -pair_count,
      pair_aa,
      method = "radix"
    )
    dominant <- dominant_order[!duplicated(pair_clone[dominant_order])]
    clone_label[pair_clone[dominant]] <- pair_aa[dominant]
  }
  space <- list(
    id = "clone",
    label = if (is.na(receptor)) {
      "Clonal expansion"
    } else {
      paste0("Clonal expansion (", receptor, ")")
    }
  )
  expansion <- as.character(cerebro_clone_expansion(clone_size[cell_clone]))
  lev <- c("No receptor", CEREBRO_CLONE_LABELS)
  expansion_codes <- match(expansion, lev) - 1L
  if (isTRUE(sparse)) {
    group <- list(
      clone_index = TRUE,
      values = I(as.integer(expansion_codes)),
      default = 0L,
      levels = I(lev),
      colors = I(c(
        "#e0e0e0",
        "#c6dbef",
        "#6baed6",
        "#f97316",
        "#c2410c",
        "#7f1d1d"
      ))
    )
    bundle <- cv_clone(
      integer(),
      unname(clone_label),
      clone_size,
      K,
      length(receptor_index),
      receptor,
      unname(clone_cdr3)
    )
    bundle$index <- I(receptor_index - 1L)
    bundle$code <- I(as.integer(cell_clone - 1L))
    bundle$id <- NULL
  } else {
    dense_clone <- rep(-1L, n)
    dense_clone[receptor_index] <- cell_clone - 1L
    dense_expansion <- rep(0L, n)
    dense_expansion[receptor_index] <- expansion_codes
    group <- cv_group(
      dense_expansion,
      lev,
      c("#e0e0e0", "#c6dbef", "#6baed6", "#f97316", "#c2410c", "#7f1d1d")
    )
    bundle <- cv_clone(
      dense_clone,
      unname(clone_label),
      clone_size,
      K,
      length(receptor_index),
      receptor,
      unname(clone_cdr3)
    )
  }
  list(space = space, group = group, bundle = bundle)
}

## Clone labels are needed only after a user selects or inspects a clone. Keep
## the panel geometry in the first clone message and retain these strings on the
## server for small, indexed follow-up requests.
cv_defer_clone_details <- function(clone) {
  if (is.null(clone) || !is.list(clone$bundle)) {
    return(list(value = clone, details = NULL))
  }
  details <- list(
    label = as.character(clone$bundle$label),
    n_cdr3 = as.integer(clone$bundle$n_cdr3)
  )
  clone$bundle$label <- I(character())
  clone$bundle$n_cdr3 <- I(integer())
  clone$bundle$details_deferred <- TRUE
  list(value = clone, details = details)
}

cv_build_clone <- function(crb, cells, n, sparse = FALSE) {
  pack <- attr(crb, "cerebro_viewer_pack", exact = TRUE)
  receptors <- if (is.list(pack)) {
    as.character(pack$manifest$immune_receptors)
  } else {
    character()
  }
  packed <- if (
    length(receptors) && exists("viewerPackImmuneIndex", mode = "function")
  ) {
    viewerPackImmuneIndex(pack, receptors[[1L]])
  } else {
    NULL
  }
  canonical_packed <- !is.null(packed) &&
    (isTRUE(pack$canonical_order) &&
      identical(cells, seq_len(n)) ||
      length(pack$cells) == n &&
        (identical(cells, seq_len(n)) ||
          identical(as.character(cells), pack$cells)))
  if (isTRUE(sparse) && canonical_packed) {
    return(cv_build_clone_rows(
      as.character(packed$clone),
      as.character(packed$ctaa),
      as.character(packed$receptor),
      as.integer(packed$cell_index),
      n,
      TRUE
    ))
  }
  cp <- if (!is.null(packed) && length(pack$cells) == n) {
    clone <- rep(NA_character_, length(cells))
    ctaa <- rep(NA_character_, length(cells))
    at <- match(pack$cells[packed$cell_index], cells)
    keep <- !is.na(at)
    clone[at[keep]] <- packed$clone[keep]
    ctaa[at[keep]] <- packed$ctaa[keep]
    list(clone = clone, ctaa = ctaa, receptor = packed$receptor)
  } else {
    ir <- tryCatch(crb$getImmuneRepertoire(), error = function(e) NULL)
    cv_clone_per_cell(ir, cells)
  }
  if (is.null(cp) || !any(!is.na(cp$clone))) {
    return(NULL)
  }
  receptor_index <- which(!is.na(cp$clone) & nzchar(cp$clone))
  cv_build_clone_rows(
    as.character(cp$clone[receptor_index]),
    as.character(cp$ctaa[receptor_index]),
    as.character(cp$receptor),
    receptor_index,
    n,
    sparse
  )
}

cv_build_primary_colours <- function(
  crb,
  md,
  group_names,
  colors_fn,
  primary_group_resource = NULL
) {
  parameters <- tryCatch(crb$getParameters(), error = function(e) list())
  colour_groups <- viewerColourGroupChoices(
    md,
    group_names,
    parameters[["main_group"]]
  )
  group_candidates <- colour_groups$choices
  default_group <- colour_groups$selected
  groups <- list()
  cat_extra <- list()
  fields <- list()
  resource_kind <- primary_group_resource$kind
  resource_name <- primary_group_resource$name
  resource_levels <- as.character(
    primary_group_resource$levels %||% character()
  )
  resource_descriptor <- primary_group_resource$descriptor
  use_resource <-
    is.character(resource_name) &&
    length(resource_name) == 1L &&
    !is.na(resource_name) &&
    identical(resource_name, default_group) &&
    resource_kind %in% c("groups", "cat_extra") &&
    length(resource_levels) &&
    is.list(resource_descriptor)
  if (use_resource) {
    group <- list(
      levels = I(resource_levels),
      colors = I(colors_fn(default_group, resource_levels)),
      values_resource = resource_descriptor
    )
    if (identical(resource_kind, "groups")) {
      groups[[default_group]] <- group
    } else {
      cat_extra[[default_group]] <- group
    }
    return(list(
      groups = groups,
      cat_extra = cat_extra,
      cat_skipped = list(),
      fields = fields,
      default_group = default_group
    ))
  }
  for (name in unique(c(default_group, group_candidates))) {
    candidate <- cv_build_groups(crb, md, colors_fn, name)
    if (length(candidate)) {
      groups <- candidate
      default_group <- names(candidate)[[1L]]
      break
    }
  }

  list(
    groups = groups,
    cat_extra = cat_extra,
    cat_skipped = list(),
    fields = fields,
    default_group = default_group
  )
}

cv_primary_group_resource <- function(crb, md, resource_fn) {
  if (!is.data.frame(md) || !is.function(resource_fn)) {
    return(NULL)
  }
  group_names <- tryCatch(crb$getGroups(), error = function(error) character())
  group_candidates <- intersect(group_names, colnames(md))
  parameters <- tryCatch(crb$getParameters(), error = function(error) list())
  name <- viewerColourGroupChoices(
    md,
    group_candidates,
    parameters[["main_group"]]
  )$selected
  value <- if (!is.null(name)) md[[name]] else NULL
  if (!(is.factor(value) || is.character(value) || is.logical(value))) {
    return(NULL)
  }
  resource <- tryCatch(
    resource_fn(name, if (is.factor(value)) levels(value) else NULL),
    error = function(error) NULL
  )
  levels <- as.character(resource$levels %||% character())
  if (!is.list(resource) || !length(levels)) {
    return(NULL)
  }
  resource$levels <- NULL
  list(
    name = name,
    kind = if (name %in% group_candidates) "groups" else "cat_extra",
    levels = I(levels),
    descriptor = resource
  )
}

cv_progressive_modalities <- function(crb, projection_names) {
  pack <- attr(crb, "cerebro_viewer_pack", exact = TRUE)
  manifest <- if (is.list(pack)) pack$manifest else NULL
  if (!is.list(manifest)) {
    return("all")
  }
  capabilities <- manifest$capabilities %||% list()
  c(
    "attributes",
    if (length(projection_names) > 1L) "projections",
    names(capabilities)[
      vapply(capabilities, isTRUE, logical(1)) &
        names(capabilities) %in% c("trajectory", "spatial", "trekker")
    ],
    if (length(manifest$immune_receptors %||% character())) "clone"
  )
}

cv_preferred_projection_name <- function(projection_names, configured = NULL) {
  projection_names <- as.character(projection_names)
  if (!length(projection_names)) {
    return(NULL)
  }
  if (
    is.character(configured) &&
      length(configured) == 1L &&
      !is.na(configured) &&
      configured %in% projection_names
  ) {
    configured
  } else if ("umap" %in% projection_names) {
    "umap"
  } else {
    projection_names[[1L]]
  }
}

cv_resource_projection <- function(resource, projection, n) {
  if (
    !is.list(resource) ||
      !is.character(resource$name) ||
      length(resource$name) != 1L ||
      is.na(resource$name) ||
      !nzchar(resource$name) ||
      !is.list(resource$descriptor) ||
      length(resource$descriptor$cells) != 1L ||
      is.na(resource$descriptor$cells) ||
      !identical(as.integer(resource$descriptor$cells), as.integer(n)) ||
      length(resource$descriptor$dimensions) != 1L ||
      is.na(resource$descriptor$dimensions) ||
      !(as.integer(resource$descriptor$dimensions) %in% c(2L, 3L))
  ) {
    return(NULL)
  }
  dimensions <- as.integer(resource$descriptor$dimensions)
  entry <- list(
    ndim = dimensions,
    projection_resource = resource$descriptor
  )
  if (dimensions == 3L) {
    axes <- colnames(projection)[seq_len(3L)]
    entry$axes <- I(
      if (is.null(axes) || length(axes) != 3L || anyNA(axes)) {
        paste0("dim ", seq_len(3L))
      } else {
        as.character(axes)
      }
    )
  }
  setNames(list(entry), resource$name)
}

## Assemble the bundle from the loaded Cerebro object. Each modality is built by
## its own cv_build_* helper; this function wires them into the final list.
cv_build_bundle <- function(
  crb,
  primary_only = FALSE,
  first_frame = NULL,
  primary_projection_resource = NULL,
  primary_group_resource = NULL,
  dataset_context = NULL
) {
  use_first_frame <- isTRUE(primary_only) &&
    is.list(first_frame) &&
    is.data.frame(first_frame$meta_data) &&
    is.list(first_frame$projections)
  md <- if (use_first_frame) {
    first_frame$meta_data
  } else {
    cv_canonical_metadata(crb$getMetaData())
  }
  if (is.null(md)) {
    return(NULL)
  }
  cells <- if (use_first_frame) seq_len(nrow(md)) else md$cell_barcode
  n <- length(cells)

  ## Seed a stable fallback here. The user-editable palette travels separately
  ## as cv_color_patch(), so changing one colour cannot rebuild this bundle.
  cv_group_colors <- function(group_name, lev) {
    tryCatch(
      cerebro_group_colors(length(lev)),
      error = function(e) cv_colors_for(lev)
    )
  }

  parameters <- tryCatch(crb$getParameters(), error = function(e) NULL)
  colour_groups <- viewerColourGroupChoices(
    md,
    tryCatch(crb$getGroups(), error = function(e) character()),
    if (is.list(parameters)) parameters[["main_group"]] else NULL
  )
  group_names <- colour_groups$choices
  if (isTRUE(primary_only)) {
    primary_colours <- cv_build_primary_colours(
      crb,
      md,
      group_names,
      cv_group_colors,
      primary_group_resource
    )
    groups <- primary_colours$groups
    cat_extra <- primary_colours$cat_extra
    cat_skipped <- primary_colours$cat_skipped
    fields <- primary_colours$fields
    default_group <- primary_colours$default_group
  } else {
    groups <- cv_build_groups(crb, md, cv_group_colors)
    cat_extra <- list()
    cat_skipped <- list()
    fields <- list()
    default_group <- colour_groups$selected
  }

  ## Every modality is independently useful. Linked views adds a coordinated
  ## workspace without changing the dedicated Projection/Spatial/Trekker pages.
  viewer_content <- cv_selected_viewer_content()
  initial_projections <- character()
  appearance <- if (exists("current_scatter_defaults", mode = "function")) {
    shiny::isolate(current_scatter_defaults())
  } else {
    viewerScatterDefaults(if (exists("Cerebro.options")) Cerebro.options else list(),
      cv_selected_dataset_name())
  }
  default_point_size <- appearance$point_size
  default_percentage_cells_to_show <- appearance$percentage_cells_to_show
  default_point_opacity <- appearance$point_opacity
  projection_names <- if (use_first_frame) {
    names(first_frame$projections)
  } else {
    tryCatch(crb$availableProjections(), error = function(e) character())
  }
  configured_projection <- viewer_content[["default_projection"]]
  preferred_projection <- cv_preferred_projection_name(
    projection_names,
    configured_projection
  )
  projections <- if (isTRUE(primary_only)) {
    resource_name <- primary_projection_resource$name
    resource_projection <- if (
      is.character(resource_name) &&
        length(resource_name) == 1L &&
        !is.na(resource_name) &&
        resource_name %in% projection_names
    ) {
      cv_resource_projection(
        primary_projection_resource,
        if (use_first_frame) {
          first_frame$projections[[resource_name]]
        } else {
          NULL
        },
        n
      )
    } else {
      NULL
    }
    if (length(resource_projection)) {
      resource_projection
    } else {
      built <- list()
      for (projection_name in unique(c(
        preferred_projection,
        projection_names
      ))) {
        built <- cv_build_projections(
          crb,
          cells,
          projection_name,
          preloaded = if (use_first_frame) first_frame$projections else NULL,
          canonical_projections = first_frame$canonical_projections,
          aligned_cache = first_frame$aligned_projections
        )
        if (length(built)) {
          break
        }
      }
      built
    }
  } else {
    cv_build_projections(crb, cells)
  }
  default_projection <- NULL
  spaces <- list()
  if (length(projections)) {
    configured_initial <- viewer_content[["initial_projections"]]
    if (is.character(configured_initial) && !is.object(configured_initial)) {
      configured_initial <- unique(configured_initial[
        !is.na(configured_initial) &
          nzchar(trimws(configured_initial)) &
          configured_initial %in% names(projections)
      ])
    } else {
      configured_initial <- character()
    }
    default_projection <- if (
      is.character(configured_projection) &&
        length(configured_projection) == 1L &&
        !is.na(configured_projection) &&
        configured_projection %in% names(projections)
    ) {
      configured_projection
    } else if (length(configured_initial)) {
      configured_initial[[1L]]
    } else if ("umap" %in% names(projections)) {
      "umap"
    } else {
      names(projections)[1]
    }
    initial_projections <- c(
      default_projection,
      configured_initial[configured_initial != default_projection]
    )
    ## Coordinates live in `projections`; the client rebuilds this descriptor
    ## from there. Keeping another x/y/z copy doubles the largest part of a
    ## million-cell wire payload.
    expression_space <- list(
      id = "expression",
      label = paste0(default_projection, " (expression)")
    )
    spaces[[length(spaces) + 1L]] <- expression_space
  }

  trajectories <- list()
  trekker_bundle <- NULL
  clone_bundle <- NULL
  if (!isTRUE(primary_only)) {
    trajectories <- cv_build_trajectories(crb, cells)
    if (length(trajectories)) {
      spaces <- c(spaces, trajectories)
    }

    ## Standard spatial and the Trekker physical mapping are independent spaces.
    sp <- cv_build_spatial(crb, cells)
    if (!is.null(sp)) {
      spaces[[length(spaces) + 1]] <- sp
    }
    tk <- cv_build_trekker(crb, cells, md)
    if (!is.null(tk)) {
      spaces[[length(spaces) + 1]] <- tk$space
      trekker_bundle <- tk$bundle
      fields <- c(fields, tk$fields)
    }

    ## Immune data is the largest optional payload in the Ren atlas. Build it
    ## only after the primary projection has painted and requested a supplement.
    cl <- cv_build_clone(crb, cells, n)
    if (!is.null(cl)) {
      spaces[[length(spaces) + 1]] <- cl$space
      groups[["clone_expansion"]] <- cl$group
      clone_bundle <- cl$bundle
    }
  }

  if (!length(spaces)) {
    return(NULL)
  }
  viewer_pack <- attr(crb, "cerebro_viewer_pack", exact = TRUE)
  canonical_order_id <- if (is.list(viewer_pack)) {
    as.character(viewer_pack$manifest$cell_order_fingerprint %||% "")
  } else {
    ""
  }
  pack_dataset_fingerprint <- if (is.list(viewer_pack)) {
    as.character(viewer_pack$manifest$dataset_fingerprint %||% "")
  } else {
    ""
  }

  if (!isTRUE(primary_only)) {
    available_groups <- intersect(group_names, names(groups))
    if (is.null(default_group) || !default_group %in% available_groups) {
      default_group <- if (length(available_groups)) {
        available_groups[[1L]]
      } else {
        NULL
      }
    }
  }

  list(
    ## Which data set this bundle IS. The client keeps per-image alignment state
    ## across pushes, and a bundle can be re-sent when returning to the tab.
    ## Without an identity to compare, "a new bundle" and "a new data set" look
    ## the same and the user's alignment work is thrown away by walking away and
    ## back.
    dataset_id = if (
      is.list(dataset_context) &&
        is.character(dataset_context$dataset_key) &&
        length(dataset_context$dataset_key) == 1L &&
        nzchar(dataset_context$dataset_key)
    ) {
      dataset_context$dataset_key
    } else {
      paste0("cells:", n, ":", if (n) cells[1] else "")
    },
    dataset_context = dataset_context,
    canonical_order_id = canonical_order_id,
    pack_dataset_fingerprint = pack_dataset_fingerprint,
    cells = I(cells),
    n = n,
    groups = groups,
    cat_extra = cat_extra,
    cat_skipped = cat_skipped,
    fields = fields,
    default_group = default_group,
    available_modalities = I(cv_progressive_modalities(crb, projection_names)),
    default_point_size = default_point_size,
    display_defaults = builderViewerDisplayDefaults(viewer_content$display),
    default_percentage_cells_to_show = default_percentage_cells_to_show,
    default_point_opacity = default_point_opacity,
    projections = projections,
    default_projection = default_projection,
    initial_projections = initial_projections,
    trajectories = trajectories,
    spaces = spaces,
    clone = clone_bundle,
    trekker = trekker_bundle
  )
}

cv_build_deferred_metadata <- function(crb, md, primary) {
  colors_for <- function(name, levels) {
    tryCatch(
      cerebro_group_colors(length(levels)),
      error = function(e) cv_colors_for(levels)
    )
  }
  deferred_group <- function(name, value) {
    levels <- if (is.factor(value)) {
      levels(value)
    } else {
      sort(unique(as.character(value)))
    }
    levels <- levels[!is.na(levels)]
    if (!length(levels)) {
      return(NULL)
    }
    list(
      values = NULL,
      levels = I(levels),
      colors = I(colors_for(name, levels)),
      deferred = TRUE
    )
  }
  group_names <- tryCatch(crb$getGroups(), error = function(e) character())
  groups <- list()
  for (name in setdiff(
    intersect(group_names, names(md)),
    names(primary$groups)
  )) {
    group <- deferred_group(name, md[[name]])
    if (!is.null(group)) groups[[name]] <- group
  }
  cat_extra <- list()
  cat_skipped <- list()
  fields <- list()
  list(
    groups = groups,
    cat_extra = cat_extra,
    cat_skipped = cat_skipped,
    fields = fields
  )
}

## Fast progressive path for an ordinary projection + immune data set. It uses
## the already-loaded thin first-frame metadata and the Viewer Pack's canonical
## immune index, so the supplement never materialises barcodes, per-cell strings,
## or metadata codes merely to discard them before transport.
## Supplement projections that the Viewer Pack serves as static resources are
## sent as descriptors: the browser fetches and caches them over HTTP instead of
## receiving every coordinate in one websocket message, and R builds no arrays
## for them. Returns the projections in `names` order.
cv_supplement_projections <- function(
  names,
  projection_resources,
  n,
  build,
  first_projections = NULL
) {
  resourced <- list()
  for (name in intersect(names, names(projection_resources))) {
    entry <- cv_resource_projection(
      list(name = name, descriptor = projection_resources[[name]]),
      first_projections[[name]],
      n
    )
    if (length(entry)) {
      resourced[[name]] <- entry[[1L]]
    }
  }
  built <- build(setdiff(names, names(resourced)))
  projections <- c(built, resourced)
  projections[intersect(names, names(projections))]
}

cv_build_compact_supplement <- function(
  crb,
  primary,
  first_frame,
  include_clone = TRUE,
  projection_resources = list()
) {
  pack <- attr(crb, "cerebro_viewer_pack", exact = TRUE)
  valid <- is.list(pack) &&
    is.list(first_frame) &&
    is.data.frame(first_frame$meta_data) &&
    is.list(first_frame$projections) &&
    identical(as.integer(primary$n), nrow(first_frame$meta_data)) &&
    identical(as.integer(pack$manifest$n_cells), as.integer(primary$n)) &&
    nzchar(primary$canonical_order_id %||% "") &&
    identical(
      as.character(pack$manifest$cell_order_fingerprint %||% ""),
      primary$canonical_order_id
    )
  if (!valid) {
    return(NULL)
  }
  projection_names <- names(first_frame$projections)
  unsupported <- length(tryCatch(
    crb$getMethodsForTrajectories(),
    error = function(e) character()
  )) ||
    length(tryCatch(
      crb$availableSpatial(),
      error = function(e) character()
    )) ||
    !is.null(tryCatch(crb$getTrekker(), error = function(e) NULL))
  if (unsupported) {
    return(NULL)
  }
  metadata <- cv_build_deferred_metadata(crb, first_frame$meta_data, primary)
  clone <- if (isTRUE(include_clone)) {
    cv_build_clone(crb, seq_len(primary$n), primary$n, sparse = TRUE)
  } else {
    NULL
  }
  if (!is.null(clone)) {
    metadata$groups[["clone_expansion"]] <- clone$group
  }
  list(
    dataset_id = primary$dataset_id,
    dataset_context = primary$dataset_context,
    dataset_fingerprint = primary$dataset_fingerprint,
    progressive_token = primary$progressive_token,
    groups = metadata$groups,
    cat_extra = metadata$cat_extra,
    fields = metadata$fields,
    cat_skipped = metadata$cat_skipped,
    projections = cv_supplement_projections(
      setdiff(projection_names, names(primary$projections)),
      projection_resources,
      primary$n,
      function(names) {
        cv_build_projections(
          crb,
          seq_len(primary$n),
          names,
          first_frame$projections,
          canonical_projections = first_frame$canonical_projections,
          aligned_cache = first_frame$aligned_projections
        )
      },
      first_frame$projections
    ),
    spaces = if (is.null(clone)) list() else list(clone$space),
    clone = if (is.null(clone)) NULL else clone$bundle,
    trekker = NULL
  )
}

## Build only the data absent from the primary response. Unlike the retired
## full-bundle/diff fallback, this never rebuilds the primary projection or
## materialises metadata vectors that the browser will request lazily.
cv_build_progressive_supplement <- function(
  crb,
  primary,
  include_clone = TRUE,
  projection_resources = list()
) {
  md <- cv_canonical_metadata(crb$getMetaData())
  if (is.null(md) || !identical(as.integer(nrow(md)), as.integer(primary$n))) {
    return(NULL)
  }
  cells <- md$cell_barcode
  metadata <- cv_build_deferred_metadata(crb, md, primary)
  projection_names <- tryCatch(
    crb$availableProjections(),
    error = function(e) character()
  )
  spaces <- cv_build_trajectories(crb, cells)
  spatial <- cv_build_spatial(crb, cells)
  if (!is.null(spatial)) {
    spaces[[length(spaces) + 1L]] <- spatial
  }
  trekker <- cv_build_trekker(crb, cells, md)
  if (!is.null(trekker)) {
    spaces[[length(spaces) + 1L]] <- trekker$space
    metadata$fields <- c(metadata$fields, trekker$fields)
  }
  clone <- if (isTRUE(include_clone)) {
    cv_build_clone(crb, cells, nrow(md), sparse = TRUE)
  } else {
    NULL
  }
  if (!is.null(clone)) {
    spaces[[length(spaces) + 1L]] <- clone$space
    metadata$groups[["clone_expansion"]] <- clone$group
  }
  list(
    dataset_id = primary$dataset_id,
    dataset_context = primary$dataset_context,
    dataset_fingerprint = primary$dataset_fingerprint,
    progressive_token = primary$progressive_token,
    groups = metadata$groups,
    cat_extra = metadata$cat_extra,
    fields = metadata$fields,
    cat_skipped = metadata$cat_skipped,
    projections = cv_supplement_projections(
      setdiff(projection_names, names(primary$projections)),
      projection_resources,
      primary$n,
      function(names) cv_build_projections(crb, cells, names)
    ),
    spaces = spaces,
    clone = if (is.null(clone)) NULL else clone$bundle,
    trekker = if (is.null(trekker)) NULL else trekker$bundle
  )
}

## Reference only geometry whose Viewer Pack explicitly validates the same
## canonical projection. Legacy/custom coordinate systems retain their arrays.
cv_reference_trajectory_geometry <- function(supplement, primary, crb) {
  pack <- attr(crb, "cerebro_viewer_pack", exact = TRUE)
  if (
    !is.list(pack) ||
      !identical(
        primary$canonical_order_id,
        pack$manifest$cell_order_fingerprint
      ) ||
      primary$n != pack$cell_count
  ) {
    return(supplement)
  }
  supplement$spaces <- lapply(supplement$spaces, function(space) {
    if (!isTRUE(space$trajectory)) {
      return(space)
    }
    frame <- viewerPackTrajectoryFrame(pack, space$method, space$name)
    if (
      is.null(frame) ||
        frame$geometry_kind != "canonical_projection" ||
        !frame$projection_name %in% names(primary$projections) ||
        !identical(
          as.integer(primary$projections[[frame$projection_name]]$ndim),
          2L
        ) ||
        length(space$x) != primary$n ||
        length(space$y) != primary$n ||
        !identical(is.na(space$x), is.na(space$y))
    ) {
      return(space)
    }
    present <- is.finite(space$x) & is.finite(space$y)
    if (sum(present) != frame$cells) {
      return(space)
    }
    space$geometry_ref <- list(
      projection = frame$projection_name,
      exclude = I(as.integer(which(!present) - 1L))
    )
    space$x <- NULL
    space$y <- NULL
    space
  })
  supplement
}

## Remove heavyweight browser-only assets from a bundle while keeping the
## descriptors needed to render controls. The server retains the returned map
## and sends one asset only when the browser displays that image or opens the
## corresponding Trekker cell card.
cv_asset_data_uri <- function(asset) {
  if (
    is.character(asset) && length(asset) == 1L && !is.na(asset) && nzchar(asset)
  ) {
    return(asset)
  }
  if (!is.list(asset)) {
    return(NULL)
  }
  path <- asset$path
  mime <- asset$mime
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !isTRUE(file_test("-f", path)) ||
      !is.character(mime) ||
      length(mime) != 1L ||
      is.na(mime) ||
      !(mime %in% c("image/png", "image/jpeg")) ||
      !requireNamespace("base64enc", quietly = TRUE)
  ) {
    return(NULL)
  }
  paste0("data:", mime, ";base64,", base64enc::base64encode(path))
}

cv_defer_assets <- function(value) {
  assets <- list()
  safe_key <- function(...) {
    gsub("[^A-Za-z0-9_.:-]", "_", paste(..., sep = ":"))
  }
  defer_images <- function(images, scope) {
    if (is.null(images) || !length(images)) {
      return(images)
    }
    original_class <- class(images)
    for (index in seq_along(images)) {
      image <- images[[index]]
      uri <- image$uri
      asset <- if (
        is.character(uri) && length(uri) == 1L && !is.na(uri) && nzchar(uri)
      ) {
        uri
      } else if (
        is.character(image$asset_path) &&
          length(image$asset_path) == 1L &&
          !is.na(image$asset_path) &&
          is.character(image$asset_mime) &&
          length(image$asset_mime) == 1L &&
          !is.na(image$asset_mime) &&
          image$asset_mime %in% c("image/png", "image/jpeg")
      ) {
        list(path = image$asset_path, mime = image$asset_mime)
      } else {
        NULL
      }
      image$asset_path <- NULL
      image$asset_mime <- NULL
      if (is.null(asset)) {
        images[[index]] <- image
        next
      }
      key <- safe_key(
        "image",
        scope,
        index,
        as.character(image$id %||% "image")
      )
      assets[[key]] <<- asset
      image$uri <- NULL
      image$asset_key <- key
      image$deferred <- TRUE
      images[[index]] <- image
    }
    class(images) <- original_class
    images
  }

  spaces <- value$spaces %||% list()
  for (space_index in seq_along(spaces)) {
    space <- spaces[[space_index]]
    scope <- safe_key(space$id %||% "space", space_index)
    space$images <- defer_images(space$images, scope)
    if (!is.null(space$image$uri)) {
      single <- defer_images(list(space$image), paste0(scope, ":default"))
      space$image <- single[[1L]]
    }
    samples <- space$samples %||% list()
    for (sample_index in seq_along(samples)) {
      sample <- samples[[sample_index]]
      sample_scope <- safe_key(
        scope,
        "sample",
        sample_index,
        sample$name %||% "sample"
      )
      sample$images <- defer_images(sample$images, sample_scope)
      if (!is.null(sample$image$uri)) {
        single <- defer_images(
          list(sample$image),
          paste0(sample_scope, ":default")
        )
        sample$image <- single[[1L]]
      }
      samples[[sample_index]] <- sample
    }
    if (!is.null(space$samples)) {
      space$samples <- samples
    }
    spaces[[space_index]] <- space
  }
  value$spaces <- spaces

  evidence_images <- value$trekker$evidence_img
  if (!is.null(evidence_images)) {
    evidence_images <- unclass(evidence_images)
    deferred_evidence <- FALSE
    for (index in seq_along(evidence_images)) {
      uri <- evidence_images[[index]]
      if (
        is.character(uri) && length(uri) == 1L && !is.na(uri) && nzchar(uri)
      ) {
        assets[[paste0("trekker-evidence:", index - 1L)]] <- uri
        deferred_evidence <- TRUE
      }
    }
    value$trekker$evidence_img <- NULL
    value$trekker$evidence_deferred <- deferred_evidence
  }
  list(value = value, assets = assets)
}
