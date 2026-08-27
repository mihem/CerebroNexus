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

## Resolve the currently selected dataset label used by all per-dataset Viewer
## configuration. `available_crb_files$selected` is the file path; the public
## createShinyApp() contract is keyed by the corresponding user-facing label.
cv_selected_dataset_name <- function() {
  nm <- NULL
  if (exists("available_crb_files") && !is.null(available_crb_files$selected)) {
    sel <- available_crb_files$selected
    idx <- which(available_crb_files$files == sel)
    if (length(idx)) {
      nm <- names(available_crb_files$files)[idx[1]]
      if (is.null(nm) || is.na(nm)) {
        nm <- available_crb_files$names[idx[1]]
      }
    }
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

## Builder alignment carries both image and point appearance. Bounds already
## contain its geometric transform, so Linked views only needs these appearance
## scalars and must validate them before they become client defaults.
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
  preset <- list(
    image_opacity = number("image_opacity", 0, 1),
    point_opacity = number("point_opacity", 0, 1),
    point_size = number("point_size", 0, 20, lower_open = TRUE)
  )
}

## Convert one public per-image settings leaf to the JavaScript transform
## contract. The same helper is used for embedded and external backgrounds:
## createShinyApp() intentionally allows a setting to target either kind.
cv_image_preset <- function(spatial_name, image_label) {
  defaults <- list(
    offsetX = 0,
    offsetY = 0,
    scaleX = 1,
    scaleY = 1,
    flipX = FALSE,
    flipY = FALSE,
    rotation = 0,
    opacity = 0.6
  )
  if (!exists("Cerebro.options")) {
    return(defaults)
  }
  dataset <- cv_selected_dataset_name()
  settings <- Cerebro.options[["spatial_image_settings"]]
  if (
    is.null(dataset) ||
      is.null(settings) ||
      !(dataset %in% names(settings)) ||
      !(spatial_name %in% names(settings[[dataset]])) ||
      !(image_label %in% names(settings[[dataset]][[spatial_name]]))
  ) {
    return(defaults)
  }
  setting <- settings[[dataset]][[spatial_name]][[image_label]]
  number <- function(key, default) {
    value <- suppressWarnings(as.numeric(setting[[key]] %||% default))
    if (length(value) != 1L || is.na(value) || !is.finite(value)) {
      default
    } else {
      unname(value)
    }
  }
  preset <- list(
    offsetX = number("offset_x", defaults$offsetX),
    offsetY = number("offset_y", defaults$offsetY),
    scaleX = number("scale_x", defaults$scaleX),
    scaleY = number("scale_y", defaults$scaleY),
    flipX = isTRUE(setting[["flip_x"]]),
    flipY = isTRUE(setting[["flip_y"]]),
    rotation = number("rotation", defaults$rotation),
    opacity = number("image_opacity", defaults$opacity)
  )
  if (!is.null(setting[["point_opacity"]])) {
    preset$pointOpacity <- number("point_opacity", 0.8)
  }
  if (!is.null(setting[["point_size"]])) {
    preset$pointSize <- number("point_size", 3)
  }
  preset
}

## Overlay the alignment stored beside one embedded image onto the generic
## Viewer preset. Embedded CRBs are self-contained, so their per-image leaf is
## the authority for every transform, not just appearance. Keeping this mapping
## here also makes the embedded and external JavaScript contracts identical.
cv_embedded_alignment_preset <- function(preset, alignment) {
  if (!is.list(alignment)) {
    return(preset)
  }
  number <- function(key, fallback) {
    value <- suppressWarnings(as.numeric(alignment[[key]]))
    if (length(value) != 1L || is.na(value) || !is.finite(value)) {
      fallback
    } else {
      unname(value)
    }
  }
  preset$offsetX <- number("dx", preset$offsetX)
  preset$offsetY <- number("dy", preset$offsetY)
  embedded_scale <- number("scale", preset$scaleX)
  preset$scaleX <- embedded_scale
  preset$scaleY <- embedded_scale
  preset$rotation <- number("rotation", preset$rotation)
  if (!is.null(alignment[["flip_x"]])) {
    preset$flipX <- isTRUE(alignment[["flip_x"]])
  }
  if (!is.null(alignment[["flip_y"]])) {
    preset$flipY <- isTRUE(alignment[["flip_y"]])
  }
  preset$opacity <- number("image_opacity", preset$opacity)
  preset$pointOpacity <- number(
    "point_opacity",
    preset$pointOpacity %||% 0.8
  )
  preset$pointSize <- number("point_size", preset$pointSize %||% 3)
  ## The Builder serializes embedded pixels after applying this geometry and
  ## writes their final data-space bounds. Viewer controls still expose the
  ## saved calibration, but drawing must apply only changes relative to it.
  preset$geometryBaked <- TRUE
  preset
}

## Resolve EXTERNAL histology images for one spatial entry of the selected data
## set. createShinyApp() stores them as dataset -> FOV -> image, with each leaf
## either a relative path or a descriptor containing path + coordinate bounds.
## The output is base64-encoded so the browser never receives a filesystem path.
cv_external_images <- function(spatial_name = NULL) {
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
  configured <- images_by_spatial[[spatial_name]]
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
    if (is.null(img_path) || !requireNamespace("base64enc", quietly = TRUE)) {
      next
    }
    mime <- image_mime(img_path)
    if (is.null(mime)) {
      next
    }
    base <- basename(path)
    label <- if (!is.null(labels) && nzchar(labels[[i]] %||% "")) {
      labels[[i]]
    } else {
      base
    }
    out[[length(out) + 1]] <- list(
      ## Section + position + label keep equal basenames and equal labels on
      ## different FOVs distinct, while remaining stable across bundle pushes.
      id = paste0("external:", spatial_name, ":", i, ":", label),
      label = label,
      uri = paste0(
        "data:",
        mime,
        ";base64,",
        base64enc::base64encode(img_path)
      ),
      bounds = bounds,
      preset = cv_image_preset(spatial_name, label)
    )
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
  list(
    dataset_id = bundle$dataset_id,
    groups = patch_groups(bundle$groups),
    cat_extra = patch_groups(bundle$cat_extra)
  )
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
      idx <- match(cells, trajectory_cells)
      if (!any(!is.na(idx))) {
        next
      }
      space <- cv_space(
        paste("trajectory", method, trajectory_name, sep = "::"),
        paste0(trajectory_name, " (trajectory)"),
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
## than one global constant. Must match FIELD_PREFIX in www/coordviews.js.
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
cv_build_groups <- function(crb, md, colors_fn) {
  group_names <- tryCatch(crb$getGroups(), error = function(e) character(0))
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

## Continuous colourings from the meta data — every numeric column, aligned to
## the meta data's row order (which IS `cells` order). This is the Projection
## tab's "Color cells by" list minus the categorical columns: it deliberately
## includes the QC columns (nUMI / nGene / percent.mt / nCount_*), because
## "colour the embedding by percent.mt and see which blob is junk" is one of the
## most-used actions on that page. Constant and all-NA columns are skipped —
## there is no colouring to build from them.
cv_build_fields <- function(md, skip = "cell_barcode") {
  fields <- list()
  for (mc in colnames(md)) {
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

## Categorical columns that are NOT registered grouping variables. The Projection
## tab offers every meta column in "Color cells by" while its "Group filters" box
## only lists getGroups(); this mirrors that split — these are colourings (legend,
## legend-hiding) but they do not become filters.
##
## Returns list(groups, skipped). A column with (nearly) as many levels as cells
## is an identifier rather than a grouping: one colour per cell, and a legend
## thousands of rows long. Those cannot be coloured by, but they are REPORTED
## instead of dropped — `skipped` is name -> level count, which the client shows
## greyed out in the picker. Silently omitting them left the two tabs offering
## different lists with no way to tell why.
cv_build_extra_groups <- function(md, group_names, colors_fn) {
  n <- nrow(md)
  max_levels <- max(2L, min(60L, as.integer(n / 2)))
  extra <- list()
  skipped <- list()
  for (mc in colnames(md)) {
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

## Every projection's coordinates travel in the bundle, keyed by name, so the
## expression panel can switch between UMAP / tSNE / PCA client-side with no
## server round-trip (the "one bundle per dataset, instant" contract).
##
## A 3-D embedding also sends its third dimension, so the client can orbit it
## rather than show a flattened shadow of it. `ndim` travels either way — the
## client needs to know which panels can rotate and which are flat.
cv_build_projections <- function(crb, cells) {
  proj_names <- tryCatch(crb$availableProjections(), error = function(e) NULL)
  projections <- list()
  for (pn in proj_names) {
    pj <- tryCatch(crb$getProjection(pn), error = function(e) NULL)
    if (is.null(pj)) {
      next
    }
    pjidx <- match(cells, rownames(pj))
    nd <- as.integer(ncol(pj))
    entry <- list(
      x = round(as.numeric(pj[pjidx, 1]), 4),
      y = round(as.numeric(pj[pjidx, 2]), 4),
      ndim = nd
    )
    ## I() so a single-cell data set still serialises z as an array, the same
    ## invariant cv_space() enforces for x/y.
    if (nd >= 3) {
      entry$z <- I(round(as.numeric(pj[pjidx, 3]), 4))
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
## its histology image. Two image sources, ONE contract: a base64 data URI + the
## data-space bounds it covers + an alignment preset (offset in DATA units, scale
## unitless, flip). The client maps the bounds to screen with the same transform
## as the cells, so the image aligns; preset/user transforms adjust on top.
##   - EMBEDDED (Xenium/MERFISH): image + its own bounds travel in the .crb.
##   - EXTERNAL (Visium H&E): separate files configured for this exact dataset
##     and FOV, with an optional per-image alignment preset and explicit bounds.
## Returns list(name, x, y, image) or NULL.
cv_spatial_one <- function(crb, cells, nm, allow_external) {
  sd <- tryCatch(crb$getSpatialData(nm), error = function(e) NULL)
  co <- if (!is.null(sd)) sd$coordinates else NULL
  if (is.null(co)) {
    return(NULL)
  }
  sidx <- match(cells, rownames(co))
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
  embedded <- sd[["histology_images", exact = TRUE]]
  if (is.null(embedded) || !length(embedded)) {
    legacy_image <- sd[["histology_image", exact = TRUE]]
    embedded <- if (!is.null(legacy_image)) {
      list("Embedded histology" = legacy_image)
    } else {
      list()
    }
  }
  alignment <- sd[["histology_alignment", exact = TRUE]]
  appearance <- cv_alignment_appearance(alignment)
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
      label <- entry$label %||% entry_name
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
    preset <- cv_image_preset(nm, label)
    entry_alignment <- if (is.list(entry)) {
      entry$histology_alignment %||% entry$alignment
    } else {
      NULL
    }
    preset <- cv_embedded_alignment_preset(preset, entry_alignment)
    entry_appearance <- cv_alignment_appearance(entry_alignment)
    if (length(entry_appearance$image_opacity) == 1L) {
      preset$opacity <- entry_appearance$image_opacity
    }
    if (length(entry_appearance$point_opacity) == 1L) {
      preset$pointOpacity <- entry_appearance$point_opacity
    }
    if (length(entry_appearance$point_size) == 1L) {
      preset$pointSize <- entry_appearance$point_size
    }
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
      preset = preset,
      coord_span = span
    )
  }
  if (allow_external) {
    for (ex in cv_external_images(nm)) {
      images[[length(images) + 1]] <- list(
        id = ex$id,
        label = ex$label,
        uri = ex$uri,
        bounds = ex$bounds %||% bounds_default,
        preset = ex$preset,
        coord_span = span
      )
    }
  }
  ## The default background, as a REFERENCE rather than a copy. A histology
  ## image is megabytes of base64; carrying the same one under both `image` and
  ## `images[1]` doubled it, and once more again for the space's own default --
  ## measured at 1.6 MB per copy on the Xenium demo, in a 3.6 MB bundle. Older
  ## readers of the singular field get the id and can look it up.
  image <- if (length(images)) {
    list(
      id = images[[1]]$id,
      label = images[[1]]$label,
      bounds = images[[1]]$bounds,
      preset = images[[1]]$preset,
      coord_span = images[[1]]$coord_span
    )
  } else {
    NULL
  }

  list(
    name = nm,
    x = round(as.numeric(co[sidx, 1]), 3),
    y = round(as.numeric(co[sidx, 2]), 3),
    ## `image` is the default one, kept so anything reading the older singular
    ## contract still works; `images` is the list the picker is built from.
    image = image,
    images = images,
    point_opacity = appearance$point_opacity,
    point_size = appearance$point_size
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
  built <- lapply(
    seq_along(sp_names),
    ## Every section resolves only its own dataset -> FOV -> image declarations.
    ## The client remembers the selected background and calibration per section.
    function(i) cv_spatial_one(crb, cells, sp_names[i], allow_external = TRUE)
  )
  built <- Filter(Negate(is.null), built)
  if (!length(built)) {
    return(NULL)
  }
  first <- built[[1]]
  space <- cv_space(
    "spatial",
    paste0(first$name, " (spatial)"),
    first$x,
    first$y
  )
  if (!is.null(first$image)) {
    space$image <- first$image
  }
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
        label = paste0(s$name, " (spatial)"),
        x = I(s$x),
        y = I(s$y),
        image = s$image,
        images = I(s$images),
        builder_point_opacity = s$point_opacity,
        builder_point_size = s$point_size
      )
    })
  }
  space$builder_point_opacity <- first$point_opacity
  space$builder_point_size <- first$point_size
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
  tk_idx <- match(cells, tk$barcodes)
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
  histology <- tk[["histology_image", exact = TRUE]]
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
    bounds <- tk[["histology_image_bounds", exact = TRUE]]
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
    label <- if (length(source) == 1L && !is.na(source) && nzchar(source)) {
      basename(source)
    } else {
      "Trekker background"
    }
    preset <- cv_image_preset("trekker", label)
    if (length(appearance$image_opacity) == 1L) {
      preset$opacity <- appearance$image_opacity
    }
    image_entry <- list(
      id = "trekker-embedded",
      label = label,
      uri = histology,
      bounds = as.list(bounds),
      preset = preset,
      coord_span = c(diff(xr), diff(yr))
    )
    space$image <- image_entry[c(
      "id",
      "label",
      "bounds",
      "preset",
      "coord_span"
    )]
    space$images <- I(list(image_entry))
    space$background_scope <- "Trekker"
  }
  space$builder_point_opacity <- appearance$point_opacity
  space$builder_point_size <- appearance$point_size
  ## Bring the Trekker page's extra controls into Linked views: continuous
  ## physical fields to colour by, per-cell positioning confidence (dissolve),
  ## and a positioning-evidence flag (nuclei markers). All aligned to `cells`;
  ## positioned-only fields are NA where unpositioned. Numeric META columns are
  ## NOT built here — cv_build_fields() offers every one of them for every data
  ## set, Trekker or not.
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
    ## shows, surfaced here via a modal (see coordviews.js `cv-tk-info-btn`).
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
cv_build_clone <- function(crb, cells, n) {
  ir <- tryCatch(crb$getImmuneRepertoire(), error = function(e) NULL)
  cp <- cv_clone_per_cell(ir, cells)
  if (is.null(cp) || !any(!is.na(cp$clone))) {
    return(NULL)
  }
  ct <- cp$clone
  tab <- sort(table(ct[!is.na(ct)]), decreasing = TRUE)
  clone_keys <- names(tab)
  clone_size <- as.integer(tab)
  cell_clone <- match(ct, clone_keys) # 1..K, NA if none
  ## A clone is called on CTgene, and one CTgene clone routinely spans several
  ## CDR3s in real receptor data can do this across many cells.
  ## Labelling it with whichever CDR3 happened to come first therefore named the
  ## row after one of its members: the table showed a single sequence while
  ## clicking it selected cells carrying eleven others. The dominant sequence is
  ## still the useful handle, so it stays, but the count of the rest travels with
  ## it and the column no longer claims to be a CDR3.
  ctaa <- cp$ctaa
  clone_aa <- split(
    ctaa[!is.na(ct)],
    factor(ct[!is.na(ct)], levels = clone_keys)
  )
  clone_summary <- Map(
    function(aa, k) {
      aa <- aa[!is.na(aa) & nzchar(aa)]
      if (!length(aa)) {
        return(list(n_cdr3 = NA_integer_, label = k))
      }
      list(
        n_cdr3 = length(unique(aa)),
        label = names(sort(table(aa), decreasing = TRUE))[1]
      )
    },
    clone_aa,
    clone_keys
  )
  clone_cdr3 <- vapply(clone_summary, `[[`, integer(1), "n_cdr3")
  clone_label <- vapply(clone_summary, `[[`, character(1), "label")
  K <- length(clone_keys)
  cx <- rep(NA_real_, n)
  cy <- rep(NA_real_, n)
  counter <- integer(K)
  for (i in seq_len(n)) {
    ci <- cell_clone[i]
    if (is.na(ci)) {
      next
    }
    counter[ci] <- counter[ci] + 1L
    cx[i] <- ci
    cy[i] <- counter[ci] - 1L
  }
  maxstack <- max(clone_size)
  na_cells <- which(is.na(cell_clone))
  if (length(na_cells)) {
    set.seed(1)
    cx[na_cells] <- -max(1, round(K * 0.06))
    cy[na_cells] <- stats::runif(length(na_cells), 0, maxstack)
  }
  ## Name the receptor in the label. The clonotypes shown are one class only
  ## (mixing TCR and BCR into one ranking is not something any page here does),
  ## and a data set carrying both would otherwise give no clue which is on screen.
  space <- cv_space(
    "clone",
    if (is.na(cp$receptor)) {
      "Clonal expansion"
    } else {
      paste0("Clonal expansion (", cp$receptor, ")")
    },
    cx,
    cy
  )
  size_per_cell <- ifelse(
    is.na(cell_clone),
    NA_integer_,
    clone_size[cell_clone]
  )
  ## Bins and labels come from clone_contract.R, so a clone lands in the same
  ## expansion level here as it does on the Clonal UMAP. "No receptor" is this
  ## page's own extra level: the Clonal UMAP draws those cells as a grey
  ## background layer rather than a level, but here every cell is in the same
  ## legend, so the absence has to be nameable.
  lvl <- as.character(cerebro_clone_expansion(size_per_cell))
  lvl[is.na(lvl)] <- "No receptor"
  lev <- c("No receptor", CEREBRO_CLONE_LABELS)
  group <- cv_group(
    match(lvl, lev) - 1L,
    lev,
    c("#e0e0e0", "#c6dbef", "#6baed6", "#f97316", "#c2410c", "#7f1d1d")
  )
  bundle <- cv_clone(
    ifelse(is.na(cell_clone), -1L, cell_clone - 1L),
    unname(clone_label),
    clone_size,
    K,
    sum(!is.na(cell_clone)),
    cp$receptor,
    unname(clone_cdr3)
  )
  list(space = space, group = group, bundle = bundle)
}

## Pick the initial categorical colouring from human-written metadata names.
## Separators and case are irrelevant; a short edit-distance fallback catches
## common transpositions such as "Cell Tyep". Cell type wins over sample, and a
## data set with neither starts on one randomly selected categorical field.
cv_default_group <- function(available) {
  available <- unique(as.character(available))
  available <- available[!is.na(available) & nzchar(available)]
  if (!length(available)) {
    return(NULL)
  }

  normalized <- tolower(gsub("[^[:alnum:]]", "", available))
  find_name <- function(target, prefix, max_distance = 2L) {
    exact <- which(normalized == target)
    if (length(exact)) {
      return(available[[exact[[1L]]]])
    }
    prefixed <- which(startsWith(normalized, target))
    if (length(prefixed)) {
      return(available[[prefixed[[1L]]]])
    }
    candidates <- which(startsWith(normalized, prefix))
    if (!length(candidates)) {
      return(NULL)
    }
    distances <- as.integer(utils::adist(normalized[candidates], target))
    closest <- which.min(distances)
    if (distances[[closest]] <= max_distance) {
      available[[candidates[[closest]]]]
    } else {
      NULL
    }
  }

  cell_type <- find_name("celltype", "cell")
  if (!is.null(cell_type)) {
    return(cell_type)
  }
  sample_group <- find_name("sample", "sam")
  if (!is.null(sample_group)) {
    return(sample_group)
  }
  sample(available, 1L)
}

## Assemble the bundle from the loaded Cerebro object. Each modality is built by
## its own cv_build_* helper; this function wires them into the final list.
cv_build_bundle <- function(crb) {
  md <- crb$getMetaData()
  if (is.null(md) || !("cell_barcode" %in% colnames(md))) {
    return(NULL)
  }
  cells <- as.character(md$cell_barcode)
  n <- length(cells)

  ## Seed a stable fallback here. The user-editable palette travels separately
  ## as cv_color_patch(), so changing one colour cannot rebuild this bundle.
  cv_group_colors <- function(group_name, lev) {
    tryCatch(
      cerebro_group_colors(length(lev)),
      error = function(e) cv_colors_for(lev)
    )
  }

  ## Three colouring sources, mirroring the Projection tab's "Color cells by"
  ## (which offers every meta column) while keeping its narrower "Group filters"
  ## (which lists only getGroups()):
  ##   groups    — registered grouping variables: colour AND filter
  ##   cat_extra — other categorical columns: colour only
  ##   fields    — numeric columns (+ Trekker's physical fields): continuous
  group_names <- tryCatch(crb$getGroups(), error = function(e) character(0))
  groups <- cv_build_groups(crb, md, cv_group_colors)
  extra <- cv_build_extra_groups(md, group_names, cv_group_colors)
  cat_extra <- extra$groups
  cat_skipped <- extra$skipped
  fields <- cv_build_fields(md)

  ## Every modality is independently useful. Linked views adds a coordinated
  ## workspace without changing the dedicated Projection/Spatial/Trekker pages.
  projections <- cv_build_projections(crb, cells)
  viewer_content <- cv_selected_viewer_content()
  default_projection <- NULL
  default_point_size <- suppressWarnings(
    as.numeric(viewer_content[["overview_point_size"]])
  )
  default_percentage_cells_to_show <- suppressWarnings(
    as.numeric(viewer_content[["overview_percentage_cells_to_show"]])
  )
  if (
    length(default_point_size) != 1L ||
      is.na(default_point_size) ||
      !is.finite(default_point_size)
  ) {
    default_point_size <- NULL
  } else {
    default_point_size <- unname(default_point_size)
  }
  if (
    length(default_percentage_cells_to_show) != 1L ||
      is.na(default_percentage_cells_to_show) ||
      !is.finite(default_percentage_cells_to_show) ||
      default_percentage_cells_to_show < 10 ||
      default_percentage_cells_to_show > 100
  ) {
    default_percentage_cells_to_show <- 100
  } else {
    default_percentage_cells_to_show <- unname(
      default_percentage_cells_to_show
    )
  }
  default_point_opacity <- NULL
  spaces <- list()
  if (length(projections)) {
    configured_projection <- viewer_content[["default_projection"]]
    default_projection <- if (
      is.character(configured_projection) &&
        length(configured_projection) == 1L &&
        !is.na(configured_projection) &&
        configured_projection %in% names(projections)
    ) {
      configured_projection
    } else if ("umap" %in% names(projections)) {
      "umap"
    } else {
      names(projections)[1]
    }
    dp <- projections[[default_projection]]
    expression_space <- cv_space(
      "umap",
      paste0(default_projection, " (expression)"),
      dp$x,
      dp$y
    )
    ## A 3-D embedding carries its z into the space too, so the expression panel
    ## starts orbitable rather than only becoming so after a projection switch.
    if (!is.null(dp$z)) {
      expression_space$z <- dp$z
      expression_space$axes <- dp$axes
    }
    spaces[[length(spaces) + 1L]] <- expression_space
  }

  trajectories <- cv_build_trajectories(crb, cells)
  if (length(trajectories)) {
    spaces <- c(spaces, trajectories)
  }

  ## Standard spatial and the Trekker physical mapping are INDEPENDENT spaces:
  ## add each whenever the object carries it. An object with both gets both panels
  ## (the right-panel switch flips between them); neither is dropped.
  sp <- cv_build_spatial(crb, cells)
  if (!is.null(sp)) {
    spaces[[length(spaces) + 1]] <- sp
  }
  trekker_bundle <- NULL
  tk <- cv_build_trekker(crb, cells, md)
  if (!is.null(tk)) {
    spaces[[length(spaces) + 1]] <- tk$space
    trekker_bundle <- tk$bundle
    fields <- c(fields, tk$fields)
  }

  ## immune axis: adds a clone space + a clone_expansion group when receptors
  ## are present.
  clone_bundle <- NULL
  cl <- cv_build_clone(crb, cells, n)
  if (!is.null(cl)) {
    spaces[[length(spaces) + 1]] <- cl$space
    groups[["clone_expansion"]] <- cl$group
    clone_bundle <- cl$bundle
  }

  if (!length(spaces)) {
    return(NULL)
  }

  ## Default colouring: prefer a cell-type-like name, then a sample-like name,
  ## then any categorical field. If no categorical field exists, use the first
  ## continuous field; with no colourable metadata the panels draw one colour.
  available_groups <- c(names(groups), names(cat_extra))
  default_group <- cv_default_group(available_groups)
  if (is.null(default_group) && length(fields)) {
    default_group <- paste0(cv_field_mode, names(fields)[1])
  }

  list(
    ## Which data set this bundle IS. The client keeps per-image alignment state
    ## across pushes, and a bundle can be re-sent when returning to the tab.
    ## Without an identity to compare, "a new bundle" and "a new data set" look
    ## the same and the user's alignment work is thrown away by walking away and
    ## back.
    dataset_id = tryCatch(
      {
        if (
          exists("available_crb_files") &&
            !is.null(available_crb_files$selected)
        ) {
          as.character(available_crb_files$selected)
        } else {
          paste0("cells:", n, ":", if (n) cells[1] else "")
        }
      },
      error = function(e) paste0("cells:", n)
    ),
    cells = cells,
    n = n,
    groups = groups,
    cat_extra = cat_extra,
    cat_skipped = cat_skipped,
    fields = fields,
    default_group = default_group,
    default_point_size = default_point_size,
    default_percentage_cells_to_show = default_percentage_cells_to_show,
    default_point_opacity = default_point_opacity,
    projections = projections,
    default_projection = default_projection,
    trajectories = trajectories,
    spaces = spaces,
    clone = clone_bundle,
    trekker = trekker_bundle
  )
}
