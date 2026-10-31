builder_spatial_test_source <- function(file, local = parent.frame()) {
  path <- builder_spatial_test_inst_path("builder", file)
  if (file.exists(path)) {
    sys.source(path, envir = local)
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

sys.source(
  builder_spatial_test_inst_path(
    "viewer",
    "core",
    "spatial_coordinate_contract.R"
  ),
  envir = environment()
)
sys.source(
  builder_spatial_test_inst_path(
    "viewer",
    "core",
    "spatial_coordinate_transform.R"
  ),
  envir = environment()
)
builder_spatial_test_source("spatial.R")
builder_spatial_test_source("preview.R")
builder_spatial_test_source("state/core.R")
builder_spatial_test_source("extras.R")
builder_spatial_test_source("worker.R")
builder_spatial_test_source("plan/defaults.R")
builder_spatial_test_source("notify.R")
builder_spatial_test_source("spatial_alignment_server.R")

test_that("image alignment scale remains strictly positive", {
  expect_error(
    .builder_alignment_parameters(list(scale = 0)),
    "outside its range",
    fixed = TRUE
  )
  expect_identical(
    .builder_alignment_parameters(list(scale = 0.02))$scale,
    0.02
  )
  expect_identical(
    .builder_alignment_parameters(list(scale = 0.001))$scale,
    0.001
  )
  expect_identical(
    .builder_alignment_parameters(list(scale = 0.01))$scale,
    0.01
  )
  expect_identical(builder_alignment_scale_step(0.001), 0.001)
  expect_identical(builder_alignment_scale_step(0.01), 0.01)
  expect_identical(builder_alignment_scale_step(1), 0.02)
})

test_that("alignment capability is limited to Spatial and Trekker datasets", {
  skip_if_not_installed("SeuratObject")
  spatial <- builder_content_spatial_example_object(c("section-a", "section-b"))
  plain <- spatial
  methods::slot(plain, "images", check = FALSE) <- list()
  plain@misc$trekker <- NULL
  trekker <- plain
  trekker@misc$trekker <- .builder_content_spatial_demo_payload()

  expect_length(builder_spatial_alignment_sections(plain), 0L)

  spatial_sections <- builder_spatial_alignment_sections(spatial)
  expect_identical(
    vapply(spatial_sections, `[[`, character(1), "id"),
    c("section-a", "section-b")
  )
  expect_true(all(vapply(
    spatial_sections,
    function(section) identical(section$kind, "spatial"),
    logical(1)
  )))
  expect_true(all(vapply(
    spatial_sections,
    function(section) identical(section$layers, "points"),
    logical(1)
  )))

  trekker_sections <- builder_spatial_alignment_sections(trekker)
  expect_length(trekker_sections, 1L)
  expect_identical(trekker_sections[[1L]]$id, "trekker")
  expect_identical(trekker_sections[[1L]]$kind, "trekker")
  expect_match(trekker_sections[[1L]]$unit, "physical", ignore.case = TRUE)
})

test_that("alignment scenes summarize sample and ROI metadata", {
  skip_if_not_installed("SeuratObject")
  object <- builder_content_spatial_example_object("xenium-fov")
  object$sample_roi <- rep(
    c("S1_lesion", "S2_border", "S2_normal"),
    length.out = ncol(object)
  )

  scene <- builder_spatial_alignment_sections(object)[[1L]]

  expect_s3_class(scene, "builder_viewer_spatial_scene")
  expect_identical(scene$observations$count, as.integer(ncol(object)))
  expect_identical(scene$annotations$sample$field, "sample")
  expect_identical(scene$annotations$sample$count, 2L)
  expect_identical(scene$annotations$roi$field, "sample_roi")
  expect_identical(scene$annotations$roi$count, 3L)
  expect_match(scene$label, "2 samples", fixed = TRUE)
  expect_match(scene$label, "3 ROIs", fixed = TRUE)
})

test_that("alignment preview separates observations by ROI metadata", {
  skip_if_not_installed("SeuratObject")
  object <- builder_content_spatial_example_object("xenium-fov")
  cells <- colnames(object)
  object$sample_roi <- rep(
    c("lesion", "border", "normal", "core"),
    length.out = length(cells)
  )

  all_rois <- builder_alignment_preview_model(
    object,
    default_projection = "pca",
    group = "seurat_clusters",
    section_id = "xenium-fov",
    layer = "counts"
  )
  lesion <- builder_alignment_preview_model(
    object,
    default_projection = "pca",
    group = "seurat_clusters",
    section_id = "xenium-fov",
    roi = "lesion",
    layer = "counts"
  )

  expect_setequal(all_rois$roi$values, c("lesion", "border", "normal", "core"))
  expect_setequal(names(all_rois$roi_bounds), all_rois$roi$values)
  expect_equal(all_rois$roi_bounds$lesion, lesion$bounds)
  expect_setequal(
    unique(all_rois$spatial$group),
    unique(as.character(object$seurat_clusters))
  )
  expect_setequal(unique(all_rois$spatial$roi), all_rois$roi$values)
  expect_identical(
    lesion$spatial$cell_barcode,
    cells[object$sample_roi == "lesion"]
  )
  expect_identical(
    lesion$spatial$group,
    as.character(object$seurat_clusters[object$sample_roi == "lesion"])
  )
  expect_true(all(lesion$spatial$roi == "lesion"))
  scene <- builder_spatial_canvas_scene(
    all_rois,
    colors = builder_level_colors(c("C1", "C2")),
    layout = "separate",
    cell_percentage = 25,
    identity = "dataset-a::xenium-fov",
    generation = 1L
  )
  expect_identical(scene$points$group, as.character(all_rois$spatial$group))
  expect_identical(scene$points$roi, as.character(all_rois$spatial$roi))
  expect_identical(scene$controls$cell_percentage, 25)
  expect_lt(
    diff(unlist(lesion$bounds[c("xmin", "xmax")])),
    diff(unlist(all_rois$bounds[c("xmin", "xmax")]))
  )
})

test_that("alignment scenes expose available spatial layers", {
  skip_if_not_installed("SeuratObject")
  object <- builder_content_spatial_example_object("xenium-fov")
  image <- object@images[[1L]]
  boundaries <- methods::slot(image, "boundaries")
  boundaries$segmentation <- SeuratObject::CreateSegmentation(data.frame(
    x = c(0, 1, 1, 0),
    y = c(0, 0, 1, 1),
    cell = rep(colnames(object)[[1L]], 4L)
  ))
  methods::slot(image, "boundaries") <- boundaries
  methods::slot(image, "molecules") <- list(
    RNA = SeuratObject::CreateMolecules(data.frame(
      x = c(0.25, 0.75),
      y = c(0.25, 0.75),
      gene = c("Gene1", "Gene2")
    ))
  )
  object@images[[1L]] <- image
  object@misc$cerebro_spatial_images <- list(
    `xenium-fov` = list(
      DAPI = list(histology_image = "data:image/png;base64,AA==")
    )
  )

  scene <- builder_spatial_alignment_sections(object)[[1L]]

  expect_identical(
    scene$layers,
    c("points", "raster", "boundaries", "molecules")
  )
})

test_that("alignment accepts a paired spatial reduction without a Seurat image", {
  skip_if_not_installed("SeuratObject")
  object <- builder_content_spatial_example_object()
  methods::slot(object, "images", check = FALSE) <- list()
  object@misc$trekker <- NULL
  cells <- colnames(object)
  coordinates <- matrix(
    seq_len(length(cells) * 2L),
    nrow = length(cells),
    dimnames = list(cells, c("SPATIAL_1", "SPATIAL_2"))
  )
  object[["SPATIAL"]] <- SeuratObject::CreateDimReducObject(
    embeddings = coordinates,
    key = "SPATIAL_",
    assay = SeuratObject::DefaultAssay(object)
  )

  sections <- builder_spatial_alignment_sections(object)
  expect_length(sections, 1L)
  expect_identical(sections[[1L]]$id, "SPATIAL")
  expect_identical(sections[[1L]]$kind, "spatial_reduction")

  model <- builder_alignment_preview_model(
    object,
    default_projection = "pca",
    section_id = "SPATIAL",
    layer = "counts"
  )
  expect_true(model$available)
  expect_identical(model$section$kind, "spatial_reduction")
  expect_identical(model$spatial$cell_barcode, model$transcriptome$cell_barcode)
  expect_equal(
    unname(as.matrix(model$spatial[, c("x", "y")])),
    unname(coordinates[model$spatial$cell_barcode, , drop = FALSE])
  )
})

test_that("preview cache pruning drops only the removed dataset", {
  shared <- list(
    "dataset-a" = list(status = "ready"),
    "dataset-b" = list(status = "ready")
  )
  spatial <- list(
    "dataset-a::section-1" = list(status = "ready"),
    "dataset-a::section-2" = list(status = "pending"),
    "dataset-b::section-1" = list(status = "ready")
  )

  expect_identical(
    names(builder_preview_cache_drop_dataset(shared, "dataset-a")),
    "dataset-b"
  )
  expect_identical(
    names(builder_spatial_preview_cache_drop_dataset(spatial, "dataset-a")),
    "dataset-b::section-1"
  )
})

test_that("alignment projection prefers UMAP then the current default then PCA", {
  expect_identical(
    builder_alignment_projection(c("pca", "umap"), "pca"),
    "umap"
  )
  expect_identical(
    builder_alignment_projection(c("pca", "tsne"), "tsne"),
    "tsne"
  )
  expect_identical(
    builder_alignment_projection(c("pca", "tsne"), "missing"),
    "pca"
  )
  expect_null(builder_alignment_projection("tsne", "missing"))
})

test_that("Builder runtime has no data-URI image materialization path", {
  extras <- paste(
    readLines(
      builder_spatial_test_inst_path("builder", "extras.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  server <- paste(
    readLines(
      builder_spatial_test_inst_path("builder", "spatial_alignment_server.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_false(grepl("builder_parse_image_uri", extras, fixed = TRUE))
  expect_false(grepl("builder_materialize_image_uri", extras, fixed = TRUE))
  expect_false(grepl("base64encode", extras, fixed = TRUE))
  expect_false(grepl("base64encode", server, fixed = TRUE))
  expect_false(grepl("readBin(data$path", server, fixed = TRUE))
  expect_match(server, "list(file = data$path, owned = FALSE)", fixed = TRUE)
})

test_that("alignment server does not subscribe to Plotly selection events", {
  server <- paste(
    readLines(
      builder_spatial_test_inst_path("builder", "spatial_alignment_server.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_false(grepl("plotly::event_data(", server, fixed = TRUE))
  expect_false(grepl("builder_alignment_event_cells", server, fixed = TRUE))
  expect_false(grepl("selected_cells", server, fixed = TRUE))
  expect_false(grepl("session$onFlushed(", server, fixed = TRUE))
  expect_false(grepl(".clientValue-", server, fixed = TRUE))
})

test_that("finishing a check waits for uploads and active interactions", {
  server <- paste(
    readLines(
      builder_test_inst_path(
        "builder",
        "spatial_alignment_server.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    server,
    "active_alignment_settled <- shiny::reactive({",
    fixed = TRUE
  )
  expect_match(server, "if (!is.null(pending_upload()))", fixed = TRUE)
  expect_match(server, "interaction <- client_interaction()", fixed = TRUE)
  settled <- sub(
    "(?s)^.*active_alignment_settled <- shiny::reactive\\(\\{",
    "",
    server,
    perl = TRUE
  )
  settled <- sub(
    "(?s)materialize_coordinate_drafts <- function\\(.*$",
    "",
    settled,
    perl = TRUE
  )
  expect_false(grepl("alignment_preview()", settled, fixed = TRUE))
  expect_false(grepl("preview_matches_owner", settled, fixed = TRUE))
  expect_match(
    server,
    "!identical(interaction$owner, active_control_owner())",
    fixed = TRUE
  )
  expect_match(
    server,
    "alignment_settled = active_alignment_settled",
    fixed = TRUE
  )

  review <- paste(
    readLines(
      builder_test_inst_path("builder", "server", "review.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    review,
    "alignment_settled <- isTRUE(alignment_server$alignment_settled())",
    fixed = TRUE
  )
  expect_match(review, "check_ready = alignment_settled", fixed = TRUE)
})

test_that("alignment preview requeues when its render contract changes", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")

  entry <- list(
    id = "dataset-a",
    snapshot = list(
      path = "/private/dataset-a",
      owner_token = "owner-a",
      object_md5 = strrep("a", 32L)
    ),
    profile = list(images = "section-a", extras = list()),
    settings = list(
      name = "Dataset A",
      images = list(),
      default_group = "cluster",
      default_projection = "umap",
      palette = "cerebro"
    )
  )
  current_entry <- shiny::reactiveVal(entry)
  current <- shiny::reactiveVal(entry$id)
  alignment_preview <- shiny::reactiveVal(NULL)
  spatial_coords <- shiny::reactiveVal(NULL)
  requests <- list()

  shiny::testServer(
    function(input, output, session) {
      builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = current,
        entry_of = function(id) current_entry(),
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) {
          requests[[length(requests) + 1L]] <<- request
          TRUE
        },
        commit_images = function(entry, images) NULL,
        alignment_preview = alignment_preview,
        spatial_coords = spatial_coords
      )
    },
    {
      session$flushReact()
      expect_length(requests, 1L)
      expect_identical(requests[[1L]]$max_cells, 5000L)

      renamed <- current_entry()
      renamed$settings$name <- "Dataset A renamed"
      current_entry(renamed)
      session$flushReact()
      expect_length(requests, 1L)

      regrouped <- current_entry()
      regrouped$settings$default_group <- "sample"
      current_entry(regrouped)
      session$flushReact()
      expect_length(requests, 2L)
      expect_identical(requests[[2L]]$group, "sample")

      replaced <- current_entry()
      replaced$snapshot$object_md5 <- strrep("b", 32L)
      current_entry(replaced)
      session$flushReact()
      expect_length(requests, 3L)
    }
  )
})

test_that("alignment points reuse the selected Group palette", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")

  entry <- list(
    id = "dataset-a",
    snapshot = list(
      path = "/private/dataset-a",
      owner_token = "owner-a",
      object_md5 = strrep("a", 32L)
    ),
    profile = list(images = "section-a", extras = list()),
    levels = list(cluster = c("A", "B", "C")),
    settings = list(
      name = "Dataset A",
      images = list(),
      default_group = "cluster",
      default_projection = "umap",
      palette = "cerebro",
      group_color_overrides = list(cluster = c(B = "#123456"))
    )
  )
  current_entry <- shiny::reactiveVal(entry)
  alignment_preview <- shiny::reactiveVal(NULL)
  scenes <- list()
  preview <- list(
    available = TRUE,
    bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    coordinate_frame = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    section = list(id = "section-a", kind = "spatial", unit = "pixels"),
    projection_name = "umap",
    capped = FALSE,
    roi = list(field = NULL, values = character(), selected = ""),
    spatial = data.frame(
      cell_barcode = c("cell-b", "cell-a"),
      x = c(2, 8),
      y = c(3, 7),
      group = c("B", "A"),
      stringsAsFactors = FALSE
    )
  )

  shiny::testServer(
    function(input, output, session) {
      original_send_custom_message <- session$sendCustomMessage
      session$sendCustomMessage <- function(type, message) {
        if (identical(type, "builder_spatial_canvas_scene")) {
          scenes[[length(scenes) + 1L]] <<- message
        }
        original_send_custom_message(type, message)
      }
      alignment <- builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = shiny::reactiveVal(entry$id),
        entry_of = function(id) current_entry(),
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) TRUE,
        commit_images = function(updated, images) invisible(updated),
        alignment_preview = alignment_preview,
        spatial_coords = shiny::reactiveVal(NULL)
      )
    },
    {
      session$flushReact()
      alignment_preview(preview)
      session$flushReact()

      expected <- builder_level_colors(
        c("A", "B", "C"),
        "cerebro",
        c(B = "#123456")
      )
      expect_identical(
        utils::tail(scenes, 1L)[[1L]]$points$color,
        unname(expected[c("B", "A")])
      )

      recolored <- current_entry()
      recolored$settings$group_color_overrides$cluster[["A"]] <- "#ABCDEF"
      current_entry(recolored)
      session$flushReact()

      expect_identical(
        utils::tail(scenes, 1L)[[1L]]$points$color,
        c("#123456", "#ABCDEF")
      )
    }
  )
})

test_that("pending tissue image requires its matching preview and snapshot", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  skip_if_not_installed("png")

  image_path <- tempfile(fileext = ".png")
  on.exit(unlink(image_path), add = TRUE)
  write_dummy_png(image_path)

  entry <- list(
    id = "dataset-a",
    snapshot = list(
      path = "/private/dataset-a",
      owner_token = "owner-a",
      object_md5 = strrep("a", 32L)
    ),
    profile = list(images = "section-a", extras = list()),
    settings = list(
      name = "Dataset A",
      images = list(),
      default_group = "cluster",
      default_projection = "umap",
      palette = "cerebro",
      spatial_point_appearance = list(
        "section-a" = list(point_opacity = 0.65, point_size = 6)
      )
    )
  )
  current_entry <- shiny::reactiveVal(entry)
  current <- shiny::reactiveVal(entry$id)
  alignment_preview <- shiny::reactiveVal(NULL)
  spatial_coords <- shiny::reactiveVal(NULL)
  committed <- list()
  commit_count <- 0L
  notifications <- character()
  notify_env <- environment(builder_spatial_alignment_server)
  while (!exists("builder_notify", envir = notify_env, inherits = FALSE)) {
    notify_env <- parent.env(notify_env)
  }
  real_notify <- get("builder_notify", envir = notify_env)
  assign(
    "builder_notify",
    function(type, message, ...) {
      notifications <<- c(notifications, as.character(message))
    },
    envir = notify_env
  )
  withr::defer(assign("builder_notify", real_notify, envir = notify_env))
  preview <- list(
    available = TRUE,
    bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    section = list(id = "section-a", kind = "spatial", unit = "pixels"),
    projection_name = "umap",
    capped = FALSE,
    transcriptome = data.frame(
      cell_id = c("cell-a", "cell-b"),
      x = c(-1, 1),
      y = c(-1, 1),
      group = c("A", "B"),
      stringsAsFactors = FALSE
    ),
    spatial = data.frame(
      cell_id = c("cell-a", "cell-b"),
      x = c(2, 8),
      y = c(3, 7),
      group = c("A", "B"),
      stringsAsFactors = FALSE
    )
  )

  shiny::testServer(
    function(input, output, session) {
      alignment <- builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = current,
        entry_of = function(id) current_entry(),
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) TRUE,
        commit_images = function(entry, images) {
          updated <- entry
          updated$settings$images <- images
          current_entry(updated)
          committed <<- images
          commit_count <<- commit_count + 1L
        },
        alignment_preview = alignment_preview,
        spatial_coords = spatial_coords
      )
    },
    {
      session$flushReact()
      expect_null(alignment$draft())

      session$setInputs(
        `enhance-tissue_image_file` = data.frame(
          name = "section-a.png",
          size = file.info(image_path)$size,
          type = "image/png",
          datapath = image_path,
          stringsAsFactors = FALSE
        )
      )
      session$flushReact()
      expect_null(alignment$draft())

      alignment_preview(preview)
      session$flushReact()

      expect_identical(alignment$draft()$source$name, "section-a.png")
      expect_false("saved" %in% names(alignment$draft()))
      expect_true(file.exists(alignment$draft()$source_path))
      expect_false("source_uri" %in% names(alignment$draft()))
      expect_identical(alignment$draft()$point_opacity, 0.65)
      expect_identical(alignment$draft()$point_size, 6)
      scene <- alignment$canvas_contract()
      image_error <- list(
        dataset = scene$dataset,
        snapshotIdentity = scene$snapshotIdentity,
        section = scene$section,
        viewKey = paste0(scene$viewKey, "-retired"),
        generation = scene$generation,
        sourceKey = alignment$draft()$source_content_md5,
        nonce = 1
      )
      session$setInputs(builder_spatial_image_load_error = image_error)
      session$flushReact()
      expect_length(notifications, 0L)
      image_error$viewKey <- scene$viewKey
      image_error$nonce <- 2
      session$setInputs(builder_spatial_image_load_error = image_error)
      session$flushReact()
      expect_length(notifications, 1L)

      controls <- scene$controls
      controls$image_opacity <- 0.7
      controls$cell_percentage <- 35
      session$setInputs(
        builder_spatial_alignment_controls = list(
          dataset = scene$dataset,
          snapshotIdentity = scene$snapshotIdentity,
          section = scene$section,
          roi = "",
          image = scene$activeImage,
          viewKey = scene$viewKey,
          generation = scene$generation,
          resetToken = scene$resetToken,
          sequence = 1,
          controls = controls
        )
      )
      session$flushReact()
      expect_identical(alignment$draft()$image_opacity, 0.7)
      expect_identical(
        current_entry()$settings$spatial_percentage_cells_to_show,
        35
      )
      expect_null(current_entry()$settings$overview_percentage_cells_to_show)
      expect_length(alignment$coordinate_drafts(), 0L)
      expect_false("roi_coordinates" %in% names(alignment$pending_drafts()))

      image_file_record <- alignment$image_file_record()
      changed_draft <- alignment$draft()
      changed_draft$image_opacity <- 0.25
      alignment$draft(changed_draft)
      session$flushReact()
      expect_identical(alignment$image_file_record(), image_file_record)
      expect_identical(
        current_entry()$settings$spatial_point_appearance[["section-a"]],
        list(point_opacity = 0.65, point_size = 6)
      )
      expect_named(committed, "section-a")
      expect_identical(commit_count, 2L)

      session$setInputs(
        `enhance-renamed_image_label` = "Renamed section",
        `enhance-rename_image_confirm` = 1L
      )
      session$flushReact()
      expect_identical(alignment$active_image(), "Renamed section")
      expect_identical(alignment$draft()$image_label, "Renamed section")
      expect_identical(
        alignment$image_file_record()$image_label,
        "Renamed section"
      )
      expect_identical(
        current_entry()$settings$images[["section-a"]][[
          "Renamed section"
        ]]$image_label,
        "Renamed section"
      )
      expect_identical(commit_count, 3L)

      suppressWarnings(session$setInputs(`enhance-drop_image` = 1L))
      session$flushReact()
      session$setInputs(`enhance-remove_image_confirm` = 1L)
      session$flushReact()
      expect_null(alignment$draft())
      expect_null(alignment$image_file_record())
      expect_identical(commit_count, 4L)
      expect_identical(
        current_entry()$settings$spatial_point_appearance[["section-a"]],
        list(point_opacity = 0.65, point_size = 6)
      )

      alignment_preview(NULL)
      session$setInputs(
        `enhance-tissue_image_file` = data.frame(
          name = "stale-section-a.png",
          size = file.info(image_path)$size,
          type = "image/png",
          datapath = image_path,
          stringsAsFactors = FALSE
        )
      )
      session$flushReact()

      replaced <- current_entry()
      replaced$snapshot$object_md5 <- strrep("b", 32L)
      current_entry(replaced)
      session$flushReact()
      alignment_preview(preview)
      session$flushReact()

      expect_null(alignment$draft())
      expect_identical(commit_count, 4L)
    }
  )
})

test_that("alignment controls auto-commit before dataset switches", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  skip_if_not_installed("png")

  image_path <- tempfile(fileext = ".png")
  on.exit(unlink(image_path), add = TRUE)
  write_dummy_png(image_path)
  image <- builder_read_image(image_path)
  expect_null(image$error)
  section_a_parameters <- builder_alignment_defaults()
  section_a_parameters$scale <- 0.001
  record <- builder_alignment_record(
    source = list(name = "section-a.png", type = "image/png"),
    base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    parameters = section_a_parameters,
    image_geometry = image,
    section = list(id = "section-a", kind = "spatial"),
    source_path = image$source_path
  )
  record$source_content_md5 <- image$source_content_md5
  section_b_parameters <- builder_alignment_defaults()
  section_b_parameters$scale <- 0.01
  record_b <- builder_alignment_record(
    source = list(name = "section-b.png", type = "image/png"),
    base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    parameters = section_b_parameters,
    image_geometry = image,
    section = list(id = "section-b", kind = "spatial"),
    source_path = image$source_path
  )
  record_b$source_content_md5 <- image$source_content_md5
  entry <- list(
    id = "dataset-a",
    snapshot = list(
      path = "/private/dataset-a",
      owner_token = "owner-a",
      object_md5 = strrep("a", 32L)
    ),
    profile = list(images = c("section-a", "section-b"), extras = list()),
    settings = list(
      name = "Dataset A",
      images = list(
        `section-a` = list(`H&E` = record),
        `section-b` = list(`H&E` = record_b)
      ),
      default_group = "cluster",
      default_projection = "umap",
      palette = "cerebro"
    )
  )
  preview <- list(
    available = TRUE,
    bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    section = list(id = "section-a", kind = "spatial", unit = "pixels"),
    projection_name = "umap",
    capped = FALSE,
    transcriptome = data.frame(
      cell_id = c("cell-a", "cell-b"),
      x = c(-1, 1),
      y = c(-1, 1),
      group = c("A", "B")
    ),
    spatial = data.frame(
      cell_id = c("cell-a", "cell-b"),
      x = c(2, 8),
      y = c(3, 7),
      group = c("A", "B")
    )
  )
  preview_b <- preview
  preview_b$section$id <- "section-b"
  current_entry <- shiny::reactiveVal(entry)
  current <- shiny::reactiveVal(entry$id)
  alignment_preview <- shiny::reactiveVal(preview)
  spatial_coords <- shiny::reactiveVal(NULL)
  switched <- character()
  input_messages <- list()

  shiny::testServer(
    function(input, output, session) {
      session$sendInputMessage <- function(input_id, message) {
        input_messages[[length(input_messages) + 1L]] <<- list(
          id = input_id,
          message = message
        )
        invisible()
      }
      alignment <- builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = current,
        entry_of = function(id) current_entry(),
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) TRUE,
        commit_images = function(entry, images) {
          entry$settings$images <- images
          current_entry(entry)
          invisible(entry)
        },
        alignment_preview = alignment_preview,
        spatial_coords = spatial_coords
      )
    },
    {
      session$flushReact()
      alignment_preview(preview)
      session$flushReact()

      expect_identical(alignment$draft()$scale, 0.001)
      initial_scale_updates <- Filter(
        function(item) identical(item$id, "enhance-img_scale"),
        input_messages
      )
      expect_identical(
        as.numeric(tail(initial_scale_updates, 1L)[[1L]]$message$value),
        0.001
      )

      session$setInputs(`enhance-active_section` = "section-b")
      session$flushReact()
      alignment_preview(preview_b)
      session$flushReact()
      expect_identical(alignment$active_section(), "section-b")
      expect_identical(alignment$draft()$scale, 0.01)
      section_b_updates <- Filter(
        function(item) identical(item$id, "enhance-img_scale"),
        input_messages
      )
      expect_identical(
        as.numeric(tail(section_b_updates, 1L)[[1L]]$message$value),
        0.01
      )

      session$setInputs(`enhance-active_section` = "section-a")
      session$flushReact()
      alignment_preview(preview)
      session$flushReact()
      expect_identical(alignment$active_section(), "section-a")
      expect_identical(alignment$draft()$scale, 0.001)
      section_a_updates <- Filter(
        function(item) identical(item$id, "enhance-img_scale"),
        input_messages
      )
      expect_identical(
        as.numeric(tail(section_a_updates, 1L)[[1L]]$message$value),
        0.001
      )

      expect_true(
        alignment$materialize_coordinate_drafts(
          dataset = "dataset-a",
          notify = FALSE,
          require_settled = TRUE
        )$ok
      )

      switch_accepted <- alignment$request_dataset_switch(
        "dataset-b",
        function() {
          switched <<- c(switched, "immediate")
        }
      )
      expect_true(switch_accepted)
      expect_identical(switched, "immediate")

      session$setInputs(`enhance-img_dx` = 0)
      session$elapse(60)
      session$flushReact()

      session$setInputs(`enhance-img_dx` = 2)
      session$elapse(60)
      session$flushReact()
      expect_identical(alignment$draft()$dx, 2)
      expect_false("outside" %in% names(alignment$draft()))
      expect_identical(
        current_entry()$settings$images[["section-a"]][["H&E"]]$dx,
        2
      )
      alignment$request_dataset_switch("dataset-b", function() {
        switched <<- c(switched, "after-dx")
      })
      expect_identical(switched, c("immediate", "after-dx"))
      expect_identical(alignment$draft()$dx, 2)

      session$setInputs(`enhance-img_rotate` = 90)
      session$elapse(60)
      session$flushReact()
      expect_identical(alignment$draft()$rotation, 90)
      expect_identical(
        current_entry()$settings$images[["section-a"]][["H&E"]]$rotation,
        90
      )

      session$setInputs(`enhance-image_flip_x` = TRUE)
      session$elapse(60)
      session$flushReact()
      expect_true(alignment$draft()$flip_x)
      expect_true(
        current_entry()$settings$images[["section-a"]][["H&E"]]$flip_x
      )

      session$setInputs(`enhance-img_dx` = 1)
      session$elapse(60)
      session$flushReact()
      canonical <- alignment$current_record()
      expect_identical(alignment$draft()$uri, canonical$uri)
      expect_identical(alignment$draft()$bounds, canonical$bounds)
      expect_false("outside" %in% names(alignment$draft()))
      expect_false("total" %in% names(alignment$draft()))
      expect_false("outside" %in% names(canonical))
      expect_false("total" %in% names(canonical))
      alignment$request_dataset_switch("dataset-b", function() {
        switched <<- c(switched, "after-final-change")
      })
      expect_identical(
        switched,
        c("immediate", "after-dx", "after-final-change")
      )
      expect_identical(alignment$draft()$dx, 1)
      expect_identical(
        current_entry()$settings$images[["section-a"]][["H&E"]]$dx,
        1
      )
    }
  )
})

test_that("stale section events cannot switch the active sample back", {
  skip_if_not_installed("shiny")

  entry <- list(
    id = "dataset-a",
    snapshot = list(
      path = "/private/dataset-a",
      owner_token = "owner-a",
      object_md5 = strrep("a", 32L)
    ),
    profile = list(
      images = c("section-a", "section-b"),
      spatial_scenes = list(
        list(
          id = "section-a",
          label = "Sample 1 section",
          annotations = list(sample = list(values = "sample-1"))
        ),
        list(
          id = "section-b",
          label = "Sample 2 section",
          annotations = list(sample = list(values = "sample-2"))
        )
      ),
      extras = list()
    ),
    settings = list(
      name = "Dataset A",
      images = list(),
      default_group = "cluster",
      default_projection = "umap",
      palette = "cerebro"
    )
  )
  current_entry <- shiny::reactiveVal(entry)
  current <- shiny::reactiveVal(entry$id)
  preview_requests <- list()

  shiny::testServer(
    function(input, output, session) {
      alignment <- builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = current,
        entry_of = function(id) current_entry(),
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) {
          preview_requests[[length(preview_requests) + 1L]] <<- request
          TRUE
        },
        commit_images = function(updated, images) invisible(updated),
        alignment_preview = shiny::reactiveVal(NULL),
        spatial_coords = shiny::reactiveVal(NULL)
      )
    },
    {
      session$flushReact()
      expect_identical(alignment$active_sample(), "sample-1")
      expect_identical(alignment$active_section(), "section-a")

      session$setInputs(`enhance-active_sample` = "sample-2")
      session$flushReact()
      expect_identical(alignment$active_sample(), "sample-2")
      expect_identical(alignment$active_section(), "section-b")
      requests_after_sample_switch <- length(preview_requests)

      ## A delayed event from the section select that belonged to Sample 1
      ## must not drive switch_to() and infer Sample 1 again.
      session$setInputs(`enhance-active_section` = "section-a")
      session$flushReact()
      expect_identical(alignment$active_sample(), "sample-2")
      expect_identical(alignment$active_section(), "section-b")
      expect_identical(length(preview_requests), requests_after_sample_switch)
    }
  )
})

test_that("new images inherit the active image appearance", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  skip_if_not_installed("png")

  image_path <- tempfile(fileext = ".png")
  on.exit(unlink(image_path), add = TRUE)
  write_dummy_png(image_path)
  image <- builder_read_image(image_path)
  expect_null(image$error)
  parameters <- builder_alignment_defaults()
  parameters[c("point_opacity", "point_size")] <- list(0.65, 6)
  existing_record <- builder_alignment_record(
    source = list(name = "duplicate.png", type = "image/png"),
    base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    parameters = parameters,
    section = list(id = "section-a", kind = "spatial"),
    source_path = image$source_path
  )
  existing_record$source_content_md5 <- image$source_content_md5
  active_record <- existing_record
  active_record$source$name <- "DAPI.png"
  active_record[c("point_opacity", "point_size")] <- list(0.7, 7)
  border_record <- existing_record
  border_record$source$name <- "border.png"
  border_record$roi_field <- "sample_roi"
  border_record$roi_value <- "border"

  entry <- list(
    id = "dataset-a",
    snapshot = list(
      path = "/private/dataset-a",
      owner_token = "owner-a",
      object_md5 = strrep("a", 32L)
    ),
    profile = list(
      images = "section-a",
      spatial_scenes = list(list(
        id = "section-a",
        annotations = list(
          roi = list(
            field = "sample_roi",
            count = 2L,
            values = c("lesion", "border")
          )
        )
      )),
      extras = list()
    ),
    settings = list(
      name = "Dataset A",
      images = list(
        `section-a` = list(
          `duplicate.png` = existing_record,
          DAPI = active_record,
          `border.png` = border_record
        )
      ),
      default_group = "cluster",
      default_projection = "umap",
      palette = "cerebro"
    )
  )
  current_entry <- shiny::reactiveVal(entry)
  current <- shiny::reactiveVal(entry$id)
  alignment_preview <- shiny::reactiveVal(NULL)
  spatial_coords <- shiny::reactiveVal(NULL)
  commit_count <- 0L
  preview_requests <- list()
  preview <- list(
    available = TRUE,
    bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    section = list(id = "section-a", kind = "spatial", unit = "pixels"),
    projection_name = "umap",
    roi = list(
      field = "sample_roi",
      values = c("lesion", "border"),
      selected = NULL
    ),
    capped = FALSE,
    transcriptome = data.frame(
      cell_id = c("cell-a", "cell-b"),
      x = c(-1, 1),
      y = c(-1, 1),
      group = c("A", "B"),
      stringsAsFactors = FALSE
    ),
    spatial = data.frame(
      cell_id = c("cell-a", "cell-b"),
      x = c(2, 8),
      y = c(3, 7),
      group = c("A", "B"),
      stringsAsFactors = FALSE
    )
  )
  upload <- data.frame(
    name = "border.png",
    size = file.info(image_path)$size,
    type = "image/png",
    datapath = image_path,
    stringsAsFactors = FALSE
  )

  shiny::testServer(
    function(input, output, session) {
      alignment <- builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = current,
        entry_of = function(id) current_entry(),
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) {
          preview_requests[[length(preview_requests) + 1L]] <<- request
          TRUE
        },
        commit_images = function(entry, images) {
          updated <- current_entry()
          updated$settings$images <- images
          current_entry(updated)
          commit_count <<- commit_count + 1L
        },
        alignment_preview = alignment_preview,
        spatial_coords = spatial_coords
      )
    },
    {
      session$flushReact()
      alignment_preview(preview)
      session$flushReact()
      expect_identical(alignment$active_image(), "duplicate.png")
      expect_named(
        current_entry()$settings$images[["section-a"]],
        c("duplicate.png", "DAPI", "border.png")
      )
      expect_identical(
        current_entry()$settings$images[["section-a"]][["duplicate.png"]][
          c("point_opacity", "point_size")
        ],
        list(point_opacity = 0.65, point_size = 6)
      )
      session$setInputs(`enhance-active_roi` = "__separate__")
      session$flushReact()
      expect_identical(alignment$roi_view(), "__separate__")
      expect_identical(alignment$active_roi(), "lesion")
      expect_identical(utils::tail(preview_requests, 1L)[[1L]]$roi, "")
      expect_identical(
        utils::tail(preview_requests, 1L)[[1L]]$group,
        "cluster"
      )
      alignment_preview(preview)
      session$flushReact()
      roi_event <- function(roi, nonce) {
        contract <- alignment$canvas_contract()
        list(
          roi = roi,
          dataset = contract$dataset,
          snapshotIdentity = contract$snapshotIdentity,
          section = contract$section,
          viewKey = contract$viewKey,
          generation = contract$generation,
          nonce = nonce
        )
      }
      stale_roi_event <- roi_event("border", 0)
      stale_roi_event$snapshotIdentity <- "stale snapshot"
      session$setInputs(builder_spatial_roi_select = stale_roi_event)
      session$flushReact()
      expect_identical(alignment$active_roi(), "lesion")
      session$setInputs(
        builder_spatial_roi_select = roi_event("border", 1)
      )
      session$flushReact()
      expect_identical(alignment$active_roi(), "border")
      session$setInputs(
        builder_spatial_roi_select = roi_event("lesion", 2)
      )
      session$flushReact()
      expect_identical(alignment$active_roi(), "lesion")
      session$setInputs(`enhance-active_image` = "DAPI")
      session$flushReact()
      expect_identical(alignment$active_image(), "DAPI")

      session$setInputs(`enhance-active_image` = "border.png")
      session$flushReact()
      expect_identical(alignment$active_image(), "DAPI")

      suppressWarnings(session$setInputs(`enhance-tissue_image_file` = upload))
      session$flushReact()
      expect_null(alignment$pending_upload())
      expect_identical(alignment$active_image(), "border.png.1")

      expect_named(
        current_entry()$settings$images[["section-a"]],
        c("duplicate.png", "DAPI", "border.png", "border.png.1")
      )
      expect_identical(
        current_entry()$settings$images[["section-a"]][["border.png.1"]][
          c("point_opacity", "point_size")
        ],
        list(point_opacity = 0.7, point_size = 7)
      )
      expect_identical(
        current_entry()$settings$images[["section-a"]][["border.png.1"]][
          c("roi_field", "roi_value")
        ],
        list(roi_field = "sample_roi", roi_value = "lesion")
      )
      expect_identical(
        current_entry()$settings$images[["section-a"]][["border.png.1"]][[
          "image_label"
        ]],
        "border.png"
      )
      expect_identical(commit_count, 1L)

      ## Separate ROIs and an explicit ROI are two views of the same ROI state.
      ## Switching views must restore the uploaded image and its point style.
      separate_scene <- alignment$canvas_contract()
      separate_controls <- separate_scene$controls
      separate_controls$coordinateRotation <- -94.7
      session$setInputs(
        builder_spatial_alignment_controls = list(
          dataset = separate_scene$dataset,
          snapshotIdentity = separate_scene$snapshotIdentity,
          section = separate_scene$section,
          roi = separate_scene$activeRoi,
          image = separate_scene$activeImage,
          viewKey = separate_scene$viewKey,
          generation = separate_scene$generation,
          resetToken = separate_scene$resetToken,
          sequence = 1,
          controls = separate_controls
        )
      )
      session$flushReact()
      session$setInputs(`enhance-active_roi` = "lesion")
      session$flushReact()
      single_roi_preview <- preview
      single_roi_preview$roi$selected <- "lesion"
      alignment_preview(single_roi_preview)
      session$flushReact()
      expect_identical(alignment$roi_view(), "lesion")
      expect_identical(alignment$active_roi(), "lesion")
      expect_identical(alignment$active_image(), "border.png.1")
      expect_identical(alignment$canvas_contract()$activeRoi, "lesion")
      expect_identical(alignment$canvas_contract()$activeImage, "border.png.1")
      expect_identical(
        alignment$canvas_contract()$controls$coordinateRotation,
        -94.7
      )
      expect_identical(
        alignment$canvas_contract()$controls[c("point_opacity", "point_size")],
        list(point_opacity = 0.7, point_size = 7)
      )

      session$setInputs(`enhance-active_roi` = "")
      session$flushReact()
      session$setInputs(`enhance-active_image` = "DAPI")
      session$flushReact()
      session$setInputs(`enhance-remove_image_confirm` = 1L)
      session$flushReact()

      expect_identical(alignment$active_image(), "duplicate.png")
      expect_named(
        current_entry()$settings$images[["section-a"]],
        c("duplicate.png", "border.png", "border.png.1")
      )
      expect_identical(
        current_entry()$settings$images[["section-a"]][["border.png"]][
          c("roi_field", "roi_value")
        ],
        list(roi_field = "sample_roi", roi_value = "border")
      )
      expect_identical(
        current_entry()$settings$images[["section-a"]][["duplicate.png"]][
          c("point_opacity", "point_size")
        ],
        list(point_opacity = 0.65, point_size = 6)
      )
      expect_identical(
        current_entry()$settings$images[["section-a"]][["border.png.1"]][
          c("point_opacity", "point_size")
        ],
        list(point_opacity = 0.7, point_size = 7)
      )
      expect_identical(commit_count, 2L)
    }
  )
})

test_that("removing an ROI's last image keeps its point appearance ROI-scoped", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("png")

  image_path <- tempfile(fileext = ".png")
  on.exit(unlink(image_path), add = TRUE)
  write_dummy_png(image_path)
  image <- builder_read_image(image_path)
  expect_null(image$error)

  lesion_parameters <- builder_alignment_defaults()
  lesion_parameters$point_opacity <- 0.4
  lesion_parameters$point_size <- 3
  lesion <- builder_alignment_record(
    source = list(name = "lesion.png", type = "image/png"),
    base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    parameters = lesion_parameters,
    image_geometry = image,
    section = list(id = "section-a", kind = "spatial"),
    source_path = image$source_path
  )
  lesion$source_content_md5 <- image$source_content_md5
  lesion$roi_field <- "sample_roi"
  lesion$roi_value <- "lesion"
  lesion$image_label <- "Lesion"

  border <- lesion
  border$source$name <- "border.png"
  border$roi_value <- "border"
  border$image_label <- "Border"
  border$point_opacity <- 0.9
  border$point_size <- 8

  entry <- list(
    id = "dataset-a",
    snapshot = list(
      path = "/private/dataset-a",
      owner_token = "owner-a",
      object_md5 = strrep("a", 32L)
    ),
    profile = list(
      images = "section-a",
      spatial_scenes = list(list(
        id = "section-a",
        annotations = list(
          roi = list(
            field = "sample_roi",
            count = 2L,
            values = c("lesion", "border")
          )
        )
      )),
      extras = list()
    ),
    settings = list(
      name = "Dataset A",
      images = list(
        `section-a` = list(
          lesion = lesion,
          border = border
        )
      ),
      spatial_point_appearance = list(
        `section-a` = list(point_opacity = 0.8, point_size = 5)
      ),
      spatial_roi_settings = list(),
      default_group = "cluster",
      default_projection = "umap",
      palette = "cerebro"
    )
  )
  preview <- list(
    available = TRUE,
    bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    section = list(id = "section-a", kind = "spatial", unit = "pixels"),
    projection_name = "umap",
    roi = list(
      field = "sample_roi",
      values = c("lesion", "border"),
      selected = NULL
    ),
    capped = FALSE,
    transcriptome = data.frame(
      cell_barcode = c("cell-a", "cell-b"),
      x = c(-1, 1),
      y = c(-1, 1),
      group = c("A", "B"),
      stringsAsFactors = FALSE
    ),
    spatial = data.frame(
      cell_barcode = c("cell-a", "cell-b"),
      x = c(2, 8),
      y = c(3, 7),
      group = c("A", "B"),
      roi = c("lesion", "border"),
      stringsAsFactors = FALSE
    )
  )
  current_entry <- shiny::reactiveVal(entry)
  current <- shiny::reactiveVal(entry$id)
  alignment_preview <- shiny::reactiveVal(preview)

  shiny::testServer(
    function(input, output, session) {
      alignment <- builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = current,
        entry_of = function(id) current_entry(),
        entries = shiny::reactive(list(current_entry())),
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) TRUE,
        commit_images = function(updated, images) {
          updated$settings$images <- images
          current_entry(updated)
          invisible(updated)
        },
        alignment_preview = alignment_preview,
        spatial_coords = shiny::reactiveVal(NULL)
      )
    },
    {
      session$flushReact()
      alignment_preview(preview)
      session$flushReact()
      expect_identical(alignment$roi_view(), "__separate__")
      expect_identical(alignment$active_roi(), "lesion")
      expect_identical(alignment$active_image(), "lesion")

      alignment_preview(NULL)
      session$flushReact()
      expect_true(alignment$alignment_settled())
      alignment_preview(preview)
      session$flushReact()

      session$setInputs(`enhance-remove_image_confirm` = 1L)
      session$flushReact()

      expect_null(alignment$active_image())
      expect_named(
        current_entry()$settings$images[["section-a"]],
        "border"
      )
      expect_identical(
        current_entry()$settings$spatial_point_appearance[["section-a"]],
        list(point_opacity = 0.8, point_size = 5)
      )
      expect_identical(
        alignment$pending_drafts()$roi_appearance[["dataset-a"]][[
          "section-a"
        ]]$lesion[c("point_opacity", "point_size")],
        list(point_opacity = 0.4, point_size = 3)
      )

      materialized <- alignment$materialize_coordinate_drafts(
        dataset = "dataset-a",
        notify = FALSE
      )
      expect_true(materialized$ok)
      expect_identical(
        current_entry()$settings$spatial_roi_settings[["section-a"]][[
          "lesion"
        ]][c("point_opacity", "point_size")],
        list(point_opacity = 0.4, point_size = 3)
      )
      expect_identical(
        current_entry()$settings$spatial_point_appearance[["section-a"]],
        list(point_opacity = 0.8, point_size = 5)
      )
    }
  )
})

test_that("browser control ownership rejects retired views but accepts an old owner flush", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("png")

  image_path <- tempfile(fileext = ".png")
  on.exit(unlink(image_path), add = TRUE)
  write_dummy_png(image_path)
  image <- builder_read_image(image_path)
  expect_null(image$error)
  record <- builder_alignment_record(
    source = list(name = "image-a.png", type = "image/png"),
    base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    image_geometry = image,
    section = list(id = "section-a", kind = "spatial"),
    source_path = image$source_path
  )
  record$source_content_md5 <- image$source_content_md5
  second <- record
  second$source$name <- "image-b.png"
  entry <- list(
    id = "dataset-a",
    snapshot = list(
      path = "/private/dataset-a",
      owner_token = "owner-a",
      object_md5 = strrep("a", 32L)
    ),
    profile = list(
      images = "section-a",
      spatial_scenes = list(list(
        id = "section-a",
        annotations = list(
          roi = list(
            field = "sample_roi",
            count = 2L,
            values = c("lesion", "border")
          )
        )
      )),
      extras = list()
    ),
    settings = list(
      name = "Dataset A",
      images = list(`section-a` = list(A = record, B = second)),
      spatial_roi_settings = list(
        `section-a` = list(
          lesion = list(
            rotation_degrees = 0,
            point_opacity = 0.8,
            point_size = 5L
          )
        )
      ),
      default_group = "cluster",
      default_projection = "umap",
      palette = "cerebro"
    )
  )
  preview <- list(
    available = TRUE,
    bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    section = list(id = "section-a", kind = "spatial", unit = "pixels"),
    projection_name = "umap",
    roi = list(
      field = "sample_roi",
      values = c("lesion", "border"),
      selected = NULL
    ),
    capped = FALSE,
    transcriptome = data.frame(
      cell_barcode = c("cell-a", "cell-b"),
      x = c(-1, 1),
      y = c(-1, 1),
      group = c("lesion", "border"),
      stringsAsFactors = FALSE
    ),
    spatial = data.frame(
      cell_barcode = c("cell-a", "cell-b"),
      x = c(2, 8),
      y = c(3, 7),
      group = c("lesion", "border"),
      stringsAsFactors = FALSE
    )
  )
  current_entry <- shiny::reactiveVal(entry)
  current <- shiny::reactiveVal(entry$id)
  alignment_preview <- shiny::reactiveVal(preview)

  shiny::testServer(
    function(input, output, session) {
      alignment <- builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = current,
        entry_of = function(id) current_entry(),
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) TRUE,
        commit_images = function(updated, images) {
          updated$settings$images <- images
          current_entry(updated)
          invisible(updated)
        },
        alignment_preview = alignment_preview,
        spatial_coords = shiny::reactiveVal(NULL)
      )
    },
    {
      session$flushReact()
      alignment_preview(preview)
      session$flushReact()
      expect_identical(alignment$roi_view(), "__separate__")
      expect_identical(alignment$active_roi(), "lesion")
      expect_identical(alignment$active_image(), "A")

      control_event <- function(scene, sequence, scale, rotation) {
        controls <- scene$controls
        controls$scale <- scale
        controls$coordinateRotation <- rotation
        list(
          dataset = scene$dataset,
          snapshotIdentity = scene$snapshotIdentity,
          section = scene$section,
          roi = scene$activeRoi %||% "",
          image = scene$activeImage %||% "",
          viewKey = scene$viewKey,
          generation = scene$generation,
          resetToken = scene$resetToken,
          sequence = sequence,
          controls = controls
        )
      }

      initial_scene <- alignment$canvas_contract()
      images_before_viewport <- current_entry()$settings$images
      session$setInputs(
        builder_spatial_viewports = list(
          viewKey = initial_scene$viewKey,
          generation = initial_scene$generation,
          viewports = list(
            lesion = list(xmin = -5, xmax = 15, ymin = -5, ymax = 15)
          ),
          imageFitViewports = list()
        )
      )
      session$flushReact()
      expect_identical(
        current_entry()$settings$images,
        images_before_viewport
      )

      synchronized <- control_event(initial_scene, 1, 1, 0)
      synchronized$controls$point_size <- as.numeric(
        synchronized$controls$point_size
      )
      session$setInputs(builder_spatial_alignment_controls = synchronized)
      session$flushReact()
      expect_false("roi_coordinates" %in% names(alignment$pending_drafts()))
      expect_false("roi_appearance" %in% names(alignment$pending_drafts()))
      materialized <- alignment$materialize_coordinate_drafts(
        dataset = "dataset-a",
        notify = FALSE
      )
      expect_true(materialized$ok)
      expect_identical(
        current_entry()$settings$spatial_roi_settings[["section-a"]][[
          "lesion"
        ]]$point_size,
        5L
      )

      session$setInputs(
        builder_spatial_alignment_controls = control_event(
          initial_scene,
          2,
          0.6,
          10
        )
      )
      session$flushReact()
      expect_identical(
        current_entry()$settings$images[["section-a"]]$A$scale,
        0.6
      )
      ## The server may publish a replacement scene before the browser receives
      ## it. A second complete control snapshot from the same active view still
      ## belongs to that activation epoch even though it carries the old render
      ## generation.
      session$setInputs(
        builder_spatial_alignment_controls = control_event(
          initial_scene,
          3,
          0.7,
          20
        )
      )
      session$flushReact()
      expect_identical(
        current_entry()$settings$images[["section-a"]]$A$scale,
        0.7
      )
      expect_identical(
        alignment$pending_drafts()$roi_coordinates[["dataset-a"]][[
          "section-a"
        ]]$lesion$spec$rotation_degrees,
        20
      )
      missing_reset_token <- control_event(initial_scene, 4, 0.8, 30)
      missing_reset_token$resetToken <- NULL
      session$setInputs(
        builder_spatial_alignment_controls = missing_reset_token
      )
      session$flushReact()
      expect_identical(
        current_entry()$settings$images[["section-a"]]$A$scale,
        0.7
      )
      expect_identical(
        alignment$pending_drafts()$roi_coordinates[["dataset-a"]][[
          "section-a"
        ]]$lesion$spec$rotation_degrees,
        20
      )
      pre_reset_scene <- alignment$canvas_contract()
      session$setInputs(`enhance-reset_coordinate_transform` = 1L)
      session$flushReact()
      reset_scene <- alignment$canvas_contract()
      expect_gt(reset_scene$resetToken, pre_reset_scene$resetToken)
      session$setInputs(
        builder_spatial_alignment_controls = control_event(
          pre_reset_scene,
          4,
          0.8,
          30
        )
      )
      session$flushReact()
      expect_identical(
        current_entry()$settings$images[["section-a"]]$A$scale,
        0.7
      )
      expect_false("roi_coordinates" %in% names(alignment$pending_drafts()))
      separate_scene <- alignment$canvas_contract()
      expect_identical(separate_scene$viewKey, initial_scene$viewKey)
      expect_gte(separate_scene$generation, initial_scene$generation)

      ## The owner stays (ROI lesion + image A), but the view is replaced.
      session$setInputs(`enhance-active_roi` = "lesion")
      session$flushReact()
      alignment_preview(preview)
      session$flushReact()
      single_scene <- alignment$canvas_contract()
      expect_false(identical(single_scene$viewKey, separate_scene$viewKey))

      stale_different_view <- control_event(separate_scene, 5, 0.9, 40)
      session$setInputs(
        builder_spatial_alignment_controls = stale_different_view
      )
      session$flushReact()
      expect_identical(
        current_entry()$settings$images[["section-a"]]$A$scale,
        0.7
      )

      ## Returning to a deterministic viewKey starts a new generation epoch;
      ## an event from the retired incarnation must still be rejected.
      session$setInputs(`enhance-active_roi` = "__separate__")
      session$flushReact()
      alignment_preview(preview)
      session$flushReact()
      returned_scene <- alignment$canvas_contract()
      expect_identical(returned_scene$viewKey, separate_scene$viewKey)
      expect_gt(returned_scene$generation, separate_scene$generation)
      stale_same_key <- control_event(separate_scene, 6, 1, 45)
      session$setInputs(builder_spatial_alignment_controls = stale_same_key)
      session$flushReact()
      expect_identical(
        current_entry()$settings$images[["section-a"]]$A$scale,
        0.7
      )

      ## A pre-switch flush belongs to a different owner after ROI selection.
      ## It remains valid for persistence, but must not replace the active
      ## border ROI's coordinate reactive state.
      roi_event <- list(
        roi = "border",
        dataset = returned_scene$dataset,
        snapshotIdentity = returned_scene$snapshotIdentity,
        section = returned_scene$section,
        viewKey = returned_scene$viewKey,
        generation = returned_scene$generation,
        nonce = 1
      )
      session$setInputs(builder_spatial_roi_select = roi_event)
      session$flushReact()
      expect_identical(alignment$active_roi(), "border")
      delayed_old_owner <- control_event(returned_scene, 7, 1, 50)
      session$setInputs(
        builder_spatial_alignment_controls = delayed_old_owner
      )
      session$flushReact()

      expect_identical(
        current_entry()$settings$images[["section-a"]]$A$scale,
        1
      )
      expect_identical(
        alignment$pending_drafts()$roi_coordinates[["dataset-a"]][[
          "section-a"
        ]]$lesion$spec$rotation_degrees,
        50
      )
      expect_identical(alignment$canvas_contract()$activeRoi, "border")
      expect_identical(
        alignment$canvas_contract()$controls$coordinateRotation,
        0
      )

      ## Reactivating the same owner can reproduce the same deterministic
      ## view key. Its newer generation is the exact high-water mark, so the
      ## retired generation must no longer be accepted.
      border_scene <- alignment$canvas_contract()
      session$setInputs(
        builder_spatial_roi_select = list(
          roi = "lesion",
          dataset = border_scene$dataset,
          snapshotIdentity = border_scene$snapshotIdentity,
          section = border_scene$section,
          viewKey = border_scene$viewKey,
          generation = border_scene$generation,
          nonce = 2
        )
      )
      session$flushReact()
      reactivated_scene <- alignment$canvas_contract()
      expect_identical(reactivated_scene$viewKey, returned_scene$viewKey)
      expect_gt(reactivated_scene$generation, returned_scene$generation)
      session$setInputs(
        builder_spatial_alignment_controls = control_event(
          returned_scene,
          7,
          1.1,
          90
        )
      )
      session$flushReact()
      expect_identical(
        current_entry()$settings$images[["section-a"]]$A$scale,
        1
      )
      expect_identical(
        alignment$pending_drafts()$roi_coordinates[["dataset-a"]][[
          "section-a"
        ]]$lesion$spec$rotation_degrees,
        50
      )
    }
  )
})

test_that("alignment preview joins both spaces by cell identity", {
  skip_if_not_installed("SeuratObject")
  object <- builder_content_spatial_example_object("section-a")
  group <- colnames(object@meta.data)[[1L]]

  model <- builder_alignment_preview_model(
    object,
    default_projection = "pca",
    group = group,
    section_id = "section-a",
    layer = "counts",
    max_cells = 12L
  )

  expect_true(model$available)
  expect_identical(model$section$id, "section-a")
  expect_lte(nrow(model$transcriptome), 12L)
  expect_identical(
    model$transcriptome$cell_barcode,
    model$spatial$cell_barcode
  )
  expect_identical(model$transcriptome$group, model$spatial$group)
  expect_named(model$bounds, c("xmin", "xmax", "ymin", "ymax"))
  expect_true(all(is.finite(unlist(model$bounds))))
})

test_that("bounded alignment previews never retain full coverage coordinates", {
  skip_if_not_installed("SeuratObject")
  object <- builder_content_spatial_example_object("section-a")
  preview <- builder_alignment_preview_model(
    object,
    section_id = "section-a",
    layer = "counts",
    max_cells = 2L
  )

  expect_true(preview$available)
  expect_lte(nrow(preview$spatial), 2L)
  expect_false("coverage" %in% names(preview))
  expect_equal(preview$total_cells, ncol(object))
  preview$roi_bounds <- list(
    A = list(xmin = -100, xmax = 10, ymin = 0, ymax = 200),
    B = list(xmin = 20, xmax = 30, ymin = 0, ymax = 20)
  )

  roi_image <- function(name, opacity) {
    builder_alignment_record(
      source = list(name = name, type = "image/png"),
      source_uri = paste0("data:image/png;base64,", name),
      uri = paste0("data:image/png;base64,", name),
      base_bounds = list(xmin = 0, xmax = 5, ymin = 0, ymax = 5),
      parameters = list(image_opacity = opacity),
      section = list(id = "section-a", kind = "spatial")
    )
  }
  scene <- builder_spatial_canvas_scene(
    preview,
    colors = character(),
    point_appearance = list(point_opacity = 0.65, point_size = 6),
    roi_images = list(
      A = list(a = roi_image("A", 0.8)),
      B = list(b = roi_image("B", 0.7))
    ),
    layout = "separate",
    active_roi = "A",
    identity = "dataset::section-a",
    generation = 1L,
    dataset = "dataset-a",
    snapshot_identity = "snapshot-a",
    section = "section-a"
  )
  expect_false("coverage" %in% names(scene))
  expect_identical(scene$dataset, "dataset-a")
  expect_identical(scene$snapshotIdentity, "snapshot-a")
  expect_identical(scene$section, "section-a")
  expect_identical(scene$controls$point_opacity, 0.65)
  expect_identical(scene$controls$point_size, 6)
  expect_identical(scene$layout, "separate")
  expect_identical(scene$activeRoi, "A")
  expect_identical(scene$roiBounds, preview$roi_bounds)
  expect_named(scene$roiImages, c("A", "B"))
  expect_identical(scene$roiImages$B[[1L]]$controls$image_opacity, 0.7)
})

test_that("spatial Canvas sends each image source only once", {
  source_key <- strrep("a", 32L)
  image <- builder_alignment_record(
    source = list(name = "large.jpg", type = "image/jpeg"),
    source_uri = "data:image/jpeg;base64,LARGE",
    uri = "data:image/jpeg;base64,LARGE",
    base_bounds = list(xmin = 0, xmax = 5, ymin = 0, ymax = 5),
    section = list(id = "section-a", kind = "spatial")
  )
  image$source_content_md5 <- source_key
  scene <- builder_spatial_canvas_scene(
    preview = list(
      available = TRUE,
      capped = FALSE,
      spatial = data.frame(
        x = c(1, 2),
        y = c(3, 4),
        cell_barcode = c("cell-a", "cell-b"),
        group = c("A", "B")
      ),
      coordinate_frame = list(xmin = 0, xmax = 5, ymin = 0, ymax = 5)
    ),
    colors = character(),
    record = image,
    roi_images = list(A = list(a = image), B = list(b = image)),
    identity = "dataset::section-a",
    generation = 1L
  )

  first <- builder_spatial_canvas_cache_sources(scene, character())
  first_uris <- c(
    list(first$scene$image$uri),
    unlist(
      lapply(first$scene$roiImages, function(images) {
        lapply(images, `[[`, "uri")
      }),
      recursive = FALSE
    )
  )
  expect_identical(sum(!vapply(first_uris, is.null, logical(1))), 1L)
  expect_identical(first$known, source_key)

  second <- builder_spatial_canvas_cache_sources(scene, first$known)
  second_uris <- c(
    list(second$scene$image$uri),
    unlist(
      lapply(second$scene$roiImages, function(images) {
        lapply(images, `[[`, "uri")
      }),
      recursive = FALSE
    )
  )
  expect_false(any(!vapply(second_uris, is.null, logical(1))))
})

test_that("alignment preview resolves layer membership without expression data", {
  preview_source <- paste(
    deparse(body(builder_alignment_preview_model)),
    collapse = "\n"
  )

  expect_false(grepl(".getExpressionMatrix(", preview_source, fixed = TRUE))
  expect_match(
    preview_source,
    "builder_alignment_layer_cells(",
    fixed = TRUE
  )
})

test_that("Trekker alignment preview uses its physical and transcriptome spaces", {
  skip_if_not_installed("SeuratObject")
  object <- builder_content_spatial_example_object()
  methods::slot(object, "images", check = FALSE) <- list()
  payload <- .builder_content_spatial_demo_payload()
  object@misc$trekker <- payload

  model <- builder_alignment_preview_model(
    object,
    default_projection = "pca",
    section_id = "trekker",
    max_cells = 4L
  )

  expect_true(model$available)
  expect_identical(model$projection_name, "Trekker UMAP")
  expect_identical(model$section$kind, "trekker")
  expect_identical(
    model$transcriptome$cell_barcode,
    model$spatial$cell_barcode
  )
  expect_lte(nrow(model$spatial), 4L)
  expect_false(anyNA(model$spatial[, c("x", "y", "group")]))
})

test_that("Trekker alignment uses the selected Viewer Group", {
  skip_if_not_installed("SeuratObject")
  object <- builder_content_spatial_example_object()
  methods::slot(object, "images", check = FALSE) <- list()
  payload <- .builder_content_spatial_demo_payload()
  payload$barcodes <- colnames(object)[seq_along(payload$barcodes)]
  object@misc$trekker <- payload
  object$alignment_group <- rep(
    c("Type A", "Type B"),
    length.out = ncol(object)
  )

  model <- builder_alignment_preview_model(
    object,
    group = "alignment_group",
    section_id = "trekker",
    max_cells = length(payload$barcodes)
  )

  expected <- as.character(object$alignment_group)[seq_along(payload$barcodes)]
  expect_identical(model$spatial$group, expected)
  expect_identical(model$transcriptome$group, expected)
})

test_that("Trekker full-control commits preserve images without coordinate drafts", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("png")

  image_path <- tempfile(fileext = ".png")
  on.exit(unlink(image_path), add = TRUE)
  write_dummy_png(image_path)
  image <- builder_read_image(image_path)
  expect_null(image$error)
  parameters <- builder_alignment_defaults()
  parameters$scale <- 0.001
  record <- builder_alignment_record(
    source = list(name = "trekker.png", type = "image/png"),
    base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    parameters = parameters,
    image_geometry = image,
    section = list(id = "trekker", kind = "trekker"),
    source_path = image$source_path
  )
  record$source_content_md5 <- image$source_content_md5
  alternate <- record
  alternate$source$name <- "trekker-alternate.png"
  alternate$image_label <- "Alternate"
  alternate$scale <- 0.01
  alternate$bounds <- builder_alignment_transform_bounds(
    alternate$base_bounds,
    .builder_alignment_parameters(alternate)
  )
  entry <- list(
    id = "dataset-a",
    snapshot = list(
      path = "/private/dataset-a",
      owner_token = "owner-a",
      object_md5 = strrep("a", 32L)
    ),
    profile = list(
      images = character(),
      extras = list(list(key = "trekker", found = TRUE))
    ),
    settings = list(
      name = "Dataset A",
      images = list(
        trekker = list(
          Histology = record,
          Alternate = alternate
        )
      ),
      default_group = "cluster",
      default_projection = "umap",
      palette = "cerebro"
    )
  )
  preview <- list(
    available = TRUE,
    bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    section = list(id = "trekker", kind = "trekker", unit = "microns"),
    projection_name = "Trekker UMAP",
    capped = FALSE,
    transcriptome = data.frame(
      cell_barcode = c("cell-a", "cell-b"),
      x = c(-1, 1),
      y = c(-1, 1),
      group = c("A", "B"),
      stringsAsFactors = FALSE
    ),
    spatial = data.frame(
      cell_barcode = c("cell-a", "cell-b"),
      x = c(2, 8),
      y = c(3, 7),
      group = c("A", "B"),
      stringsAsFactors = FALSE
    )
  )
  current_entry <- shiny::reactiveVal(entry)
  current <- shiny::reactiveVal(entry$id)
  alignment_preview <- shiny::reactiveVal(preview)
  input_messages <- list()

  shiny::testServer(
    function(input, output, session) {
      session$sendInputMessage <- function(input_id, message) {
        input_messages[[length(input_messages) + 1L]] <<- list(
          id = input_id,
          message = message
        )
        invisible()
      }
      alignment <- builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = current,
        entry_of = function(id) current_entry(),
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) TRUE,
        commit_images = function(updated, images) {
          updated$settings$images <- images
          current_entry(updated)
          invisible(updated)
        },
        alignment_preview = alignment_preview,
        spatial_coords = shiny::reactiveVal(NULL)
      )
    },
    {
      session$flushReact()
      alignment_preview(preview)
      session$flushReact()
      scene <- alignment$canvas_contract()
      expect_identical(alignment$draft()$scale, 0.001)
      expect_identical(alignment$active_image(), "Histology")

      slider_updates <- Filter(
        function(item) identical(item$id, "enhance-img_scale"),
        input_messages
      )
      number_updates <- Filter(
        function(item) identical(item$id, "enhance-img_scale_number"),
        input_messages
      )
      expect_gt(length(slider_updates), 0L)
      expect_gt(length(number_updates), 0L)
      expect_identical(
        as.numeric(tail(slider_updates, 1L)[[1L]]$message$value),
        0.001
      )
      expect_identical(
        as.numeric(tail(number_updates, 1L)[[1L]]$message$value),
        0.001
      )

      session$setInputs(`enhance-active_image` = "Alternate")
      session$flushReact()
      expect_identical(alignment$active_image(), "Alternate")
      expect_identical(alignment$draft()$scale, 0.01)
      alternate_slider_updates <- Filter(
        function(item) identical(item$id, "enhance-img_scale"),
        input_messages
      )
      alternate_number_updates <- Filter(
        function(item) identical(item$id, "enhance-img_scale_number"),
        input_messages
      )
      expect_identical(
        as.numeric(tail(alternate_slider_updates, 1L)[[1L]]$message$value),
        0.01
      )
      expect_identical(
        as.numeric(tail(alternate_number_updates, 1L)[[1L]]$message$value),
        0.01
      )

      session$setInputs(`enhance-active_image` = "Histology")
      session$flushReact()
      expect_identical(alignment$active_image(), "Histology")
      expect_identical(alignment$draft()$scale, 0.001)
      restored_slider_updates <- Filter(
        function(item) identical(item$id, "enhance-img_scale"),
        input_messages
      )
      restored_number_updates <- Filter(
        function(item) identical(item$id, "enhance-img_scale_number"),
        input_messages
      )
      expect_identical(
        as.numeric(tail(restored_slider_updates, 1L)[[1L]]$message$value),
        0.001
      )
      expect_identical(
        as.numeric(tail(restored_number_updates, 1L)[[1L]]$message$value),
        0.001
      )

      scene <- alignment$canvas_contract()
      controls <- scene$controls
      controls$scale <- 0.01
      controls$coordinateRotation <- 72
      session$setInputs(
        builder_spatial_alignment_controls = list(
          dataset = scene$dataset,
          snapshotIdentity = scene$snapshotIdentity,
          section = scene$section,
          roi = "",
          image = scene$activeImage,
          viewKey = scene$viewKey,
          generation = scene$generation,
          resetToken = scene$resetToken,
          sequence = 1,
          controls = controls
        )
      )
      session$flushReact()

      expect_identical(
        current_entry()$settings$images$trekker$Histology$scale,
        0.01
      )
      expect_length(alignment$coordinate_drafts(), 0L)
      expect_false("roi_coordinates" %in% names(alignment$pending_drafts()))
      expect_identical(
        alignment$canvas_contract()$controls$coordinateRotation,
        0
      )

      ## A late Shiny echo from IonRangeSlider must not replace the browser's
      ## accepted full-control state with the slider's coarser/default value.
      session$setInputs(`enhance-img_scale` = 0.02)
      session$elapse(300)
      session$flushReact()
      expect_identical(
        current_entry()$settings$images$trekker$Histology$scale,
        0.01
      )
      expect_identical(alignment$draft()$scale, 0.01)
    }
  )
})

test_that("alignment preview fails safely when no paired spaces exist", {
  skip_if_not_installed("SeuratObject")
  object <- builder_content_spatial_example_object()
  methods::slot(object, "images", check = FALSE) <- list()
  object@misc$trekker <- NULL

  model <- builder_alignment_preview_model(object, default_projection = "pca")
  expect_false(model$available)
  expect_match(model$message, "Spatial or Trekker", fixed = TRUE)
})

test_that("default image fit preserves aspect ratio inside the viewport", {
  fitted <- builder_alignment_fit_bounds(
    list(xmin = 0, xmax = 100, ymin = 0, ymax = 100),
    c(width = 200, height = 100)
  )
  expect_equal(fitted, list(xmin = 0, xmax = 100, ymin = 25, ymax = 75))
  expect_equal(
    (fitted$xmax - fitted$xmin) / (fitted$ymax - fitted$ymin),
    2
  )
  expect_gte(fitted$xmin, 0)
  expect_lte(fitted$xmax, 100)
  expect_gte(fitted$ymin, 0)
  expect_lte(fitted$ymax, 100)
})

test_that("image fit can use the displayed extent of rotated coordinates", {
  rotated <- builder_alignment_rotated_bounds(
    list(xmin = 0, xmax = 20, ymin = 0, ymax = 100),
    rotation = 45
  )

  expect_equal(rotated$xmax - rotated$xmin, 120 / sqrt(2))
  expect_equal(rotated$ymax - rotated$ymin, 120 / sqrt(2))
  expect_equal(
    c((rotated$xmin + rotated$xmax) / 2, (rotated$ymin + rotated$ymax) / 2),
    c(10, 50)
  )
})

test_that("default image fit contains decimal bounds", {
  bounds <- list(
    xmin = 17.52,
    xmax = 4151.96,
    ymin = 3.92,
    ymax = 3173.76
  )
  fitted <- builder_alignment_fit_bounds(
    bounds,
    c(width = 320, height = 240)
  )
  expect_gte(fitted$xmin, bounds$xmin)
  expect_lte(fitted$xmax, bounds$xmax)
  expect_gte(fitted$ymin, bounds$ymin)
  expect_lte(fitted$ymax, bounds$ymax)
})

test_that("centering aligns an existing image with the active viewport", {
  record <- builder_alignment_record(
    source = list(name = "tissue.png", type = "image/png"),
    source_uri = "data:image/png;base64,SOURCE",
    uri = "data:image/png;base64,DISPLAY",
    base_bounds = list(xmin = 0, xmax = 20, ymin = 10, ymax = 20),
    parameters = list(scale = 1.5, rotation = 30),
    section = list(id = "fov", kind = "spatial")
  )

  centered <- builder_alignment_center(
    record,
    list(xmin = 100, xmax = 200, ymin = 40, ymax = 80)
  )

  expect_equal(centered$dx, 140)
  expect_equal(centered$dy, 45)
  expect_equal(centered$scale, 1.5)
  expect_equal(centered$rotation, 30)
})

test_that("canonical alignment transform is deterministic and complete", {
  original <- list(xmin = 0, xmax = 100, ymin = 25, ymax = 75)
  parameters <- list(
    dx = 10,
    dy = -5,
    scale = 1.5,
    rotation = 30,
    flip_x = TRUE,
    flip_y = FALSE,
    image_opacity = 0.7,
    point_opacity = 0.9,
    point_size = 6
  )
  record <- builder_alignment_record(
    source = list(name = "section-a.png", type = "image/png"),
    source_uri = "data:image/png;base64,AAAA",
    uri = "data:image/png;base64,BBBB",
    base_bounds = original,
    parameters = parameters,
    section = list(id = "section-a", kind = "spatial")
  )

  expect_named(
    record,
    c(
      "source",
      "source_uri",
      "uri",
      "base_bounds",
      "bounds",
      "dx",
      "dy",
      "scale",
      "rotation",
      "flip_x",
      "flip_y",
      "image_opacity",
      "point_opacity",
      "point_size",
      "section_id",
      "section_kind"
    ),
    ignore.order = TRUE
  )
  expect_false("saved" %in% names(record))
  expect_equal(
    record$bounds,
    list(xmin = -15, xmax = 135, ymin = 7.5, ymax = 82.5)
  )
  expect_identical(
    builder_alignment_transform_bounds(original, parameters),
    record$bounds
  )
  expect_identical(
    builder_alignment_transform_bounds(original, parameters),
    builder_alignment_transform_bounds(original, parameters)
  )
})

test_that("rotated alignment bounds preserve one data-unit scale per image pixel", {
  base_bounds <- list(xmin = 0, xmax = 92, ymin = 0, ymax = 56)
  image_geometry <- list(
    source_width = 920,
    source_height = 560,
    extent_width = 1060,
    extent_height = 870
  )

  oriented <- builder_alignment_oriented_bounds(
    base_bounds,
    image_geometry
  )

  expect_equal(
    (oriented$xmax - oriented$xmin) / image_geometry$extent_width,
    (oriented$ymax - oriented$ymin) / image_geometry$extent_height,
    tolerance = 1e-12
  )
  expect_equal(
    c(
      x = (oriented$xmin + oriented$xmax) / 2,
      y = (oriented$ymin + oriented$ymax) / 2
    ),
    c(x = 46, y = 28),
    tolerance = 1e-12
  )

  record <- builder_alignment_record(
    source = list(name = "directional.png", type = "image/png"),
    source_uri = "data:image/png;base64,SOURCE",
    uri = "data:image/png;base64,ROTATED",
    base_bounds = base_bounds,
    parameters = list(dx = 7, dy = -3, scale = 1.2, rotation = -23),
    image_geometry = image_geometry,
    section = list(id = "FOV_A", kind = "spatial")
  )
  expected <- builder_adjust_bounds(oriented, dx = 7, dy = -3, scale = 1.2)
  expect_equal(record$bounds, expected, tolerance = 1e-12)
})

test_that("reset preserves the section image identity", {
  defaults <- builder_alignment_defaults()
  first <- builder_alignment_record(
    source = list(name = "first.png", type = "image/png"),
    source_uri = "data:image/png;base64,FIRST",
    uri = "data:image/png;base64,FIRST",
    base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    parameters = modifyList(
      defaults,
      list(
        dx = 4,
        rotation = 20,
        flip_x = TRUE,
        image_opacity = 0.4,
        point_opacity = 0.5,
        point_size = 7
      )
    ),
    section = list(id = "first", kind = "spatial")
  )
  reset <- builder_alignment_reset(first)
  expect_identical(reset$source_uri, first$source_uri)
  expect_identical(reset$base_bounds, first$base_bounds)
  expect_identical(reset$dx, 0)
  expect_identical(reset$rotation, 0)
  expect_identical(reset$point_opacity, first$point_opacity)
  expect_identical(reset$point_size, first$point_size)
  expect_identical(reset$bounds, first$base_bounds)
  expect_false("saved" %in% names(reset))
})

test_that("legacy image records drop saved flags and gain canonical defaults", {
  legacy <- list(
    uri = "data:image/png;base64,AAAA",
    bounds = list(xmin = 0, xmax = 4, ymin = 0, ymax = 3),
    saved = FALSE
  )
  normalized <- builder_alignment_normalize(legacy, section_id = "fov")
  expect_false("saved" %in% names(normalized))
  expect_identical(normalized$base_bounds, legacy$bounds)
  expect_identical(normalized$section_id, "fov")
  expect_identical(normalized$point_size, 5)
})

test_that("ROI image scope is complete and non-empty", {
  legacy <- list(
    uri = "data:image/png;base64,AAAA",
    bounds = list(xmin = 0, xmax = 4, ymin = 0, ymax = 3)
  )

  expect_error(
    builder_alignment_normalize(c(legacy, list(roi_field = "sample_roi"))),
    "roi_field and roi_value"
  )
  expect_error(
    builder_alignment_normalize(c(
      legacy,
      list(roi_field = "sample_roi", roi_value = "")
    )),
    "non-empty"
  )
})

test_that("named spatial image collections normalize without losing labels", {
  record <- function(section, filename) {
    builder_alignment_record(
      source = list(name = filename, type = "image/png", size = 4),
      source_uri = "data:image/png;base64,AAAA",
      uri = "data:image/png;base64,AAAA",
      base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      section = list(id = section, kind = "spatial")
    )
  }
  images <- list(
    section_a = list(
      `H&E` = record("section_a", "H&E.png"),
      DAPI = record("section_a", "DAPI.png")
    )
  )

  normalized <- builder_image_collection_normalize(images)
  expect_named(normalized, "section_a")
  expect_named(normalized$section_a, c("H&E", "DAPI"))
  expect_length(builder_image_collection_flatten(normalized), 2L)
  expect_identical(
    vapply(
      builder_image_collection_flatten(normalized),
      `[[`,
      "",
      "image_label"
    ),
    c("H&E", "DAPI")
  )
  expect_error(
    builder_image_collection_normalize(list(
      section_a = setNames(
        list(record(
          "section_a",
          "bad.png"
        )),
        ""
      )
    )),
    "non-empty"
  )
  duplicate <- structure(
    list(record("section_a", "A.png"), record("section_a", "B.png")),
    names = c("H&E", "H&E")
  )
  expect_error(
    builder_image_collection_normalize(list(section_a = duplicate)),
    "unique"
  )

  legacy <- list(section_a = record("section_a", "H&E.png"))
  upgraded <- builder_image_collection_normalize(legacy)
  expect_named(upgraded$section_a, "H&E.png")
})

test_that("named spatial image actions preserve unaffected records", {
  record <- function(section, filename, dx = 0) {
    builder_alignment_record(
      source = list(name = filename, type = "image/png", size = 4),
      source_uri = paste0("data:image/png;base64,", filename),
      uri = paste0("data:image/png;base64,", filename),
      base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      parameters = list(dx = dx),
      section = list(id = section, kind = "spatial")
    )
  }
  he <- record("section_a", "he.png", dx = 3)
  dapi <- record("section_a", "dapi.png")
  images <- builder_image_collection_normalize(list(
    section_a = list(`H&E` = he, DAPI = dapi)
  ))
  expect_named(images$section_a, c("H&E", "DAPI"))
  he$image_label <- "H&E"
  dapi$image_label <- "DAPI"
  expect_identical(images$section_a[["H&E"]], he)

  renamed <- builder_image_collection_rename(
    images,
    "section_a",
    "DAPI",
    "IF"
  )
  expect_named(renamed$section_a, c("H&E", "IF"))
  renamed_dapi <- dapi
  renamed_dapi$image_label <- "IF"
  expect_identical(renamed$section_a[["IF"]], renamed_dapi)
  expect_identical(renamed$section_a[["H&E"]], he)
  expect_error(
    builder_image_collection_rename(renamed, "section_a", "IF", "H&E"),
    "unique"
  )

  removed <- builder_image_collection_remove(renamed, "section_a", "H&E")
  expect_named(removed$section_a, "IF")
  expect_identical(removed$section_a$IF, renamed_dapi)
})

test_that("spatial image display labels are unique only within one ROI", {
  scoped_record <- function(roi) {
    value <- builder_alignment_record(
      source = list(name = "same.png", type = "image/png", size = 4),
      source_uri = paste0("data:image/png;base64,", roi),
      uri = paste0("data:image/png;base64,", roi),
      base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      section = list(id = "section_a", kind = "spatial")
    )
    value$image_label <- "same.png"
    value$roi_field <- "sample_roi"
    value$roi_value <- roi
    value
  }

  images <- builder_image_collection_normalize(list(
    section_a = list(
      `same.png` = scoped_record("lesion"),
      `same.png.1` = scoped_record("border")
    )
  ))
  expect_identical(
    names(builder_image_collection_choices(images, "section_a", "lesion")),
    "same.png"
  )
  expect_identical(
    names(builder_image_collection_choices(images, "section_a", "border")),
    "same.png"
  )

  duplicate <- images
  duplicate$section_a[["same.png.2"]] <- scoped_record("lesion")
  expect_error(builder_image_collection_normalize(duplicate), "same ROI")
})

test_that("snapshot drop acknowledgement clears every preview cache", {
  imports <- paste(
    readLines(
      builder_spatial_test_inst_path("builder", "server", "imports.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(imports, "builder_preview_cache_drop_dataset(", fixed = TRUE)
  expect_match(
    imports,
    "builder_spatial_preview_cache_drop_dataset(",
    fixed = TRUE
  )
  expect_match(imports, "projection_previews(", fixed = TRUE)
  expect_match(imports, "trajectory_previews(", fixed = TRUE)
  expect_match(imports, "spatial_previews(", fixed = TRUE)
})

test_that("coordinate drafts stay partitioned and reject stale browser events", {
  drafts <- list()
  first <- builder_coordinate_drafts_put(
    drafts,
    dataset = "dataset-a",
    snapshot_identity = "snapshot-a",
    section = "fov-a",
    spec = list(rotation_degrees = 37.5, scale = 1),
    sequence = 4
  )
  expect_true(first$accepted)

  second <- builder_coordinate_drafts_put(
    first$drafts,
    dataset = "dataset-a",
    snapshot_identity = "snapshot-a",
    section = "fov-b",
    spec = list(rotation_degrees = -12, scale = 1),
    sequence = 5
  )
  stale <- builder_coordinate_drafts_put(
    second$drafts,
    dataset = "dataset-a",
    snapshot_identity = "snapshot-a",
    section = "fov-a",
    spec = list(rotation_degrees = 0, scale = 1),
    sequence = 3
  )

  expect_false(stale$accepted)
  expect_identical(
    builder_coordinate_drafts_get(stale$drafts, "dataset-a", "fov-a")$spec,
    list(schema_version = 1L, rotation_degrees = 37.5, scale = 1)
  )
  expect_identical(
    builder_coordinate_drafts_get(stale$drafts, "dataset-a", "fov-b")$spec,
    list(schema_version = 1L, rotation_degrees = -12, scale = 1)
  )
})

test_that("coordinate draft pruning removes deleted and stale snapshots", {
  drafts <- list()
  drafts <- builder_coordinate_drafts_put(
    drafts,
    "dataset-a",
    "old-snapshot",
    "fov-a",
    list(rotation_degrees = 10, scale = 1),
    1
  )$drafts
  drafts <- builder_coordinate_drafts_put(
    drafts,
    "dataset-b",
    "snapshot-b",
    "fov-b",
    list(rotation_degrees = 20, scale = 1),
    2
  )$drafts
  drafts <- builder_coordinate_drafts_put(
    drafts,
    "dataset-c",
    "snapshot-c",
    "fov-c",
    list(rotation_degrees = 30, scale = 1),
    3
  )$drafts

  pruned <- builder_coordinate_drafts_prune(
    drafts,
    list(
      list(id = "dataset-a", snapshot_identity = "new-snapshot"),
      list(id = "dataset-b", snapshot_identity = "snapshot-b")
    )
  )

  expect_null(builder_coordinate_drafts_get(
    pruned$drafts,
    "dataset-a",
    "fov-a"
  ))
  expect_identical(
    builder_coordinate_drafts_get(
      pruned$drafts,
      "dataset-b",
      "fov-b"
    )$spec$rotation_degrees,
    20
  )
  expect_null(builder_coordinate_drafts_get(
    pruned$drafts,
    "dataset-c",
    "fov-c"
  ))
  expect_setequal(pruned$removed, c("dataset-a::fov-a", "dataset-c::fov-c"))
})

test_that("coordinate draft pruning treats artifact entries as snapshotless", {
  drafts <- builder_coordinate_drafts_put(
    list(),
    "dataset-a",
    "old-snapshot",
    "fov-a",
    list(rotation_degrees = 10, scale = 1),
    1
  )$drafts

  pruned <- builder_coordinate_drafts_prune(
    drafts,
    list(list(id = "dataset-a", snapshot_identity = NULL))
  )

  expect_null(pruned$drafts[["dataset-a"]])
  expect_identical(pruned$removed, "dataset-a::fov-a")
})

test_that("artifact entries initialize Spatial when their source attaches", {
  skip_if_not_installed("shiny")
  entry <- list(
    id = "dataset-a",
    load_state = "artifact_ready",
    snapshot = NULL,
    profile = list(images = "fov-a", extras = list()),
    settings = list(
      name = "Dataset A",
      images = list(
        fov_a = list(
          `H&E` = list(
            project_asset = list(path = "spatial-assets/image.png"),
            bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10)
          )
        )
      )
    )
  )
  current_entry <- shiny::reactiveVal(entry)
  current <- shiny::reactiveVal(entry$id)

  shiny::testServer(
    function(input, output, session) {
      alignment <- builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = current,
        entry_of = function(id) current_entry(),
        entries = shiny::reactive(list(current_entry())),
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) TRUE,
        commit_images = function(entry, images) {
          stop("artifact entry committed editable images")
        },
        alignment_preview = shiny::reactiveVal(NULL),
        spatial_coords = shiny::reactiveVal(NULL)
      )
    },
    {
      session$flushReact()
      expect_null(alignment$active_section())
      expect_null(alignment$active_image())
      expect_null(alignment$draft())

      expect_true(alignment$restore_project_selection(list(
        dataset = "dataset-a",
        section = "fov-a",
        image = "H&E"
      )))
      session$flushReact()
      expect_null(alignment$active_section())
      expect_null(alignment$active_image())

      source <- current_entry()
      source$load_state <- "loaded"
      source$snapshot <- list(
        path = "/private/dataset-a",
        owner_token = "owner-a",
        object_md5 = strrep("a", 32L),
        serialization = "qs2"
      )
      current_entry(source)
      session$flushReact()

      expect_identical(alignment$active_section(), "fov-a")
    }
  )
})

test_that("points-only Spatial FOV appearance persists without an image", {
  skip_if_not_installed("shiny")
  entry <- list(
    id = "dataset-a",
    snapshot = list(
      path = "/private/dataset-a",
      owner_token = "owner-a",
      object_md5 = strrep("a", 32L)
    ),
    profile = list(images = "fov-a", extras = list()),
    settings = list(
      name = "Dataset A",
      images = list(),
      default_group = "cluster",
      default_projection = "umap",
      palette = "cerebro",
      spatial_point_appearance = list(
        "fov-a" = list(point_opacity = 0.65, point_size = 6)
      )
    )
  )
  current_entry <- shiny::reactiveVal(entry)
  current <- shiny::reactiveVal(entry$id)
  preview <- list(
    available = TRUE,
    bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    section = list(id = "fov-a", kind = "spatial", unit = "pixels"),
    projection_name = "umap",
    capped = FALSE,
    transcriptome = data.frame(
      cell_id = c("cell-a", "cell-b"),
      x = c(-1, 1),
      y = c(-1, 1),
      group = c("A", "B")
    ),
    spatial = data.frame(
      cell_id = c("cell-a", "cell-b"),
      x = c(2, 8),
      y = c(3, 7),
      group = c("A", "B")
    )
  )
  preview_state <- shiny::reactiveVal(NULL)

  shiny::testServer(
    function(input, output, session) {
      alignment <- builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = current,
        entry_of = function(id) current_entry(),
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) TRUE,
        commit_images = function(updated, images) {
          updated$settings$images <- images
          current_entry(updated)
          invisible(updated)
        },
        alignment_preview = preview_state,
        spatial_coords = shiny::reactiveVal(NULL)
      )
    },
    {
      session$flushReact()
      session$flushReact()
      preview_state(preview)
      session$flushReact()
      expect_identical(alignment$active_section(), "fov-a")
      expect_identical(
        alignment$point_appearance(),
        list(opacity = 0.65, size = 6)
      )
      scene_generation <- alignment$canvas_contract()$generation
      renamed <- current_entry()
      renamed$settings$name <- "Renamed without changing Spatial"
      current_entry(renamed)
      session$flushReact()
      expect_identical(
        alignment$canvas_contract()$generation,
        scene_generation
      )
      scene <- alignment$canvas_contract()
      controls <- scene$controls
      controls$point_opacity <- 0.7
      session$setInputs(
        builder_spatial_alignment_controls = list(
          dataset = "dataset-a",
          snapshotIdentity = .builder_worker_identity(
            current_entry()$snapshot
          ),
          section = "fov-a",
          roi = "",
          image = "",
          viewKey = scene$viewKey,
          generation = scene$generation,
          resetToken = scene$resetToken,
          sequence = 1,
          controls = controls
        )
      )
      session$flushReact()

      expect_identical(
        current_entry()$settings$spatial_point_appearance[["fov-a"]],
        list(point_opacity = 0.7, point_size = 6)
      )

      identity <- .builder_worker_identity(current_entry()$snapshot)
      session$setInputs(
        builder_spatial_coordinate_draft = list(
          dataset = "dataset-a",
          snapshotIdentity = identity,
          section = "fov-a",
          rotationDegrees = 35,
          sequence = 1
        )
      )
      before_reset <- alignment$canvas_contract()$resetToken
      session$setInputs(`enhance-reset_coordinate_transform` = 1L)
      session$flushReact()

      defaults <- builder_alignment_defaults()
      expect_identical(
        alignment$coordinate_drafts()[["dataset-a"]][["fov-a"]]$spec,
        list(schema_version = 1L, rotation_degrees = 0, scale = 1)
      )
      expect_identical(
        current_entry()$settings$spatial_point_appearance[["fov-a"]],
        defaults[c("point_opacity", "point_size")]
      )
      expect_identical(
        alignment$point_appearance(),
        list(opacity = defaults$point_opacity, size = defaults$point_size)
      )
      expect_gt(alignment$canvas_contract()$resetToken, before_reset)
    }
  )
})

test_that("coordinate drafts batch materialize transforms without image state", {
  record <- function(section) {
    builder_alignment_record(
      source = list(name = paste0(section, ".png")),
      source_uri = "data:image/png;base64,AA==",
      uri = "data:image/png;base64,AA==",
      base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      section = list(id = section, kind = "spatial")
    )
  }
  entry <- list(
    id = "dataset-a",
    settings = list(
      spatial_coordinate_transforms = list(
        "fov-b" = list(rotation_degrees = 19, scale = 1)
      ),
      images = list(
        "fov-a" = list(image = record("fov-a")),
        "fov-b" = list(image = record("fov-b")),
        "fov-c" = list(image = record("fov-c"))
      )
    )
  )
  drafts <- list()
  drafts <- builder_coordinate_drafts_put(
    drafts,
    "dataset-a",
    "snapshot-a",
    "fov-a",
    list(rotation_degrees = 42, scale = 1),
    1
  )$drafts
  drafts <- builder_coordinate_drafts_put(
    drafts,
    "dataset-a",
    "snapshot-a",
    "fov-b",
    list(rotation_degrees = 0, scale = 1),
    2
  )$drafts

  applied <- builder_coordinate_drafts_apply_entry(
    entry,
    drafts[["dataset-a"]],
    snapshot_identity = "snapshot-a"
  )

  expect_true(applied$changed)
  expect_setequal(applied$sections, c("fov-a", "fov-b"))
  expect_identical(
    applied$entry$settings$spatial_coordinate_transforms[["fov-a"]],
    list(schema_version = 1L, rotation_degrees = 42, scale = 1)
  )
  expect_null(applied$entry$settings$spatial_coordinate_transforms[["fov-b"]])
  expect_false(
    "saved" %in%
      names(
        applied$entry$settings$images[["fov-a"]]$image
      )
  )
  expect_false(
    "saved" %in%
      names(
        applied$entry$settings$images[["fov-b"]]$image
      )
  )
  expect_false(
    "saved" %in%
      names(
        applied$entry$settings$images[["fov-c"]]$image
      )
  )
})

test_that("ROI drafts materialize coordinate and point settings together", {
  entry <- list(
    id = "dataset-a",
    settings = list(spatial_roi_settings = list())
  )
  coordinates <- list(
    "fov-a" = list(
      lesion = list(
        dataset = "dataset-a",
        snapshot_identity = "snapshot-a",
        section = "fov-a",
        roi = "lesion",
        spec = list(rotation_degrees = 37.5, scale = 1),
        sequence = 1
      )
    )
  )
  appearance <- list(
    "fov-a" = list(
      lesion = list(
        dataset = "dataset-a",
        snapshot_identity = "snapshot-a",
        section = "fov-a",
        roi = "lesion",
        point_opacity = 0.65,
        point_size = 7
      )
    )
  )

  applied <- builder_roi_drafts_apply_entry(
    entry,
    coordinates,
    appearance,
    snapshot_identity = "snapshot-a"
  )

  expect_true(applied$changed)
  expect_identical(
    applied$entry$settings$spatial_roi_settings[["fov-a"]][["lesion"]],
    list(rotation_degrees = 37.5, point_opacity = 0.65, point_size = 7)
  )
})

test_that("coordinate release events stay light until one batched materialization", {
  skip_if_not_installed("shiny")
  entry <- list(
    id = "dataset-a",
    snapshot = list(
      path = "/private/dataset-a",
      owner_token = "owner-a",
      object_md5 = strrep("a", 32L)
    ),
    profile = list(images = c("fov-a", "fov-b"), extras = list()),
    settings = list(
      name = "Dataset A",
      images = list(),
      spatial_coordinate_transforms = list(),
      default_group = "cluster",
      default_projection = "umap",
      palette = "cerebro"
    )
  )
  current_entry <- shiny::reactiveVal(entry)
  current <- shiny::reactiveVal(entry$id)
  commit_count <- 0L
  identity <- .builder_worker_identity(entry$snapshot)

  shiny::testServer(
    function(input, output, session) {
      alignment <- builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = current,
        entry_of = function(id) current_entry(),
        entries = shiny::reactive(list(current_entry())),
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) TRUE,
        commit_images = function(updated, images) {
          updated$settings$images <- images
          current_entry(updated)
          commit_count <<- commit_count + 1L
          invisible(updated)
        },
        alignment_preview = shiny::reactiveVal(NULL),
        spatial_coords = shiny::reactiveVal(NULL)
      )
    },
    {
      session$flushReact()
      session$setInputs(
        builder_spatial_coordinate_draft = list(
          dataset = "dataset-a",
          snapshotIdentity = identity,
          section = "fov-a",
          rotationDegrees = 37.5,
          sequence = 2
        )
      )
      session$setInputs(`enhance-active_section` = "fov-b")
      session$setInputs(
        builder_spatial_coordinate_draft = list(
          dataset = "dataset-a",
          snapshotIdentity = identity,
          section = "fov-b",
          rotationDegrees = -12,
          sequence = 3
        )
      )
      session$setInputs(
        builder_spatial_coordinate_draft = list(
          dataset = "dataset-a",
          snapshotIdentity = identity,
          section = "fov-a",
          rotationDegrees = 0,
          sequence = 1
        )
      )
      session$flushReact()

      expect_identical(commit_count, 0L)
      expect_identical(
        alignment$coordinate_drafts()[["dataset-a"]][[
          "fov-a"
        ]]$spec$rotation_degrees,
        37.5
      )
      expect_identical(
        alignment$coordinate_drafts()[["dataset-a"]][[
          "fov-b"
        ]]$spec$rotation_degrees,
        -12
      )

      materialized <- alignment$materialize_coordinate_drafts()
      expect_true(materialized$ok)
      expect_identical(commit_count, 1L)
      expect_identical(
        current_entry()$settings$spatial_coordinate_transforms[[
          "fov-a"
        ]]$rotation_degrees,
        37.5
      )
      expect_identical(
        current_entry()$settings$spatial_coordinate_transforms[[
          "fov-b"
        ]]$rotation_degrees,
        -12
      )
      expect_length(alignment$coordinate_drafts(), 0L)
    }
  )
})

test_that("project restore replaces default coordinate drafts authoritatively", {
  skip_if_not_installed("shiny")
  entry <- list(
    id = "ds1",
    snapshot = list(
      path = "/private/ds1",
      owner_token = "owner-ds1",
      object_md5 = strrep("a", 32L)
    ),
    profile = list(images = "section_a_1_fov_1", extras = list()),
    settings = list(
      name = "Dataset 1",
      images = list(),
      spatial_coordinate_transforms = list(),
      default_group = "cluster",
      default_projection = "umap",
      palette = "cerebro"
    )
  )
  preview <- list(
    available = TRUE,
    bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    section = list(
      id = "section_a_1_fov_1",
      kind = "spatial",
      unit = "pixels"
    ),
    projection_name = "umap",
    capped = FALSE,
    spatial = data.frame(
      cell_barcode = c("cell-a", "cell-b"),
      x = c(2, 8),
      y = c(3, 7),
      group = c("A", "B"),
      stringsAsFactors = FALSE
    )
  )
  current_entry <- shiny::reactiveVal(entry)
  current <- shiny::reactiveVal(entry$id)
  alignment_preview <- shiny::reactiveVal(preview)
  input_messages <- list()
  identity <- .builder_worker_identity(entry$snapshot)

  shiny::testServer(
    function(input, output, session) {
      session$sendInputMessage <- function(input_id, message) {
        input_messages[[length(input_messages) + 1L]] <<- list(
          id = input_id,
          message = message
        )
        invisible()
      }
      alignment <- builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = current,
        entry_of = function(id) current_entry(),
        entries = shiny::reactive(list(current_entry())),
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) TRUE,
        commit_images = function(updated, images) {
          updated$settings$images <- images
          current_entry(updated)
          invisible(updated)
        },
        alignment_preview = alignment_preview,
        spatial_coords = shiny::reactiveVal(NULL)
      )
    },
    {
      session$flushReact()
      alignment_preview(preview)
      session$flushReact()
      initial_scene <- alignment$canvas_contract()
      expect_identical(initial_scene$controls$coordinateRotation, 0)
      session$setInputs(
        builder_spatial_coordinate_draft = list(
          dataset = "ds1",
          snapshotIdentity = identity,
          section = "section_a_1_fov_1",
          rotationDegrees = 0,
          sequence = 1,
          generation = initial_scene$generation
        )
      )
      session$flushReact()

      restored <- current_entry()
      restored$settings$spatial_coordinate_transforms <- list(
        section_a_1_fov_1 = list(rotation_degrees = 66.9, scale = 1)
      )
      restored$snapshot$owner_token <- "owner-ds1-restored"
      restored_identity <- .builder_worker_identity(restored$snapshot)
      expect_identical(
        alignment$coordinate_drafts()[["ds1"]][[
          "section_a_1_fov_1"
        ]]$spec$rotation_degrees,
        0
      )
      alignment$discard_project_drafts("ds1")
      current_entry(restored)
      expect_identical(
        current_entry()$settings$spatial_coordinate_transforms[[
          "section_a_1_fov_1"
        ]]$rotation_degrees,
        66.9
      )
      ## Replacing the source snapshot rebuilds the Spatial scene. Mirror the
      ## current worker response before asserting its replacement scene.
      alignment_preview(NULL)
      session$flushReact()
      alignment_preview(preview)
      session$flushReact()
      session$setInputs(
        builder_spatial_coordinate_draft = list(
          dataset = "ds1",
          snapshotIdentity = identity,
          section = "section_a_1_fov_1",
          rotationDegrees = 0,
          sequence = 2,
          generation = initial_scene$generation
        )
      )
      session$flushReact()
      session$flushReact()

      expect_identical(
        current_entry()$settings$spatial_coordinate_transforms[[
          "section_a_1_fov_1"
        ]]$rotation_degrees,
        66.9
      )
      slider_updates <- Filter(
        function(item) identical(item$id, "enhance-coordinate_rotation"),
        input_messages
      )
      expect_gt(length(slider_updates), 0L)
      expect_identical(
        as.numeric(tail(slider_updates, 1L)[[1L]]$message$value),
        66.9
      )
      expect_identical(
        alignment$canvas_contract()$controls$coordinateRotation,
        66.9
      )
      expect_length(alignment$coordinate_drafts(), 0L)

      restored_scene <- alignment$canvas_contract()
      session$setInputs(
        builder_spatial_coordinate_draft = list(
          dataset = "ds1",
          snapshotIdentity = restored_identity,
          section = "section_a_1_fov_1",
          rotationDegrees = 12.5,
          sequence = 3,
          generation = restored_scene$generation
        )
      )
      session$flushReact()
      expect_identical(
        alignment$coordinate_drafts()[["ds1"]][[
          "section_a_1_fov_1"
        ]]$spec$rotation_degrees,
        12.5
      )
    }
  )
})

test_that("failed coordinate materialization retains its session drafts", {
  skip_if_not_installed("shiny")
  entry <- list(
    id = "dataset-a",
    snapshot = list(
      path = "/private/dataset-a",
      owner_token = "owner-a",
      object_md5 = strrep("a", 32L)
    ),
    profile = list(images = "fov-a", extras = list()),
    settings = list(images = list(), spatial_coordinate_transforms = list())
  )
  current_entry <- shiny::reactiveVal(entry)
  current <- shiny::reactiveVal(entry$id)
  identity <- .builder_worker_identity(entry$snapshot)

  shiny::testServer(
    function(input, output, session) {
      alignment <- builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = current,
        entry_of = function(id) current_entry(),
        entries = shiny::reactive(list(current_entry())),
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) TRUE,
        commit_images = function(entry, images) FALSE,
        alignment_preview = shiny::reactiveVal(NULL),
        spatial_coords = shiny::reactiveVal(NULL)
      )
    },
    {
      session$flushReact()
      session$setInputs(
        builder_spatial_coordinate_draft = list(
          dataset = "dataset-a",
          snapshotIdentity = identity,
          section = "fov-a",
          rotationDegrees = 18,
          sequence = 1
        )
      )
      session$flushReact()

      materialized <- alignment$materialize_coordinate_drafts(notify = FALSE)
      expect_false(materialized$ok)
      expect_identical(
        alignment$coordinate_drafts()[["dataset-a"]][[
          "fov-a"
        ]]$spec$rotation_degrees,
        18
      )
      expect_null(current_entry()$settings$spatial_coordinate_transforms[[
        "fov-a"
      ]])
    }
  )
})

test_that("serialized alignment payload excludes editing bytes and local paths", {
  record <- builder_alignment_record(
    source = list(name = "tissue.png", type = "image/png"),
    source_uri = "data:image/png;base64,SOURCE",
    uri = "data:image/png;base64,DISPLAY",
    base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 5),
    parameters = list(dx = 2, rotation = 15, point_size = 7),
    section = list(id = "fov", kind = "spatial")
  )
  record$datapath <- "/private/tmp/upload.png"
  payload <- builder_alignment_payload(record)

  expect_named(
    payload,
    c(
      "source",
      "builder_managed",
      "dx",
      "dy",
      "scale",
      "rotation",
      "flip_x",
      "flip_y",
      "image_opacity",
      "point_opacity",
      "point_size"
    )
  )
  expect_identical(payload$source, "tissue.png")
  expect_true(payload$builder_managed)
  expect_false(any(grepl("/private/tmp", unlist(payload), fixed = TRUE)))
  expect_false("source_uri" %in% names(payload))
  expect_false("datapath" %in% names(payload))
})

test_that("Builder alignment payload retains its ROI scope", {
  record <- builder_alignment_record(
    source = list(name = "roi-lesion.png", type = "image/png"),
    source_uri = "data:image/png;base64,SOURCE",
    uri = "data:image/png;base64,DISPLAY",
    base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 5),
    section = list(id = "fov", kind = "spatial")
  )
  record$roi_field <- "sample_roi"
  record$roi_value <- "lesion"

  expect_identical(
    builder_alignment_payload(record)[c("roi_field", "roi_value")],
    list(roi_field = "sample_roi", roi_value = "lesion")
  )
})

test_that("Spatial and Trekker alignments are partitioned without collision", {
  spatial <- list(
    fov = list(
      uri = "data:image/png;base64,SPATIAL",
      bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      section_kind = "spatial"
    ),
    trekker = list(
      uri = "data:image/png;base64,TREKKER",
      bounds = list(xmin = 20, xmax = 30, ymin = 40, ymax = 50),
      section_kind = "trekker"
    )
  )
  split <- builder_partition_alignments(spatial)
  expect_identical(names(split$spatial), "fov")
  expect_identical(split$trekker$uri, spatial$trekker$uri)
  expect_false("trekker" %in% names(split$spatial))
})

test_that("real Seurat image coordinates normalize by barcode", {
  skip_if_not_installed("SeuratObject")
  object <- builder_content_spatial_example_object()
  image <- names(object@images)[[1L]]
  raw <- SeuratObject::GetTissueCoordinates(object[[image]])

  contract <- builder_spatial_contract(object, image = image)
  coordinates <- contract$coordinates
  source_index <- match(coordinates$cell_barcode, raw$cell)

  expect_named(coordinates, c("cell_barcode", "x", "y"))
  expect_identical(coordinates$cell_barcode, SeuratObject::Cells(object))
  expect_equal(coordinates$x, raw$x[source_index])
  expect_equal(coordinates$y, raw$y[source_index])
  expect_identical(contract$preview, contract$export)

  legacy <- builder_spatial_coords(object, image)
  expect_identical(legacy[[1L]], coordinates$x)
  expect_identical(legacy[[2L]], coordinates$y)
})

test_that("SlideSeq unnamed helper columns do not hide valid coordinates", {
  cells <- c("cell-a", "cell-b")
  coordinates <- data.frame(
    x = c(10, 20),
    y = c(30, 40),
    duplicate_barcode = cells,
    cells = cells,
    row.names = cells,
    check.names = FALSE
  )
  names(coordinates)[[3L]] <- NA_character_

  contract <- builder_spatial_contract(
    coordinates,
    cells,
    barcodes = rownames(coordinates),
    source = "seurat_image"
  )

  expect_identical(contract$coordinate_columns, c(x = "x", y = "y"))
  expect_identical(contract$coordinates$cell_barcode, cells)
  expect_identical(contract$coordinates$x, coordinates$x)
  expect_identical(contract$coordinates$y, coordinates$y)
})

test_that("metadata coordinates require explicit x and y selections", {
  cells <- c("cell-a", "cell-b", "cell-c")
  metadata <- data.frame(
    quality = c(100, 200, 300),
    x = c(1, 2, 3),
    y = c(4, 5, 6),
    row.names = cells
  )

  expect_error(
    builder_spatial_contract(metadata, cells, source = "metadata"),
    "explicit"
  )
  contract <- builder_spatial_contract(
    metadata,
    cells,
    coord_cols = c("x", "y"),
    source = "metadata"
  )
  expect_identical(contract$coordinates$x, metadata$x)
  expect_identical(contract$coordinates$y, metadata$y)
  expect_false(identical(contract$coordinates$x, metadata$quality))
  expect_error(
    builder_spatial_contract(
      metadata,
      cells,
      coord_cols = c("x", "x"),
      source = "metadata"
    ),
    "distinct"
  )

  arbitrary <- data.frame(
    quality = c(9, 8, 7),
    foo = c(11, 12, 13),
    bar = c(21, 22, 23),
    row.names = cells
  )
  explicit <- builder_spatial_contract(
    arbitrary,
    cells,
    coord_cols = c("foo", "bar"),
    source = "metadata"
  )
  expect_identical(explicit$coordinates$x, arbitrary$foo)
  expect_identical(explicit$coordinates$y, arbitrary$bar)
})

test_that("Seurat metadata subclasses fail before row-name dispatch", {
  skip_if_not_installed("SeuratObject")
  touched <- FALSE
  assign(
    "dimnames.builder_seurat_metadata_trap",
    function(value) {
      touched <<- TRUE
      stop("untrusted Seurat metadata dimnames method executed")
    },
    envir = .GlobalEnv
  )
  on.exit(
    rm("dimnames.builder_seurat_metadata_trap", envir = .GlobalEnv),
    add = TRUE
  )

  object <- builder_content_spatial_example_object()
  metadata <- methods::slot(object, "meta.data")
  metadata$x <- seq_len(nrow(metadata))
  metadata$y <- rev(metadata$x)
  class(metadata) <- c("builder_seurat_metadata_trap", "data.frame")
  methods::slot(object, "meta.data", check = FALSE) <- metadata

  expect_error(
    builder_spatial_contract(object, coord_cols = c("x", "y")),
    "unclassed base matrix|exact base data frame"
  )
  expect_false(touched)
})

test_that("Seurat coordinate aliases win over physical column order", {
  cells <- c("cell-a", "cell-b", "cell-c")
  coordinates <- data.frame(
    quality = c(99, 98, 97),
    y = c(40, 50, 60),
    x = c(10, 20, 30),
    cell = rev(cells)
  )

  contract <- builder_spatial_contract(
    coordinates,
    cells,
    barcodes = coordinates$cell,
    source = "seurat_image"
  )
  index <- match(cells, coordinates$cell)

  expect_identical(contract$coordinate_columns, c(x = "x", y = "y"))
  expect_identical(contract$coordinates$cell_barcode, cells)
  expect_identical(contract$coordinates$x, coordinates$x[index])
  expect_identical(contract$coordinates$y, coordinates$y[index])
})

test_that("spatial subsets filter outside rows before preview and export", {
  cells <- c("cell-a", "cell-b", "cell-c")
  metadata <- data.frame(
    foo = c(20, NA_real_, 10),
    bar = c(2, Inf, 1),
    row.names = c("cell-b", "outside", "cell-a")
  )
  contract <- builder_spatial_contract(
    metadata,
    cells,
    coord_cols = c("foo", "bar"),
    source = "metadata"
  )

  expect_identical(contract$match$extra, "outside")
  expect_identical(
    contract$coordinates$cell_barcode,
    c("cell-a", "cell-b")
  )
  expect_identical(contract$coordinates$x, c(10, 20))
  expect_false("outside" %in% contract$preview$cell_barcode)
  expect_false("outside" %in% contract$export$cell_barcode)
  expect_identical(contract$preview, contract$export)
})

test_that("spatial identity and coordinate damage fails closed", {
  cells <- c("cell-a", "cell-b", "cell-c")
  valid <- data.frame(
    x = c(1, 2),
    y = c(3, 4),
    row.names = cells[1:2]
  )

  duplicate <- valid
  attr(duplicate, "row.names") <- c("cell-a", "cell-a")
  expect_error(
    builder_spatial_contract(
      duplicate,
      cells,
      coord_cols = c("x", "y"),
      source = "metadata"
    ),
    "duplicate"
  )

  blank <- valid
  attr(blank, "row.names") <- c("cell-a", "")
  expect_error(
    builder_spatial_contract(
      blank,
      cells,
      coord_cols = c("x", "y"),
      source = "metadata"
    ),
    "blank"
  )

  expect_error(
    builder_spatial_contract(
      valid,
      c("cell-a", "cell-a", "cell-c"),
      coord_cols = c("x", "y"),
      source = "metadata"
    ),
    "duplicate"
  )

  non_finite <- valid
  non_finite$x[[2L]] <- Inf
  expect_error(
    builder_spatial_contract(
      non_finite,
      cells,
      coord_cols = c("x", "y"),
      source = "metadata"
    ),
    "finite"
  )
})

test_that("classed numeric spatial coordinates fail without method dispatch", {
  touched <- FALSE
  assign(
    "as.numeric.builder_coordinate_trap",
    function(value, ...) {
      touched <<- TRUE
      stop("untrusted coordinate conversion executed")
    },
    envir = .GlobalEnv
  )
  on.exit(
    rm("as.numeric.builder_coordinate_trap", envir = .GlobalEnv),
    add = TRUE
  )
  cells <- c("cell-a", "cell-b")
  coordinates <- data.frame(
    x = structure(c(1, 2), class = "builder_coordinate_trap"),
    y = c(3, 4),
    row.names = cells
  )

  expect_error(
    builder_spatial_contract(
      coordinates,
      cells,
      coord_cols = c("x", "y"),
      source = "metadata"
    ),
    "unclassed|base integer|base double"
  )
  expect_false(touched)
})

test_that("classed spatial coordinate tables fail without method dispatch", {
  touched <- FALSE
  method_names <- paste0(
    c("as.data.frame.", "dim.", "dimnames.", "row.names."),
    "builder_table_trap"
  )
  for (method_name in method_names) {
    assign(
      method_name,
      function(value, ...) {
        touched <<- TRUE
        stop("untrusted coordinate table method executed")
      },
      envir = .GlobalEnv
    )
  }
  on.exit(rm(list = method_names, envir = .GlobalEnv), add = TRUE)

  cells <- c("cell-a", "cell-b")
  matrix_table <- matrix(
    c(1, 2, 3, 4),
    nrow = 2L,
    dimnames = list(cells, c("x", "y"))
  )
  class(matrix_table) <- "builder_table_trap"
  data_frame_table <- data.frame(
    x = c(1, 2),
    y = c(3, 4),
    row.names = cells
  )
  class(data_frame_table) <- c("builder_table_trap", "data.frame")

  for (kind in c("matrix", "data_frame")) {
    touched <- FALSE
    coordinate_table <- if (identical(kind, "matrix")) {
      matrix_table
    } else {
      data_frame_table
    }
    expect_error(
      builder_spatial_contract(
        coordinate_table,
        cells,
        coord_cols = c("x", "y"),
        source = "metadata"
      ),
      "unclassed base matrix|exact base data frame",
      info = kind
    )
    expect_false(touched, info = kind)
  }
})

test_that("PNG and JPEG read while TIFF variants give conversion guidance", {
  skip_if_not_installed("png")
  skip_if_not_installed("jpeg")
  directory <- withr::local_tempdir()
  rgb <- array(seq(0, 1, length.out = 27L), dim = c(3L, 3L, 3L))
  png_path <- file.path(directory, "image.png")
  jpeg_path <- file.path(directory, "image.jpeg")
  png::writePNG(rgb, png_path)
  jpeg::writeJPEG(rgb, jpeg_path)

  expect_null(builder_read_image(png_path)$error)
  expect_null(builder_read_image(jpeg_path)$error)

  for (extension in c("tif", "tiff", "ome.tif", "ome.tiff")) {
    path <- file.path(directory, paste0("image.", extension))
    file.create(path)
    got <- builder_read_image(path)
    expect_match(got$error, "convert", ignore.case = TRUE, info = extension)
    expect_match(got$error, "PNG", fixed = TRUE, info = extension)
    expect_match(got$error, "JPEG", fixed = TRUE, info = extension)
  }
})

test_that("uploaded images are retained atomically without a second digest", {
  directory <- withr::local_tempdir()
  source <- file.path(directory, "upload.png")
  target <- file.path(directory, "retained.png")
  writeBin(as.raw(seq_len(64L)), source)

  copied <- builder_retain_uploaded_image(source, target, file.size(source))

  expect_null(copied$error)
  expect_true(file.exists(copied$path))
  expect_identical(
    readBin(copied$path, "raw", n = 64L),
    readBin(source, "raw", n = 64L)
  )

  copy_calls <- 0L
  reused <- builder_retain_uploaded_image(
    source,
    target,
    file.size(source),
    .copy = function(...) {
      copy_calls <<- copy_calls + 1L
      FALSE
    }
  )
  expect_null(reused$error)
  expect_identical(copy_calls, 0L)
})

test_that("uploaded image retention rejects incomplete copies", {
  directory <- withr::local_tempdir()
  source <- file.path(directory, "upload.png")
  target <- file.path(directory, "retained.png")
  writeBin(as.raw(seq_len(64L)), source)

  retained <- builder_retain_uploaded_image(
    source,
    target,
    file.size(source),
    .copy = function(from, to, overwrite = FALSE) {
      writeBin(readBin(from, "raw", n = 8L), to)
      TRUE
    }
  )

  expect_match(retained$error, "retained safely", fixed = TRUE)
  expect_false(file.exists(target))
})

test_that("image headers are read without raster decoding", {
  skip_if_not_installed("png")
  skip_if_not_installed("jpeg")
  directory <- withr::local_tempdir()
  rgb <- array(seq(0, 1, length.out = 27L), dim = c(3L, 3L, 3L))
  png_path <- file.path(directory, "budget.png")
  jpeg_path <- file.path(directory, "budget.jpeg")
  png::writePNG(rgb, png_path)
  jpeg::writeJPEG(rgb, jpeg_path)

  expect_identical(
    builder_image_file_dimensions(png_path, "budget.png"),
    c(width = 3L, height = 3L)
  )
  expect_identical(
    builder_image_file_dimensions(jpeg_path, "budget.jpeg"),
    c(width = 3L, height = 3L)
  )
  expect_match(
    builder_read_image(png_path, max_bytes = 8L)$error,
    "configured file-size limit",
    fixed = TRUE
  )
  expect_false(grepl(
    "readPNG|readJPEG",
    deparse1(body(builder_image_file_dimensions))
  ))
})

test_that("JPEG EXIF orientations 5 through 8 swap display dimensions", {
  skip_if_not_installed("jpeg")
  directory <- withr::local_tempdir()
  source <- file.path(directory, "source.jpeg")
  jpeg::writeJPEG(
    array(seq(0, 1, length.out = 4L * 6L * 3L), dim = c(4L, 6L, 3L)),
    source
  )
  source_bytes <- readBin(source, what = "raw", n = file.size(source))

  for (orientation in 5:8) {
    exif <- as.raw(c(
      0xff,
      0xe1,
      0x00,
      0x22,
      0x45,
      0x78,
      0x69,
      0x66,
      0x00,
      0x00,
      0x4d,
      0x4d,
      0x00,
      0x2a,
      0x00,
      0x00,
      0x00,
      0x08,
      0x00,
      0x01,
      0x01,
      0x12,
      0x00,
      0x03,
      0x00,
      0x00,
      0x00,
      0x01,
      0x00,
      orientation,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00
    ))
    path <- file.path(directory, paste0("orientation-", orientation, ".jpeg"))
    writeBin(c(source_bytes[1:2], exif, source_bytes[-c(1:2)]), path)

    image <- builder_read_image(path)

    expect_null(image$error, info = as.character(orientation))
    expect_identical(
      image$source_dimensions,
      c(width = 4L, height = 6L),
      info = as.character(orientation)
    )
  }
})

test_that("JPEG metadata scanning reports a missing frame", {
  path <- withr::local_tempfile(fileext = ".jpeg")
  writeBin(
    c(as.raw(c(0xff, 0xd8)), raw(1024L)),
    path
  )

  dimensions <- builder_image_file_dimensions(path, "padded.jpeg")
  expect_true(is.list(dimensions))
  expect_match(dimensions$error, "metadata", ignore.case = TRUE)
  expect_match(
    builder_read_image(path, filename = "padded.jpeg")$error,
    "metadata",
    ignore.case = TRUE
  )
  parser <- paste(
    deparse(body(.builder_jpeg_dimensions_from_connection)),
    collapse = "\n"
  )
  expect_false(grepl("which(", parser, fixed = TRUE))
  expect_false(grepl("as.integer(bytes)", parser, fixed = TRUE))
})

test_that("incomplete PNG files are rejected", {
  malformed <- c(
    as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)),
    as.raw(c(0x00, 0x00, 0x00, 0x0d)),
    charToRaw("IHDR"),
    as.raw(c(0x00, 0x00, 0x00, 0x03)),
    as.raw(c(0x00, 0x00, 0x00, 0x03))
  )
  path <- withr::local_tempfile(fileext = ".png")
  writeBin(malformed, path)

  expect_identical(
    .builder_png_dimensions(malformed),
    c(width = 3L, height = 3L)
  )
  expect_identical(
    builder_read_image(path)$error,
    "The image file is incomplete or truncated."
  )
})

test_that("PNG uint32 dimensions remain numeric above the integer range", {
  path <- withr::local_tempfile(fileext = ".png")
  header <- c(
    as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)),
    as.raw(c(0x00, 0x00, 0x00, 0x0d)),
    charToRaw("IHDR"),
    as.raw(c(0x80, 0x00, 0x00, 0x00)),
    as.raw(c(0x00, 0x00, 0x00, 0x01))
  )
  writeBin(header, path)

  dimensions <- builder_image_file_dimensions(path, "oversized.png")
  expect_identical(unname(dimensions), c(2147483648, 1))
  expect_false(anyNA(dimensions))
})

test_that("image read failures never expose a server-side upload path", {
  skip_if_not_installed("png")
  path <- withr::local_tempfile(fileext = ".png")
  writeBin(charToRaw("not a png"), path)

  got <- builder_read_image(path, filename = "tissue.png")

  expect_identical(
    got$error,
    "Could not read this image. Check that it is a valid PNG or JPEG file."
  )
  expect_false(grepl(path, got$error, fixed = TRUE))
})

test_that("alignment records propagate transformed extent facts", {
  picture <- list(
    uri = "data:image/png;base64,AAAA",
    bytes = 4L,
    width = 2L,
    height = 2L,
    source_width = 4L,
    source_height = 2L,
    extent_width = 2L,
    extent_height = 4L,
    display_width = 2L,
    display_height = 2L
  )
  per_section <- list(
    section = list(
      bounds = list(xmin = 0, xmax = 2L, ymin = 0, ymax = 4L),
      cover = list(outside = 0L, total = 2L)
    )
  )
  paired <- builder_pair_sections(picture, per_section)$section

  for (field in c(
    "source_width",
    "source_height",
    "extent_width",
    "extent_height",
    "display_width",
    "display_height"
  )) {
    expect_identical(paired[[field]], picture[[field]], info = field)
  }

  app <- builder_app_source_text()
  alignment_server <- paste(
    readLines(
      builder_spatial_test_inst_path(
        "builder",
        "spatial_alignment_server.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  session <- paste(
    readLines(
      builder_spatial_test_inst_path("builder", "session.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(alignment_server, '"extent_width"', fixed = TRUE)
  expect_match(alignment_server, '"extent_height"', fixed = TRUE)
  expect_match(alignment_server, "builder_alignment_record", fixed = TRUE)
  expect_false(grepl("builder_encode_image", alignment_server, fixed = TRUE))
  expect_match(app, "nxt\\$extent_width")
  expect_match(app, "nxt\\$extent_height")
  expect_match(session, "extent_width")
  expect_match(session, "extent_height")
})

test_that("one slide applied to every section keeps each section's own extent", {
  picture <- list(
    source_path = "session-images/slide.png",
    project_asset = "assets/slide.png",
    bytes = 4L,
    width = 300L,
    height = 240L
  )
  per_section <- list(
    A = list(
      bounds = list(xmin = 0, xmax = 100, ymin = 0, ymax = 80),
      cover = list(outside = 0L, total = 100L)
    ),
    B = list(
      bounds = list(xmin = 500, xmax = 600, ymin = 0, ymax = 80),
      cover = list(outside = 0L, total = 100L)
    ),
    C = list(
      bounds = list(xmin = 2000, xmax = 2100, ymin = 0, ymax = 80),
      cover = list(outside = 7L, total = 100L)
    )
  )

  got <- builder_pair_sections(picture, per_section)
  expect_identical(
    unique(vapply(got, function(x) x$source_path, "")),
    picture$source_path
  )
  expect_identical(
    unique(vapply(got, function(x) x$project_asset, "")),
    picture$project_asset
  )
  expect_identical(
    vapply(got, function(x) x$bounds$xmin, numeric(1)),
    c(A = 0, B = 500, C = 2000)
  )
  expect_identical(
    vapply(got, function(x) x$outside, integer(1)),
    c(A = 0L, B = 0L, C = 7L)
  )
})
