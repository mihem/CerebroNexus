test_that("headless Project open validates every external dataset source", {
  local({
    source(
      builder_profile_inst_path(
        "builder",
        "core",
        "bundle_path_contract.R"
      ),
      local = TRUE
    )
    source(builder_profile_inst_path("builder", "io.R"), local = TRUE)
    source(builder_profile_inst_path("builder", "project.R"), local = TRUE)

    root <- withr::local_tempdir()
    allowed <- file.path(root, "allowed")
    outside <- file.path(root, "outside")
    dir.create(allowed)
    dir.create(outside)
    inside_source <- file.path(allowed, "inside.rds")
    outside_source <- file.path(outside, "outside.rds")
    writeBin(charToRaw("inside"), inside_source)
    writeBin(charToRaw("outside"), outside_source)

    manifest <- list(
      datasets = list(list(
        id = "ds1",
        name = "Allowed dataset",
        source = list(kind = "external", path = inside_source)
      ))
    )
    checked <- builder_project_validate_external_sources(
      manifest,
      roots = allowed
    )
    expect_identical(
      checked$datasets[[1L]]$source$path,
      normalizePath(inside_source, winslash = "/", mustWork = TRUE)
    )

    manifest$datasets[[1L]]$name <- "Outside dataset"
    manifest$datasets[[1L]]$source$path <- outside_source
    expect_error(
      builder_project_validate_external_sources(manifest, roots = allowed),
      "Outside dataset",
      fixed = TRUE
    )
  })
})

test_that("server path fallback resolves symlinks inside explicit roots", {
  local({
    source(
      builder_profile_inst_path(
        "builder",
        "core",
        "bundle_path_contract.R"
      ),
      local = TRUE
    )
    source(
      builder_profile_inst_path("builder", "io.R"),
      local = TRUE
    )
    root <- withr::local_tempdir()
    allowed <- file.path(root, "allowed")
    outside <- file.path(root, "outside")
    inside <- file.path(allowed, "project")
    dir.create(inside, recursive = TRUE)
    dir.create(outside)

    expect_identical(
      builder_server_path_resolve(inside, "directory", roots = allowed),
      normalizePath(inside, winslash = "/", mustWork = TRUE)
    )
    expect_error(
      builder_server_path_resolve(outside, "directory", roots = allowed),
      "outside the allowed server folders",
      fixed = TRUE
    )

    linked <- file.path(allowed, "linked-outside")
    if (isTRUE(file.symlink(outside, linked))) {
      expect_error(
        builder_server_path_resolve(linked, "directory", roots = allowed),
        "outside the allowed server folders",
        fixed = TRUE
      )
    }
  })
})
