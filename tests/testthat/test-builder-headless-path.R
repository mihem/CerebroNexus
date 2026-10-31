test_that("a server only opens project sources inside its allowed folders", {
  local({
    source(
      builder_profile_inst_path("builder", "core", "bundle_path_contract.R"),
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

    entry <- function(id, name, path) {
      list(
        id = id,
        filename = basename(path),
        path = path,
        source_origin = "local",
        settings = list(name = name)
      )
    }
    manifest <- builder_project_snapshot(
      "p",
      list(
        entry("ds1", "Allowed dataset", inside_source),
        entry("ds2", "Outside dataset", outside_source)
      )
    )
    path_allowed <- function(path) {
      !inherits(
        tryCatch(
          builder_server_path_resolve(path, "file", roots = allowed),
          error = identity
        ),
        "condition"
      )
    }
    plan <- builder_project_restore_plan(manifest, path_allowed = path_allowed)
    expect_identical(plan$ds1$status, "ready")
    expect_identical(plan$ds2$status, "missing")
    expect_match(plan$ds2$reason, "outside the folders")
    expect_identical(
      builder_project_restore_plan(manifest)$ds2$status,
      "ready"
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
