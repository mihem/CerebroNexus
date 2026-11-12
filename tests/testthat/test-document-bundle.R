## The kept page with each run of scripts and of styles as one file
## (viewerDocumentBundle() in inst/viewer/core/kept_document.R).

document_bundle_env <- function() {
  env <- new.env()
  sys.source(viewer_test_path("ui_cache.R"), env)
  sys.source(viewer_test_path("core", "kept_document.R"), env)
  env
}

## Resource directories as a page's dependencies are: scripts and styles by
## prefix, a style that names a font in the next directory.
document_bundle_files <- function() {
  ## not in the session's temporary directory: such resource paths are not kept
  root <- withr::local_tempdir(
    tmpdir = dirname(tempdir()),
    .local_envir = parent.frame()
  )
  write <- function(path, text) {
    path <- file.path(root, path)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(text, path)
  }
  write("liba/a.js", "window.order = ['a'];")
  write("liba/b.js", "window.order.push('b')")
  write("libb/c.js", "window.order.push('c');")
  write("libb/d.js", "window.order.push('d');")
  write("libb/e.js", "window.order.push('e');")
  write(
    "fonts/css/f.css",
    "@font-face { src: url(../webfonts/f.woff2?v=1#x); }"
  )
  write(
    "fonts/css/g.css",
    "a { background: url('data:image/png;base64,AAAA'); }"
  )
  write("fonts/css/import.css", "@import url(other.css);")
  write("fonts/css/h.css", "b { background: url(\"../img/h.svg\"); }")
  ## a stylesheet with a character that is two bytes
  writeBin(
    c(
      charToRaw("/* "),
      as.raw(c(0xc3, 0xa9)),
      charToRaw(
        " */ c { background: url(x.png); }
"
      )
    ),
    file.path(root, "fonts/css/u.css")
  )
  paths <- c(
    liba = file.path(root, "liba"),
    libb = file.path(root, "libb"),
    fonts = file.path(root, "fonts")
  )
  list(root = root, paths = paths)
}

document_bundle_html <- function(body) {
  c(
    charToRaw(paste0(
      "<!DOCTYPE html>\n<html><head>\n",
      body,
      "\n</head><body>page "
    )),
    as.raw(c(0xc3, 0xa9)),
    charToRaw("</body></html>")
  )
}

test_that("scripts that follow one another become one file, in their order", {
  env <- document_bundle_env()
  files <- document_bundle_files()
  html <- document_bundle_html(paste(
    "<script src=\"liba/a.js\"></script>",
    "<script src=\"liba/b.js\"></script>",
    "<script src=\"libb/c.js\"></script>",
    sep = "\n"
  ))
  bundle <- env$viewerDocumentBundle(html, files$paths)
  expect_length(bundle$files, 1L)
  name <- names(bundle$files)
  expect_match(name, "^bundle-[0-9a-f]{16}[.]js$")
  expect_identical(
    rawToChar(bundle$files[[1L]]),
    paste0(
      "window.order = ['a'];\n\n;\n",
      "window.order.push('b')\n\n;\n",
      "window.order.push('c');\n\n;\n"
    )
  )
  page <- rawToChar(bundle$html)
  expect_identical(lengths(regmatches(page, gregexpr("<script", page))), 1L)
  expect_match(
    page,
    sprintf("<script src=\"cerebro-bundle/%s\"></script>", name),
    fixed = TRUE
  )
  ## the rest of the page, with its bytes, is as it was
  expect_identical(utils::tail(bundle$html, 20L), utils::tail(html, 20L))
})

test_that("an inline script, another site and a plain script apart end a run", {
  env <- document_bundle_env()
  files <- document_bundle_files()
  html <- document_bundle_html(paste(
    "<script src=\"liba/a.js\"></script>",
    "<script src=\"liba/b.js\"></script>",
    "<script>window.inline = true;</script>",
    "<script src=\"libb/c.js\"></script>",
    "<script src=\"https://example.org/x.js\"></script>",
    "<script src=\"libb/d.js\"></script>",
    "<script src=\"libb/e.js\"></script>",
    "<script type=\"application/json\">{}</script>",
    "<script src=\"nothing/z.js\"></script>",
    sep = "\n"
  ))
  bundle <- env$viewerDocumentBundle(html, files$paths)
  ## a and b, and d and e: two runs; c is alone, and so are the others
  expect_length(bundle$files, 2L)
  page <- rawToChar(bundle$html)
  tags <- regmatches(page, gregexpr("<script[^>]*>", page))[[1L]]
  expect_length(tags, 7L)
  expect_match(tags[[1L]], "cerebro-bundle")
  expect_identical(tags[[2L]], "<script>")
  expect_identical(tags[[3L]], "<script src=\"libb/c.js\">")
  expect_identical(tags[[4L]], "<script src=\"https://example.org/x.js\">")
  expect_match(tags[[5L]], "cerebro-bundle")
  expect_identical(tags[[6L]], "<script type=\"application/json\">")
  expect_identical(tags[[7L]], "<script src=\"nothing/z.js\">")
  ## and nothing is left to join
  expect_null(env$viewerDocumentBundle(
    document_bundle_html("<script src=\"liba/a.js\"></script>"),
    files$paths
  ))
})

test_that("scripts with defer are joined across what lies between them", {
  env <- document_bundle_env()
  files <- document_bundle_files()
  html <- document_bundle_html(paste(
    "<script defer src=\"liba/a.js\"></script>",
    "<script>window.inline = true;</script>",
    "<script src=\"libb/c.js\"></script>",
    "<script defer src=\"liba/b.js\"></script>",
    "<script defer src=\"https://example.org/y.js\"></script>",
    "<script async src=\"libb/d.js\"></script>",
    "<script defer src=\"libb/d.js\"></script>",
    "<script defer src=\"libb/e.js\"></script>",
    sep = "\n"
  ))
  bundle <- env$viewerDocumentBundle(html, files$paths)
  ## a and b, and d and e: another deferred script that stays ends the run
  expect_length(bundle$files, 2L)
  page <- rawToChar(bundle$html)
  expect_identical(
    lengths(regmatches(
      page,
      gregexpr("<script defer src=\"cerebro-bundle/", page)
    )),
    2L
  )
  expect_match(
    page,
    "<script defer src=\"https://example.org/y.js\">",
    fixed = TRUE
  )
  ## an async script and a script without defer do not end it
  expect_match(page, "<script async src=\"libb/d.js\">", fixed = TRUE)
  expect_match(page, "<script src=\"libb/c.js\">", fixed = TRUE)
})

test_that("styles are joined with their urls moved, and an @import stays", {
  env <- document_bundle_env()
  files <- document_bundle_files()
  html <- document_bundle_html(paste(
    "<link href=\"fonts/css/f.css\" rel=\"stylesheet\" />",
    "<link href=\"fonts/css/g.css\" rel=\"stylesheet\" />",
    "<link href=\"fonts/css/h.css\" rel=\"stylesheet\" />",
    "<link href=\"fonts/css/u.css\" rel=\"stylesheet\" />",
    "<style>body { margin: 0 }</style>",
    "<link href=\"fonts/css/f.css\" rel=\"stylesheet\" />",
    "<link href=\"fonts/css/import.css\" rel=\"stylesheet\" />",
    "<link href=\"fonts/css/g.css\" media=\"print\" rel=\"stylesheet\" />",
    "<link href=\"fonts/css/h.css\" rel=\"stylesheet\" />",
    "<link href=\"liba/a.js\" rel=\"preload\" />",
    sep = "\n"
  ))
  bundle <- env$viewerDocumentBundle(html, files$paths)
  expect_length(bundle$files, 1L)
  css <- rawToChar(bundle$files[[1L]])
  ## the three files in the order of the page, seen from "cerebro-bundle/"
  expect_match(css, "url(\"../fonts/webfonts/f.woff2?v=1#x\")", fixed = TRUE)
  expect_match(css, "data:image/png;base64,AAAA", fixed = TRUE)
  expect_match(css, "url(\"../fonts/img/h.svg\")", fixed = TRUE)
  expect_match(css, "url(\"../fonts/css/x.png\")", fixed = TRUE)
  ## bytes are bytes: the two of the character are there, once
  e_acute <- rawToChar(as.raw(c(0xc3, 0xa9)))
  expect_identical(
    lengths(regmatches(
      css,
      gregexpr(e_acute, css, fixed = TRUE, useBytes = TRUE)
    )),
    1L
  )
  expect_lt(regexpr("f.woff2", css), regexpr("data:image", css))
  expect_lt(regexpr("data:image", css), regexpr("h.svg", css))
  page <- rawToChar(bundle$html)
  links <- regmatches(page, gregexpr("<link[^>]*>", page))[[1L]]
  expect_length(links, 6L)
  expect_match(links[[1L]], "cerebro-bundle/bundle-.*[.]css")
  expect_identical(
    links[[2L]],
    "<link href=\"fonts/css/f.css\" rel=\"stylesheet\" />"
  )
  expect_identical(
    links[[3L]],
    "<link href=\"fonts/css/import.css\" rel=\"stylesheet\" />"
  )
  expect_match(page, "<style>body { margin: 0 }</style>", fixed = TRUE)
})

test_that("a kept document is restored in its bundled form unless that is off", {
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  env <- document_bundle_env()
  files <- document_bundle_files()
  for (prefix in names(files$paths)) {
    shiny::addResourcePath(prefix, files$paths[[prefix]])
  }
  withr::defer(
    for (prefix in names(files$paths)) {
      try(shiny::removeResourcePath(prefix), silent = TRUE)
    }
  )
  html <- c(
    document_bundle_html(paste(
      "<script src=\"liba/a.js\"></script>",
      "<script src=\"liba/b.js\"></script>",
      sep = "\n"
    ))
  )
  ## what a Shiny page says of itself
  html <- c(html, charToRaw(" shiny"))
  file <- env$viewerDocumentKeep("bundle-key", html)
  kept <- readRDS(file)
  expect_identical(kept$html, html)
  expect_length(kept$bundle$files, 1L)

  target <- withr::local_tempdir()
  withr::defer(unlink(env$viewerDocumentBundleDirectory(), recursive = TRUE))
  withr::local_options(CerebroNexus.bundle = TRUE)
  expect_identical(
    env$viewerDocumentRestore("bundle-key", target = target),
    target
  )
  restored <- rawToChar(readBin(file.path(target, "index.html"), "raw", 1e6))
  expect_match(restored, "cerebro-bundle/bundle-", fixed = TRUE)
  written <- file.path(
    env$viewerDocumentBundleDirectory(),
    names(kept$bundle$files)
  )
  expect_true(file.exists(written))
  expect_identical(readBin(written, "raw", 1e6), kept$bundle$files[[1L]])

  ## the page as it was kept, with the option off
  withr::local_options(CerebroNexus.bundle = FALSE)
  env$viewerDocumentRestore("bundle-key", target = target)
  expect_identical(readBin(file.path(target, "index.html"), "raw", 1e6), html)

  ## a bundle that is not what it should be is not used
  damaged <- kept
  damaged$bundle$files <- list(`../escape.js` = as.raw(1:3))
  withr::local_options(CerebroNexus.bundle = TRUE)
  expect_identical(env$viewerDocumentPage(damaged)$html, html)
  expect_length(env$viewerDocumentPage(damaged)$files, 0L)
})

test_that("the bundle is served with a header that keeps it", {
  env <- document_bundle_env()
  directory <- withr::local_tempdir()
  env$viewerDocumentWriteFiles(
    list(`bundle-0123456789abcdef.js` = charToRaw("1;")),
    directory
  )
  path <- env$viewerDocumentBundleStaticPath(directory)
  expect_match(path$options$headers[["Cache-Control"]], "immutable")
  expect_false(isTRUE(path$options$indexhtml))
  expect_true(path$options$fallthrough)
  expect_null(env$viewerDocumentWriteFiles(list(), directory))
})
