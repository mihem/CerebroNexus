## Fail when an R CMD check log holds anything CRAN would push back on.
##
## R CMD check prints "Status: 1 NOTE" and exits 0, so CI that only looks at the
## exit code or at ERROR/WARNING lets a new NOTE through. This reads
## 00check.log, fails on every ERROR and WARNING, and fails on every NOTE that is
## not on the allow-list below.
##
## Usage: Rscript check-log-notes.R <path to 00check.log>

## NOTEs accepted on purpose. Each entry: the check's name, and regexes of which
## every non-empty line of the NOTE's body must match one.
##
## Remove the "CRAN incoming feasibility" entry once 8.0.0 is on CRAN: the
## "New submission" line disappears then, and the target is zero NOTEs.
allowed_notes <- list(
  list(
    check = "CRAN incoming feasibility",
    body = c("^Maintainer: ", "^New submission$")
  )
)

args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L, file.exists(args[[1L]]))
log <- readLines(args[[1L]], warn = FALSE, encoding = "UTF-8")

## A check starts with "* checking <name> ... [timing] <STATUS>"; its body is every
## line up to the next line that starts with "* ".
starts <- which(grepl("^\\* ", log))
ends <- c(starts[-1L] - 1L, length(log))

is_allowed <- function(name, body) {
  for (a in allowed_notes) {
    if (identical(a$check, name)) {
      return(all(vapply(
        body,
        function(line) any(vapply(a$body, grepl, logical(1L), x = line)),
        logical(1L)
      )))
    }
  }
  FALSE
}

problems <- character()
for (k in seq_along(starts)) {
  first <- log[[starts[[k]]]]
  level <- sub("^.* \\.\\.\\. (\\[[^]]*\\] )?", "", first)
  if (!level %in% c("NOTE", "WARNING", "ERROR")) {
    next
  }
  body <- log[seq_len(ends[[k]] - starts[[k]]) + starts[[k]]]
  body <- trimws(body[nzchar(trimws(body))])
  name <- sub("^\\* checking (.*) \\.\\.\\. (\\[[^]]*\\] )?[A-Z]+$", "\\1", first)
  if (level != "NOTE" || !is_allowed(name, body)) {
    problems <- c(problems, first, paste0("    ", body), "")
  }
}

if (length(problems)) {
  cat("R CMD check reported findings that are not on the allow-list:\n\n")
  cat(problems, sep = "\n")
  cat(
    "Fix them, or for a NOTE accepted on purpose add it to allowed_notes in",
    ".github/scripts/check-log-notes.R.\n"
  )
  quit(status = 1L)
}
cat("check-log-notes: no ERROR, WARNING or unexpected NOTE in", args[[1L]], "\n")
