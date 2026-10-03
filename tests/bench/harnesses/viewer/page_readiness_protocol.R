validate_page_profile <- function(profile, rounds) {
  if (is.na(rounds) || rounds < 1L) {
    stop("ROUNDS must be a positive integer.", call. = FALSE)
  }
  if (identical(profile, "evidence") && rounds < 5L) {
    stop("evidence requires at least 5 rounds", call. = FALSE)
  }
  invisible(TRUE)
}

page_budget_pass <- function(elapsed_ms, budget_ms) {
  is.finite(elapsed_ms) && elapsed_ms < budget_ms
}

requires_ready_event <- function(page_name, visit, warmed) {
  !identical(page_name, "coordinated_views") ||
    !identical(visit, "repeat") ||
    !isTRUE(warmed)
}

build_balanced_schedule <- function(candidates, page_names, rounds) {
  rows <- list()
  index <- 1L
  for (round in seq_len(rounds)) {
    shift <- (round - 1L) %% length(candidates)
    order <- candidates[c(
      seq.int(shift + 1L, length(candidates)),
      if (shift) seq_len(shift) else integer()
    )]
    for (page_name in page_names) {
      for (visit in c("first", "repeat")) {
        for (position in seq_along(order)) {
          rows[[index]] <- data.frame(
            schedule_position = index,
            round = round,
            candidate_position = position,
            candidate = order[[position]],
            page = page_name,
            visit = visit,
            stringsAsFactors = FALSE
          )
          index <- index + 1L
        }
      }
    }
  }
  do.call(rbind, rows)
}
