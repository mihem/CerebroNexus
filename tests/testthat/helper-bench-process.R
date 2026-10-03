bench_process_env <- function(env) {
  if (!length(env)) {
    return(character())
  }
  if (!is.null(names(env)) && all(nzchar(names(env)))) {
    return(env)
  }
  separator <- regexpr("=", env, fixed = TRUE)
  if (any(separator < 2L)) {
    stop("benchmark process variables must use NAME=VALUE", call. = FALSE)
  }
  stats::setNames(
    substring(env, separator + 1L),
    substr(env, 1L, separator - 1L)
  )
}

bench_system2 <- function(command, args = character(), ..., env = character()) {
  variables <- bench_process_env(env)
  if (!length(variables)) {
    return(system2(command, args, ...))
  }
  withr::with_envvar(variables, system2(command, args, ...))
}
