library(testthat)
library(CerebroNexus)

## R CMD check --as-cran sets _R_CHECK_LIMIT_CORES_; CRAN allows two cores.
## Keep testthat at two test processes and each process single-threaded
## (OpenMP, RcppParallel). The test processes inherit these settings.
local({
  limit <- Sys.getenv("_R_CHECK_LIMIT_CORES_")
  if (nzchar(limit) && !identical(tolower(limit), "false")) {
    Sys.setenv(OMP_THREAD_LIMIT = "1", RCPP_PARALLEL_NUM_THREADS = "1")
    if (!nzchar(Sys.getenv("TESTTHAT_CPUS"))) {
      options(Ncpus = min(2L, as.integer(getOption("Ncpus", 2L))))
    }
  }
})

test_check("CerebroNexus")
