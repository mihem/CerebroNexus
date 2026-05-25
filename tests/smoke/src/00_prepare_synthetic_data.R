#!/usr/bin/env Rscript

rm(list = ls())

source("src/smoke_fixture_utils.R")
ensure_smoke_fixtures(
  data_dir = "data",
  force = identical(Sys.getenv("SMOKE_SYNTH_FORCE", unset = "0"), "1")
)
