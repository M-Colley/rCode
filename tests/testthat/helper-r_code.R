library(testthat)
options(rcode.auto_install = FALSE)
rcode_file <- normalizePath(file.path("..", "..", "r_functionality.R"), mustWork = TRUE)
suppressWarnings(source(rcode_file, local = FALSE))
