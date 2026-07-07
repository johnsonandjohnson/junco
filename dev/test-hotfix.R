suppressMessages(library(junco))
suppressMessages(library(rtables))
suppressMessages(library(rlistings))
suppressMessages(library(codetools))

hotfix_file <- file.path(testthat::test_path(), "..", "..", "code_library", "junco_hotfix.r")

load_hotfix_env <- function(file) {
  e <- new.env(parent = globalenv())
  # Suppress the rver stop() and library() side-effects by temporarily
  # overriding getRversion and library inside the sourced env.
  e$getRversion <- function() package_version("4.5.2")
  e$library <- function(...) invisible(NULL)
  suppressMessages(sys.source(file, envir = e))
  e
}

test_that("hotfix file can be sourced without errors", {
  expect_no_error(load_hotfix_env(hotfix_file))
})

test_that("hotfix defines expected functions", {
  e <- load_hotfix_env(hotfix_file)
  expected_funs <- c(
    "tt_to_tlgrtf",
    "tt_to_flextable_j",
    "export_TLG_as_docx"
  )
  for (fn in expected_funs) {
    expect_true(
      exists(fn, envir = e, mode = "function", inherits = FALSE),
      label = paste0("hotfix defines `", fn, "`")
    )
  }
})

test_that("all functions called by hotfix are resolvable (no missing symbols)", {
  e <- load_hotfix_env(hotfix_file)
  funs <- Filter(is.function, mget(ls(e), envir = e))

  calls <- unique(unlist(lapply(funs, function(f) {
    codetools::findGlobals(f, merge = FALSE)$functions
  })))

  # Build a whitelist of everything reachable: junco namespace + all packages
  # junco imports from (tern, rtables, etc.) + hotfix-defined symbols.
  junco_ns <- asNamespace("junco")
  junco_all <- ls(junco_ns)
  junco_imports <- unlist(lapply(
    names(getNamespaceImports("junco")),
    function(pkg) tryCatch(ls(asNamespace(pkg)), error = function(e) character(0))
  ))
  hotfix_defined <- ls(e)

  missing <- sort(
    calls[
      !sapply(calls, function(x)
        exists(x, envir = e, mode = "function", inherits = TRUE) ||
          x %in% junco_all ||
          x %in% junco_imports ||
          x %in% hotfix_defined
      )
    ]
  )

  expect_equal(
    missing,
    character(0),
    label = paste(
      "Unresolvable function calls in hotfix (likely missing `pkg:::` prefix):",
      paste(missing, collapse = ", ")
    )
  )
})

test_that("junco internal calls use `junco:::` prefix (not bare names)", {
  src <- paste(readLines(hotfix_file), collapse = "\n")

  junco_ns <- asNamespace("junco")
  junco_internals <- setdiff(ls(junco_ns), getNamespaceExports("junco"))

  # Exclude symbols that are redefined in the hotfix itself — those are
  # intentional local overrides, not bare calls to the locked namespace.
  e <- load_hotfix_env(hotfix_file)
  hotfix_defined <- ls(e, all.names = FALSE)
  # Also exclude the helper stubs injected by load_hotfix_env
  hotfix_defined <- setdiff(hotfix_defined, c("getRversion", "library"))

  junco_internals_to_check <- setdiff(junco_internals, hotfix_defined)

  bare_calls <- Filter(function(fn) {
    grepl(paste0("(?<!:)\\b", fn, "\\s*\\("), src, perl = TRUE) &&
      !grepl(paste0("junco:{2,3}", fn), src, perl = TRUE)
  }, junco_internals_to_check)

  expect_equal(
    sort(bare_calls),
    character(0),
    label = paste(
      "Bare calls to junco internals found (should use `junco:::` prefix):",
      paste(sort(bare_calls), collapse = ", ")
    )
  )
})
