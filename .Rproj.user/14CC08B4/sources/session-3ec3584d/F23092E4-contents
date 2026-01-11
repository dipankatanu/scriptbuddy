#' Scan dependencies from an R script or project folder
#'
#' ScriptBuddy finds packages used via `library()`, `require()`, `pkg::fun()`, and follows `source()` files recursively.
#'
#' @param path Path to a `.R` file or a directory containing `.R` files.
#' @return Character vector of detected package names.
#' @export
scan_deps <- function(path) {
  path <- normalizePath(path, winslash = '/', mustWork = FALSE)
  files <- .sb_collect_files_recursive(path)
  if (length(files) == 0) return(character())
  pkgs <- character()
  for (f in files) pkgs <- c(pkgs, .sb_parse_file_pkgs(f))
  base_pkgs <- c('stats','graphics','grDevices','utils','methods','base','datasets')
  pkgs <- setdiff(.sb_unique_clean(pkgs), base_pkgs)
  pkgs
}

#' Classify packages as CRAN, Bioconductor, or Unknown
#' Classify packages as CRAN, Bioconductor, or Unknown
#'
#' @param pkgs Character vector of package names.
#' @param cran_repo CRAN repository URL.
#' @return A list with elements `cran`, `bioc`, and `unknown`.
#' @export
classify_deps <- function(pkgs, cran_repo = "https://cloud.r-project.org") {
  pkgs <- .sb_unique_clean(pkgs)
  if (length(pkgs) == 0) return(list(cran = character(), bioc = character(), unknown = character()))

  ap <- tryCatch(utils::available.packages(repos = cran_repo), error = function(e) NULL)
  cran_avail <- if (!is.null(ap)) rownames(ap) else character()
  cran <- intersect(pkgs, cran_avail)
  remaining <- setdiff(pkgs, cran)

  bioc <- character()
  if (requireNamespace("BiocManager", quietly = TRUE) && length(remaining) > 0) {
    # Temporarily set Bioconductor repos so BiocManager doesn't warn
    old_repos <- getOption("repos")
    on.exit(options(repos = old_repos), add = TRUE)

    bioc_repos <- tryCatch(BiocManager::repositories(), error = function(e) NULL)
    if (!is.null(bioc_repos)) {
      # Keep CRAN at user-provided repo, but add Bioc repos
      options(repos = c(CRAN = cran_repo, bioc_repos))
    } else {
      options(repos = c(CRAN = cran_repo))
    }

    bi <- suppressMessages(tryCatch(BiocManager::available(), error = function(e) character()))
    bioc <- intersect(remaining, bi)
  }

  unknown <- setdiff(remaining, bioc)
  list(cran = .sb_unique_clean(cran), bioc = .sb_unique_clean(bioc), unknown = .sb_unique_clean(unknown))
}

#' Install dependencies detected from an R script or project
#'
#' Install dependencies detected from an R script/project
#'
#' Installs only missing packages from CRAN and Bioconductor.
#'
#' @export
install_deps <- function(path, cran_repo = "https://cloud.r-project.org", quiet = FALSE) {
  pkgs <- scan_deps(path)
  cls <- classify_deps(pkgs, cran_repo = cran_repo)

  installed <- rownames(installed.packages())

  cran_missing <- setdiff(cls$cran, installed)
  bioc_missing <- setdiff(cls$bioc, installed)

  cran_present <- intersect(cls$cran, installed)
  bioc_present <- intersect(cls$bioc, installed)

  # Pretty reporting
  if (requireNamespace("cli", quietly = TRUE)) {
    cli::cli_h2("Dependency status")

    if (length(cran_present) > 0)
      cli::cli_alert_success(paste("Already installed (CRAN):", paste(cran_present, collapse = ", ")))
    if (length(bioc_present) > 0)
      cli::cli_alert_success(paste("Already installed (Bioconductor):", paste(bioc_present, collapse = ", ")))

    if (length(cran_missing) > 0)
      cli::cli_alert_info(paste("Will install (CRAN):", paste(cran_missing, collapse = ", ")))
    if (length(bioc_missing) > 0)
      cli::cli_alert_info(paste("Will install (Bioconductor):", paste(bioc_missing, collapse = ", ")))
  }

  # Set repos safely
  old_repos <- getOption("repos")
  on.exit(options(repos = old_repos), add = TRUE)

  if (requireNamespace("BiocManager", quietly = TRUE)) {
    bioc_repos <- tryCatch(BiocManager::repositories(), error = function(e) NULL)
    if (!is.null(bioc_repos)) {
      options(repos = c(CRAN = cran_repo, bioc_repos))
    } else {
      options(repos = c(CRAN = cran_repo))
    }
  } else {
    options(repos = c(CRAN = cran_repo))
  }

  # Install only what is missing
  if (length(cran_missing) > 0) {
    utils::install.packages(cran_missing, dependencies = TRUE, quiet = quiet)
  }

  if (length(bioc_missing) > 0) {
    if (!requireNamespace("BiocManager", quietly = TRUE)) {
      utils::install.packages("BiocManager", quiet = quiet)
    }
    suppressMessages(
      BiocManager::install(bioc_missing, ask = FALSE, update = FALSE, quiet = quiet)
    )
  }

  invisible(list(
    already_installed = c(cran_present, bioc_present),
    installed_now = c(cran_missing, bioc_missing),
    unknown = cls$unknown
  ))
}

#' ScriptBuddy: scan + report + optional install (one command)
#'
#' @param path Path to a `.R` file or a directory.
#' @param install If TRUE, installs detected packages.
#' @param verbose If TRUE, prints detailed output.
#' @param max_items Max number of package names to show per line.
#' @param cran_repo CRAN repository URL.
#' @return Invisibly returns a list with classification and GitHub remotes.
#' @export
scriptbuddy <- function(path, install = FALSE, verbose = TRUE, max_items = 12,
                        cran_repo = "https://cloud.r-project.org") {

  t0 <- .sb_now()
  path_norm <- normalizePath(path, winslash = "/", mustWork = FALSE)

  pkgs <- scan_deps(path_norm)
  cls  <- classify_deps(pkgs, cran_repo = cran_repo)
  gh   <- scan_remotes(path_norm)

  installed <- rownames(installed.packages())
  cran_missing <- setdiff(cls$cran, installed)
  bioc_missing <- setdiff(cls$bioc, installed)
  cran_present <- intersect(cls$cran, installed)
  bioc_present <- intersect(cls$bioc, installed)

  if (.sb_cli() && verbose) {
    cli::cli_h1("ScriptBuddy")
    cli::cli_alert_info(paste0("Scan target: {.path ", path_norm, "}"))

    .sb_rule("Detected")
    cli::cli_alert_success(paste0("CRAN (", length(cls$cran), "): ", .sb_fmt_list(cls$cran, max_items)))
    cli::cli_alert_success(paste0("Bioconductor (", length(cls$bioc), "): ", .sb_fmt_list(cls$bioc, max_items)))
    if (length(gh)) cli::cli_alert_warning(paste0("GitHub remotes (", length(gh), "): ", .sb_fmt_list(gh, max_items)))
    if (length(cls$unknown)) cli::cli_alert_warning(paste0("Unknown (", length(cls$unknown), "): ", .sb_fmt_list(cls$unknown, max_items)))

    .sb_rule("Install plan")
    if (length(cran_present)) cli::cli_alert_success(paste0("Already installed (CRAN): ", .sb_fmt_list(cran_present, max_items)))
    if (length(bioc_present)) cli::cli_alert_success(paste0("Already installed (Bioc): ", .sb_fmt_list(bioc_present, max_items)))
    if (length(cran_missing)) cli::cli_alert_info(paste0("Will install (CRAN): ", .sb_fmt_list(cran_missing, max_items)))
    if (length(bioc_missing)) cli::cli_alert_info(paste0("Will install (Bioc): ", .sb_fmt_list(bioc_missing, max_items)))

    if (!install) {
      .sb_callout(
        "Install missing packages",
        "Run {.fn scriptbuddy}(path, install = TRUE) to install what is missing."
      )

      if (length(gh)) {
        .sb_callout(
          "GitHub packages detected",
          "Run {.fn print_install_commands}(path) to copy/paste GitHub install commands."
        )
      }
    }

  }

  if (isTRUE(install)) {
    install_deps(path_norm, cran_repo = cran_repo, quiet = FALSE)
  }

  # ---- Summary card ----
  if (.sb_cli()) {
    .sb_rule("Summary")
    cli::cli_text(paste0(
      "CRAN: ", .sb_plural(length(cls$cran), "pkg"), " | ",
      "Bioconductor: ", .sb_plural(length(cls$bioc), "pkg"), " | ",
      "GitHub: ", .sb_plural(length(gh), "repo"), " | ",
      "Missing: ", .sb_plural(length(cran_missing) + length(bioc_missing), "pkg")
    ))
    cli::cli_alert_success(paste0("Time: ", .sb_elapsed(t0), " sec"))
  }

  invisible(list(classified = cls, github = gh))
}

# ----------------------------
# Internal helpers (not exported)
# ----------------------------
.sb_now <- function() Sys.time()

.sb_elapsed <- function(t0) {
  round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
}

.sb_callout <- function(title, text) {
  if (.sb_cli()) {
    cli::cli_rule(cli::col_yellow(paste("TIP:", title)))
    cli::cli_alert_warning(text)
  }
}



.sb_plural <- function(n, word) {
  if (n == 1) paste0(n, " ", word) else paste0(n, " ", word, "s")
}


.sb_unique_clean <- function(x) sort(unique(x[nzchar(x) & !is.na(x)]))
.sb_is_string1 <- function(x) is.character(x) && length(x) == 1 && !is.na(x)

.sb_read_text <- function(file) {
  tryCatch(readLines(file, warn = FALSE, encoding = 'UTF-8'),
           error = function(e) readLines(file, warn = FALSE))
}

.sb_extract_pkg_from_call <- function(expr) {
  out <- character()
  if (!is.call(expr)) return(out)
  fn <- expr[[1]]

  # library(pkg), require(pkg), requireNamespace('pkg'), loadNamespace('pkg')
  if (is.name(fn) && as.character(fn) %in% c('library','require','requireNamespace','loadNamespace')) {
    if (length(expr) >= 2) {
      arg <- expr[[2]]
      if (is.name(arg)) out <- c(out, as.character(arg))
      if (.sb_is_string1(arg)) out <- c(out, arg)
    }
    return(.sb_unique_clean(out))
  }

  # pacman::p_load(pkg1, pkg2, ...)
  if (is.call(fn) && identical(fn[[1]], as.name('::')) && length(fn) >= 3) {
    pkg <- fn[[2]]; fun <- fn[[3]]
    if (is.name(pkg) && is.name(fun) && as.character(pkg) == 'pacman' && as.character(fun) == 'p_load') {
      args <- as.list(expr)[-1]
      for (a in args) {
        if (is.name(a)) out <- c(out, as.character(a))
        if (.sb_is_string1(a)) out <- c(out, a)
      }
      return(.sb_unique_clean(out))
    }
  }

  # pkg::fun(...) or pkg:::fun(...)
  if (identical(fn, as.name('::')) && length(expr) >= 3) {
    pkg <- expr[[2]]
    if (is.name(pkg)) out <- c(out, as.character(pkg))
    return(.sb_unique_clean(out))
  }
  if (identical(fn, as.name(':::')) && length(expr) >= 3) {
    pkg <- expr[[2]]
    if (is.name(pkg)) out <- c(out, as.character(pkg))
    return(.sb_unique_clean(out))
  }

  .sb_unique_clean(out)
}
.sb_cli <- function() requireNamespace("cli", quietly = TRUE)

.sb_fmt_list <- function(x, max_items = 12) {
  x <- .sb_unique_clean(x)
  if (!length(x)) return("none")
  if (length(x) <= max_items) return(paste(x, collapse = ", "))
  paste0(paste(x[1:max_items], collapse = ", "), ", … (+", length(x) - max_items, " more)")
}


.sb_rule <- function(label = NULL) {
  if (.sb_cli()) {
    if (is.null(label)) cli::cli_rule()
    else cli::cli_rule(left = label)
  }
}


.sb_walk_expr <- function(expr) {
  pkgs <- character()
  walk <- function(x) {
    if (is.call(x)) {
      pkgs <<- c(pkgs, .sb_extract_pkg_from_call(x))
      for (i in seq_along(x)) walk(x[[i]])
    } else if (is.pairlist(x) || is.list(x)) {
      for (i in seq_along(x)) walk(x[[i]])
    }
  }
  walk(expr)
  .sb_unique_clean(pkgs)
}

.sb_parse_file_pkgs <- function(file) {
  file <- normalizePath(file, winslash = '/', mustWork = FALSE)
  txt <- .sb_read_text(file)
  if (!length(txt)) return(character())
  exprs <- tryCatch(parse(text = txt, keep.source = FALSE), error = function(e) NULL)
  if (is.null(exprs)) return(character())
  pkgs <- character()
  for (ex in as.list(exprs)) pkgs <- c(pkgs, .sb_walk_expr(ex))
  .sb_unique_clean(pkgs)
}

.sb_find_source_files <- function(file) {
  file <- normalizePath(file, winslash = '/', mustWork = FALSE)
  txt <- .sb_read_text(file)
  if (!length(txt)) return(character())
  exprs <- tryCatch(parse(text = txt, keep.source = FALSE), error = function(e) NULL)
  if (is.null(exprs)) return(character())

  sourced <- character()
  basedir <- dirname(file)

  find_source <- function(expr) {
    if (!is.call(expr)) return()
    if (is.name(expr[[1]]) && as.character(expr[[1]]) == 'source' && length(expr) >= 2) {
      arg <- expr[[2]]
      if (.sb_is_string1(arg)) {
        f <- normalizePath(file.path(basedir, arg), winslash = '/', mustWork = FALSE)
        if (file.exists(f)) sourced <<- c(sourced, f)
      }
    }
    for (i in seq_along(expr)) if (is.language(expr[[i]])) find_source(expr[[i]])
  }

  for (ex in as.list(exprs)) find_source(ex)
  .sb_unique_clean(sourced)
}

.sb_collect_files_recursive <- function(path) {
  path <- normalizePath(path, winslash = '/', mustWork = FALSE)
  files <- if (dir.exists(path)) {
    list.files(path, pattern = '\\.[Rr]$', recursive = TRUE, full.names = TRUE)
  } else {
    path
  }

  seen <- character()
  queue <- .sb_unique_clean(files[file.exists(files)])

  while (length(queue)) {
    f <- queue[[1]]; queue <- queue[-1]
    if (f %in% seen) next
    seen <- c(seen, f)
    queue <- .sb_unique_clean(c(queue, .sb_find_source_files(f)))
  }

  seen[file.exists(seen)]
}
.sb_extract_github_remotes_from_call <- function(expr) {
  # Detect remotes::install_github("owner/repo") or devtools::install_github("owner/repo")
  out <- character()
  if (!is.call(expr)) return(out)

  fn <- expr[[1]]

  # Case: remotes::install_github("x") or devtools::install_github("x")
  if (is.call(fn) && identical(fn[[1]], as.name("::")) && length(fn) >= 3) {
    pkg <- fn[[2]]
    fun <- fn[[3]]
    if (is.name(pkg) && is.name(fun) && as.character(fun) == "install_github") {
      if (as.character(pkg) %in% c("remotes", "devtools")) {
        if (length(expr) >= 2) {
          arg <- expr[[2]]
          if (.sb_is_string1(arg)) out <- c(out, arg)
        }
      }
    }
  }

  .sb_unique_clean(out)
}

.sb_walk_expr_remotes <- function(expr) {
  rem <- character()
  walk <- function(x) {
    if (is.call(x)) {
      rem <<- c(rem, .sb_extract_github_remotes_from_call(x))
      for (i in seq_along(x)) walk(x[[i]])
    } else if (is.pairlist(x) || is.list(x)) {
      for (i in seq_along(x)) walk(x[[i]])
    }
  }
  walk(expr)
  .sb_unique_clean(rem)
}

.sb_parse_file_remotes <- function(file) {
  file <- normalizePath(file, winslash = "/", mustWork = FALSE)
  txt <- .sb_read_text(file)
  if (!length(txt)) return(character())
  exprs <- tryCatch(parse(text = txt, keep.source = FALSE), error = function(e) NULL)
  if (is.null(exprs)) return(character())
  rem <- character()
  for (ex in as.list(exprs)) rem <- c(rem, .sb_walk_expr_remotes(ex))
  .sb_unique_clean(rem)
}


# ----------------------------
# GitHub remote detection
# ----------------------------

.sb_extract_github_remotes_from_call <- function(expr) {
  out <- character()
  if (!is.call(expr)) return(out)

  fn <- expr[[1]]

  # remotes::install_github("owner/repo") or devtools::install_github("owner/repo")
  if (is.call(fn) && identical(fn[[1]], as.name("::")) && length(fn) >= 3) {
    pkg <- fn[[2]]
    fun <- fn[[3]]

    if (is.name(pkg) && is.name(fun) && as.character(fun) == "install_github") {
      if (as.character(pkg) %in% c("remotes", "devtools")) {
        if (length(expr) >= 2) {
          arg <- expr[[2]]
          if (.sb_is_string1(arg)) out <- c(out, arg)
        }
      }
    }
  }

  .sb_unique_clean(out)
}

.sb_walk_expr_remotes <- function(expr) {
  rem <- character()
  walk <- function(x) {
    if (is.call(x)) {
      rem <<- c(rem, .sb_extract_github_remotes_from_call(x))
      for (i in seq_along(x)) walk(x[[i]])
    } else if (is.pairlist(x) || is.list(x)) {
      for (i in seq_along(x)) walk(x[[i]])
    }
  }
  walk(expr)
  .sb_unique_clean(rem)
}

.sb_parse_file_remotes <- function(file) {
  file <- normalizePath(file, winslash = "/", mustWork = FALSE)
  txt <- .sb_read_text(file)
  if (!length(txt)) return(character())
  exprs <- tryCatch(parse(text = txt, keep.source = FALSE), error = function(e) NULL)
  if (is.null(exprs)) return(character())

  rem <- character()
  for (ex in as.list(exprs)) rem <- c(rem, .sb_walk_expr_remotes(ex))
  .sb_unique_clean(rem)
}

scan_remotes <- function(path) {
  files <- .sb_collect_files_recursive(path)
  rem <- character()
  for (f in files) rem <- c(rem, .sb_parse_file_remotes(f))
  .sb_unique_clean(rem)
}



scan_remotes <- function(path) {
  # Not exported by default (we can export later if you want)
  files <- .sb_collect_files_recursive(path)
  rem <- character()
  for (f in files) rem <- c(rem, .sb_parse_file_remotes(f))
  .sb_unique_clean(rem)
}





#' Print copy-paste install commands for detected dependencies
#'
#' @param path Script or folder to scan.
#' @param cran_repo CRAN repo URL used for classification.
#' @export
print_install_commands <- function(path, cran_repo = "https://cloud.r-project.org") {
  cls <- classify_deps(scan_deps(path), cran_repo = cran_repo)
  rem <- scan_remotes(path)

  if (requireNamespace("cli", quietly = TRUE)) {
    cli::cli_h2("Copy/paste install commands")
  } else {
    message("Copy/paste install commands")
  }

  if (length(cls$cran) > 0) {
    cmd <- paste0("install.packages(c(", paste(sprintf('\"%s\"', cls$cran), collapse = ", "), "), repos = \"", cran_repo, "\")")
    cat(cmd, "\n\n")
  } else {
    cat("# CRAN: none\n\n")
  }

  if (length(cls$bioc) > 0) {
    cat("if (!requireNamespace(\"BiocManager\", quietly = TRUE)) install.packages(\"BiocManager\")\n")
    cmd <- paste0("BiocManager::install(c(", paste(sprintf('\"%s\"', cls$bioc), collapse = ", "), "), ask = FALSE, update = FALSE)")
    cat(cmd, "\n\n")
  } else {
    cat("# Bioconductor: none\n\n")
  }

  if (length(rem) > 0) {
    cat("if (!requireNamespace(\"remotes\", quietly = TRUE)) install.packages(\"remotes\")\n")
    cmd <- paste0("remotes::install_github(c(", paste(sprintf('\"%s\"', rem), collapse = ", "), "))")
    cat(cmd, "\n\n")
  } else {
    cat("# GitHub remotes: none detected\n\n")
  }

  invisible(list(classified = cls, remotes = rem))
}

#' Create a reproducible renv lockfile for a script/project
#'
#' This installs `renv` if needed, initializes renv (bare), and snapshots to `renv.lock`.
#'
#' @param path Script or folder to scan.
#' @param project_dir Directory to create renv files in (default = current project).
#' @param cran_repo CRAN repo URL.
#' @export
scriptbuddy_lock <- function(path, project_dir = ".", cran_repo = "https://cloud.r-project.org") {
  if (!requireNamespace("renv", quietly = TRUE)) {
    utils::install.packages("renv", repos = cran_repo)
  }

  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(project_dir)

  # Initialize if not already initialized
  if (!file.exists("renv.lock")) {
    renv::init(bare = TRUE)
  }

  # Install detected deps into renv library
  cls <- install_deps(path, cran_repo = cran_repo, quiet = FALSE)

  # Snapshot lockfile
  renv::snapshot(prompt = FALSE)

  if (requireNamespace("cli", quietly = TRUE)) cli::cli_alert_success("Created/updated renv.lock")
  invisible(cls)
}

#' Create or update a reproducible renv lockfile for a script/project
#'
#' @param path Script or folder to scan.
#' @param project_dir Directory where renv should live (default: current project).
#' @param cran_repo CRAN repo URL.
#' @export
scriptbuddy_lock <- function(path, project_dir = ".", cran_repo = "https://cloud.r-project.org") {
  if (!requireNamespace("renv", quietly = TRUE)) {
    utils::install.packages("renv", repos = cran_repo)
  }

  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(project_dir)

  # Initialize renv if not already present
  if (!file.exists("renv.lock")) {
    renv::init(bare = TRUE)
  }

  # Install detected deps (CRAN + Bioc). GitHub installs are printed separately.
  cls <- install_deps(path, cran_repo = cran_repo, quiet = FALSE)
  rem <- scan_remotes(path)

  # Snapshot lockfile
  renv::snapshot(prompt = FALSE)

  if (requireNamespace("cli", quietly = TRUE)) {
    cli::cli_alert_success("Created/updated renv.lock")
    if (length(rem) > 0) {
      cli::cli_alert_warning("GitHub remotes detected (not auto-installed into renv):")
      cli::cli_text(paste("•", rem, collapse = "\n"))
      cli::cli_alert_info("Run: print_install_commands(path) to get the remotes::install_github(...) line.")
    }
  } else {
    message("Created/updated renv.lock")
    if (length(rem) > 0) {
      message("GitHub remotes detected (not auto-installed): ", paste(rem, collapse = ", "))
    }
  }

  invisible(list(classified = cls, github = rem))
}


##
#' Write a dependency report to disk
#'
#' @param path Script or folder to scan.
#' @param out_dir Output directory.
#' @param cran_repo CRAN repo URL used for classification.
#' @return Invisibly returns the classification list.
#' @export
write_deps_report <- function(path, out_dir = "scriptbuddy_report", cran_repo = "https://cloud.r-project.org") {
  pkgs <- scan_deps(path)
  cls <- classify_deps(pkgs, cran_repo = cran_repo)
  rem <- scan_remotes(path)

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # CSV summary
  df <- data.frame(
    package = c(cls$cran, cls$bioc, cls$unknown, rem),
    source = c(
      rep("CRAN", length(cls$cran)),
      rep("Bioconductor", length(cls$bioc)),
      rep("Unknown", length(cls$unknown)),
      rep("GitHub", length(rem))
    ),
    stringsAsFactors = FALSE
  )

  utils::write.csv(df, file.path(out_dir, "deps.csv"), row.names = FALSE)

  # Plain-text lists
  writeLines(cls$cran, file.path(out_dir, "cran.txt"))
  writeLines(cls$bioc, file.path(out_dir, "bioc.txt"))
  writeLines(cls$unknown, file.path(out_dir, "unknown.txt"))
  writeLines(rem, file.path(out_dir, "github.txt"))

  invisible(list(classified = cls, github = rem))
}

#' Print copy/paste install commands for detected dependencies
#'
#' @param path Script or folder to scan.
#' @param cran_repo CRAN repo URL used for classification.
#' @export
print_install_commands <- function(path, cran_repo = "https://cloud.r-project.org") {
  cls <- classify_deps(scan_deps(path), cran_repo = cran_repo)
  rem <- scan_remotes(path)

  if (requireNamespace("cli", quietly = TRUE)) {
    cli::cli_h2("Copy/paste install commands")
  } else {
    message("Copy/paste install commands")
  }

  # CRAN
  if (length(cls$cran) > 0) {
    cmd <- paste0(
      "install.packages(c(",
      paste(sprintf('\"%s\"', cls$cran), collapse = ", "),
      "), repos = \"", cran_repo, "\")"
    )
    cat(cmd, "\n\n")
  } else {
    cat("# CRAN: none\n\n")
  }

  # Bioconductor
  if (length(cls$bioc) > 0) {
    cat("if (!requireNamespace(\"BiocManager\", quietly = TRUE)) install.packages(\"BiocManager\")\n")
    cmd <- paste0(
      "BiocManager::install(c(",
      paste(sprintf('\"%s\"', cls$bioc), collapse = ", "),
      "), ask = FALSE, update = FALSE)"
    )
    cat(cmd, "\n\n")
  } else {
    cat("# Bioconductor: none\n\n")
  }

  # GitHub remotes
  if (length(rem) > 0) {
    cat("if (!requireNamespace(\"remotes\", quietly = TRUE)) install.packages(\"remotes\")\n")
    cmd <- paste0(
      "remotes::install_github(c(",
      paste(sprintf('\"%s\"', rem), collapse = ", "),
      "))"
    )
    cat(cmd, "\n\n")
  } else {
    cat("# GitHub remotes: none detected\n\n")
  }

  invisible(list(classified = cls, github = rem))
}

#' Quiet wrapper for ScriptBuddy
#'
#' Same as `scriptbuddy()` but suppresses verbose output.
#'
#' @param path Script or folder to scan.
#' @param install If TRUE, installs missing packages.
#' @param cran_repo CRAN repo URL.
#' @export
scriptbuddy_quiet <- function(path, install = FALSE, cran_repo = "https://cloud.r-project.org") {
  scriptbuddy(path, install = install, verbose = FALSE, cran_repo = cran_repo)
}
#' Pretty dependency report table
#'
#' @param path Script or folder to scan.
#' @param cran_repo CRAN repo URL.
#' @export
scriptbuddy_report <- function(path, cran_repo = "https://cloud.r-project.org") {
  cls <- classify_deps(scan_deps(path), cran_repo = cran_repo)
  gh  <- scan_remotes(path)

  df <- data.frame(
    Package = c(cls$cran, cls$bioc, gh, cls$unknown),
    Source = c(
      rep("CRAN", length(cls$cran)),
      rep("Bioconductor", length(cls$bioc)),
      rep("GitHub", length(gh)),
      rep("Unknown", length(cls$unknown))
    ),
    stringsAsFactors = FALSE
  )

  if (requireNamespace("cli", quietly = TRUE)) {
    cli::cli_h2("ScriptBuddy Dependency Report")
    print(df, row.names = FALSE)
  } else {
    print(df)
  }

  invisible(df)
}

