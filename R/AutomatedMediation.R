# ============================================================
# Automated Mediation Runner (RStudio) — uses Python PROCESS
# via reticulate + pyprocessmacro
# 
# This program will automatically run every possible mediation analysis
# on variables you enter from a CSV file. Type 'quit' anytime to exit.
# 
# Requirements:
# 1) Internet access the first time (to install Python packages)
# 2) Your data set needs to be csv formatted and in your working directory
# 3) Variables must be your column names, must be numeric, and have no missing values.
#
# On first run may take 5-10 minutes to:
#   - Create a Python virtualenv "r-pyproc-env"
#   - Install numpy<2, pandas, pyprocessmacro
# Afterwards:
#   - Reuses that env automatically
#
# *Each variable entered exponentially increases the number of mediations run
# and thus the duration of analysis time
# ============================================================

# Silence all Python warnings globally (before Python starts)
Sys.setenv(PYTHONWARNINGS = "ignore")

suppressPackageStartupMessages({
  library(reticulate)
  library(utils)
})

newline <- function() cat("\n\n")
line    <- function() cat("_____________________________________________________________\n")

checkForQuit <- function(x) {
  if (tolower(trimws(x)) == "quit") return(FALSE)
  x
}

# ---------- Ensure Python + pyprocessmacro (self-contained) ----------

.ensure_python_process <- function() {
  
  env_name <- "r-pyproc-env"  # name of the virtualenv
  env_dir  <- file.path(virtualenv_root(), env_name)
  
  # 1) If the env doesn't exist yet, create it and install packages
  if (!dir.exists(env_dir)) {
    message("\nCreating Python virtualenv '", env_name, "'...\n")
    
    py <- Sys.which("python3")
    if (py == "") py <- Sys.which("python")
    if (py == "")
      stop(
        "No system Python found.\n",
        "Please install Python 3 (e.g., from python.org) and re-run.\n",
        call. = FALSE
      )
    
    # Create env
    virtualenv_create(envname = env_name, python = py)
    
    message("\nInstalling numpy<2, pandas, pyprocessmacro in '", env_name, "'...\n")
    py_install(
      packages = c("numpy<2", "pandas", "pyprocessmacro"),
      envname  = env_name,
      method   = "virtualenv",
      pip      = TRUE
    )
  }
  
  # 2) Tell reticulate to use this env BEFORE any Python initialization
  use_virtualenv(env_name, required = TRUE)
  
  # 3) Make sure required modules are importable
  if (!py_module_available("numpy") ||
      !py_module_available("pandas") ||
      !py_module_available("pyprocessmacro")) {
    stop(
      "Python env '", env_name, "' is missing numpy/pandas/pyprocessmacro.\n",
      "Try restarting R and re-running this script so it can reinstall.",
      call. = FALSE
    )
  }
  
  # 4) Safety patch: if for some reason numpy.product doesn't exist, alias it
  py_run_string("
import numpy as np
if not hasattr(np, 'product'):
    np.product = np.prod
")
  
  invisible(TRUE)
}

# Convert R data.frame -> pandas.DataFrame
.to_pandas <- function(df) {
  # Let reticulate convert automatically
  reticulate::r_to_py(df, convert = TRUE)
}

# ---------- Main ----------

automated_mediation <- function() {
  options(warn = -1)
  newline()
  cat(
    "This program will automatically run every possible mediation analysis\n",
    "on variables you enter from a CSV file. Type 'quit' anytime to exit.\n",
    sep = ""
  )
  newline()
  cat("Requirements:\n")
  cat("1) R package 'reticulate' installed\n")
  cat("2) Internet access the first time (to install Python packages)\n")
  cat("3) Your CSV should be in this working directory:\n   ", getwd(), "\n",
      "If this is the wrong directory type 'quit', update your directory, and run the script again.", "\n", sep = "")
  cat("4) Column names are your variables, must be numeric, and have no missing values.\n")
  line()
  
  # Ensure Python + modules (auto-creates env if needed)
  .ensure_python_process()
  
  # Globally ignore all Python warnings inside this session
  py_run_string("import warnings; warnings.filterwarnings('ignore')")
  
  # Define a Python wrapper so argument names don't collide with reticulate
  py_run_string("
from pyprocessmacro import Process

def _run_process_wrapper(dat, mdl, xvar, yvar, med_list, ctrls, ctrls_in, suppress_init):
    return Process(
        data=dat,
        model=mdl,
        x=xvar,
        y=yvar,
        m=med_list,
        controls=ctrls,
        controls_in=ctrls_in,
        suppr_init=suppress_init
    )
", local = FALSE)
  
  # --------- Read dataset ----------
  dataset <- NULL
  variablenames <- NULL
  control_values <- NULL
  want_direct <- FALSE
  want_csv <- FALSE
  csv_filename <- NULL
  
  repeat {
    datasetentry <- readline(
      "Enter the exact CSV file name (case-sensitive, e.g., 'data.csv'): "
    )
    ans <- checkForQuit(datasetentry); if (identical(ans, FALSE)) {
      newline(); return(invisible(FALSE))
    }
    datasetentry <- ans
    
    if (!file.exists(datasetentry)) {
      newline(); line()
      cat("File not found. Ensure your CSV is in:", getwd(), "and the name is exact.\n")
      line()
    } else {
      dataset <- tryCatch({
        read.csv(datasetentry, check.names = FALSE)
      }, error = function(e) NULL)
      
      if (is.null(dataset)) {
        newline(); line(); cat("Couldn't read CSV. Please try again.\n"); line()
      } else {
        names(dataset) <- gsub(" ", "", names(dataset), fixed = TRUE)
        variablenames <- names(dataset)
        newline(); line(); newline()
        cat("Here are the variables in your dataset:\n"); line(); newline()
        cat(paste(variablenames, collapse = ", "), "\n")
        break
      }
    }
  }
  
  # --------- Variables of interest ----------
  variablelist <- NULL
  repeat {
    uservariables <- readline(
      "Enter all interest variables (min 3) separated by a comma and a space: "
    )
    ans <- checkForQuit(uservariables); if (identical(ans, FALSE)) {
      newline(); return(invisible(FALSE))
    }
    
    variablelist <- strsplit(uservariables, ",\\s*")[[1]]
    if (length(variablelist) < 3) {
      newline(); line(); cat("You must enter at least 3 variables.\n"); line()
      next
    }
    missing_vars <- setdiff(variablelist, variablenames)
    if (length(missing_vars) > 0) {
      newline(); line()
      cat(
        "These variables are not in your dataset (case-sensitive): ",
        paste(missing_vars, collapse = ", "), "\n",
        sep = ""
      )
      cat("Please check spelling and separators (comma + space).\n")
      line()
      next
    }
    break
  }
  
  # --------- Direct effect output? ----------
  repeat {
    deq <- tolower(trimws(readline("Output direct effects for significant mediations in the console too? (yes/no): ")))
    ans <- checkForQuit(deq); if (identical(ans, FALSE)) {
      newline(); return(invisible(FALSE))
    }
    if (deq %in% c("yes","no")) { want_direct <- (deq == "yes"); break }
    cat("Please enter yes or no.\n")
  }
  
  # --------- Controls? ----------
  control_question <- NULL
  repeat {
    cq <- tolower(trimws(readline("Control for any variables? (yes/no): ")))
    ans <- checkForQuit(cq); if (identical(ans, FALSE)) {
      newline(); return(invisible(FALSE))
    }
    if (cq %in% c("yes","no")) { control_question <- cq; break }
    cat("Please enter yes or no.\n")
  }
  
  if (control_question == "yes") {
    repeat {
      usercontrols <- readline(
        "Enter control variables separated by a comma and a space: "
      )
      ans <- checkForQuit(usercontrols); if (identical(ans, FALSE)) {
        newline(); return(invisible(FALSE))
      }
      control_values <- strsplit(usercontrols, ",\\s*")[[1]]
      
      bad1 <- setdiff(control_values, variablenames)
      bad2 <- intersect(control_values, variablelist)
      if (length(bad1) > 0 || length(bad2) > 0) {
        newline()
        cat("Check controls — each must exist and cannot be one of the interest variables.\n")
      } else {
        break
      }
    }
  } else {
    control_values <- NULL
  }
  
  # --------- CSV output? ----------
  repeat {
    cq <- tolower(trimws(readline("Write results to a CSV file? (yes/no): ")))
    ans <- checkForQuit(cq); if (identical(ans, FALSE)) {
      newline(); return(invisible(FALSE))
    }
    if (cq %in% c("yes","no")) { want_csv <- (cq == "yes"); break }
    cat("Please enter yes or no.\n")
  }
  
  if (want_csv) {
    nm <- readline("Enter the base name for the output file (no extension): ")
    ans <- checkForQuit(nm); if (identical(ans, FALSE)) {
      newline(); return(invisible(FALSE))
    }
    csv_filename <- paste0(nm, ".csv")
  }
  
  # --------- Generate all 3-length permutations ----------
  perm3 <- function(v) {
    n <- length(v)
    out <- list()
    idx <- 1
    for (i in seq_len(n)) for (j in seq_len(n)) for (k in seq_len(n)) {
      if (i != j && j != k && i != k) {
        out[[idx]] <- c(v[i], v[j], v[k]); idx <- idx + 1
      }
    }
    do.call(rbind, out)
  }
  
  allpossibleorders <- perm3(variablelist)
  colnames(allpossibleorders) <- c("X","Y","M")
  
  # Prepare data for Python
  py_df <- .to_pandas(dataset)
  
  results <- list()
  newline(); line()
  
  for (i in seq_len(nrow(allpossibleorders))) {
    xvalue <- allpossibleorders[i, "X"]
    yvalue <- allpossibleorders[i, "Y"]
    mvalue <- allpossibleorders[i, "M"]
    
    newline()
    
    controls_arg <- if (is.null(control_values)) list() else as.list(control_values)
    
    # Call the Python wrapper
    p <- py$`_run_process_wrapper`(
      dat           = py_df,
      mdl           = 4L,
      xvar          = xvalue,
      yvar          = yvalue,
      med_list      = list(mvalue),
      ctrls         = controls_arg,
      ctrls_in      = "all",
      suppress_init = TRUE
    )
    
    estimates <- p$indirect_model$coeff_summary()
    directest <- p$direct_model$coeff_summary()
    
    names(estimates) <- gsub(" ", "", names(estimates), fixed = TRUE)
    names(directest) <- gsub(" ", "", names(directest), fixed = TRUE)
    
    boot_LLCI <- as.numeric(estimates$BootLLCI[1])
    boot_ULCI <- as.numeric(estimates$BootULCI[1])
    boot_SE   <- as.numeric(estimates$BootSE[1])
    indirect_effect <- as.numeric(estimates$Effect[1])
    ci_width <- boot_ULCI - boot_LLCI
    
    direct_p   <- suppressWarnings(as.numeric(directest$p[1]))
    direct_eff <- suppressWarnings(as.numeric(directest$Effect[1]))
    direct_se  <- suppressWarnings(as.numeric(directest$SE[1]))
    
    indirect_sig <- if ((boot_LLCI > 0 && boot_ULCI > 0) ||
                        (boot_LLCI < 0 && boot_ULCI < 0)) "Significant" else "Not Significant"
    direct_sig <- if (!is.na(direct_p) && direct_p < 0.05) "Significant" else "Not Significant"
    
    if (boot_LLCI > 0 && boot_ULCI > 0) {
      cat("Significantly positive indirect effect\n"); newline()
      cat("Put simply:", xvalue, "indirectly predicts", yvalue, "via", mvalue, "\n"); newline()
      cat("X=", xvalue, " Y=", yvalue, " M=", mvalue, "\n"); newline()
      if (!is.null(control_values)) {
        cat("Control Variables:", paste(control_values, collapse = ", "), "\n"); newline()
      }
      print(estimates); newline()
      if (!is.na(direct_p) && direct_p < 0.05 && !is.na(direct_eff) && direct_eff > 0) {
        cat("Significantly positive direct effect\n")
      } else if (!is.na(direct_p) && direct_p < 0.05 && !is.na(direct_eff) && direct_eff < 0) {
        cat("Significantly negative direct effect\n")
      } else {
        cat("No significant direct effect\n")
      }
      if (want_direct) { newline(); print(p$direct_model$summary()) }
      line(); newline()
      
    } else if (boot_LLCI < 0 && boot_ULCI < 0) {
      cat("Significantly negative indirect effect\n"); newline()
      cat("Put simply:", xvalue, "indirectly predicts", yvalue, "via", mvalue, "\n"); newline()
      cat("X=", xvalue, " Y=", yvalue, " M=", mvalue, "\n"); newline()
      if (!is.null(control_values)) {
        cat("Control Variables:", paste(control_values, collapse = ", "), "\n"); newline()
      }
      print(estimates); newline()
      if (!is.na(direct_p) && direct_p < 0.05 && !is.na(direct_eff) && direct_eff > 0) {
        cat("Significantly positive direct effect\n")
      } else if (!is.na(direct_p) && direct_p < 0.05 && !is.na(direct_eff) && direct_eff < 0) {
        cat("Significantly negative direct effect\n")
      } else {
        cat("No significant direct effect\n")
      }
      if (want_direct) { newline(); print(p$direct_model$summary()) }
      line(); newline()
      
    } else {
      cat("Indirect effect not significant\n")
      cat("X=", xvalue, " Y=", yvalue, " M=", mvalue, "\n")
      line()
    }
    
    results[[length(results) + 1]] <- data.frame(
      X = xvalue,
      Y = yvalue,
      M = mvalue,
      Controls = if (is.null(control_values)) "" else paste(control_values, collapse = ";"),
      Indirect_Significance = indirect_sig,
      Indirect_Effect = indirect_effect,
      CI_Width = ci_width,
      Boot_LLCI = boot_LLCI,
      Boot_ULCI = boot_ULCI,
      Boot_SE = boot_SE,
      Direct_Significance = direct_sig,
      Direct_Effect = direct_eff,
      Direct_Pvalue = direct_p,
      Direct_SE = direct_se,
      stringsAsFactors = FALSE
    )
  }
  
  if (want_csv) {
    out <- do.call(rbind, results)
    utils::write.csv(out, csv_filename, row.names = FALSE)
    newline(); cat("Saved results to:", csv_filename, "\n", "in directory:", getwd())
  }
  
  newline(); cat("Thanks for using the automated mediation program!\n")
  invisible(TRUE)
}

