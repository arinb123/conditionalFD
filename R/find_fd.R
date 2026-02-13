#' Find conditional FDs in an ADMG/DAG
#'
#' @param dag ADMG (in daggity) on which FDs should be found.
#' @param X Exposure (String).
#' @param Y Outcome (String).
#' @param verbose Whether to print more detailed output (boolean).
#' @param adj_type Type of adjustment sets to be returned ("minimal"/"canonical"/"all").
#'
#' @returns Void; printed output.
#' @export
#'
#' @examples
#' dag1 <- dagitty("dag {
#' X -> M
#' M -> Y
#' X [pos=\"0,0\"]
#'   M [pos=\"1,0\"]
#'   Y [pos=\"2,0\"]
#' }")
#'
#' find_fd(dag1, "X", "Y")
#'
#'
#' Candidate nodes: M
#' Candidate sets:
#'  {M}
#' Intercept OK: {M}
#' ---- Adjustment A (block X <-> Z) ----
#'   {M}: found 1 W set(s). Example: {}
#' ---- Adjustment B (block Z <-> Y given X) ----
#'   {M}: 1 T set(s). Example: {}
#' ==== Final front-door solutions ====
#'   Front-door Adjustment A (X-Z) Adjustment B (Z-Y)
#' {M}                 {}                 {}


find_fd <- function(dag, X, Y, verbose=TRUE, adj_type="minimal") {
  # 1. Setup candidate nodes (all nodes except X and Y)
  stopifnot(is.character(X), length(X) == 1L,
            is.character(Y), length(Y) == 1L)
  nodes <- names(dag)
  if (!(X %in% nodes)) stop("Node '", X, "' not found in DAG.")
  if (!(Y %in% nodes)) stop("Node '", Y, "' not found in DAG.")

  # ---- STEP 0: fail-safe (direct edge X->Y?) ----
  if (Y %in% children(dag, X)){
    if (verbose) message("No valid front-door: direct edge ", X, " -> ", Y, " exists.")
    cat("No valid front-door\n")
    return(invisible(NULL))
  }

  # ---- STEP 1: candidate nodes with criterion 1 ----
  paths <- find_paths(dag, X, Y)
  cand_sets <- subset_paths(paths, X, Y)
  message(cand_sets)

  # ---- STEP 3A: Adjustment A for X->Z (use adjustmentSets) ----
  Z_with_W <- list(); W_sets_all <- list()
  if (verbose) message("---- Adjustment A (block X <-> Z) ----")
  for (Z in cand_sets){
    W_raw  <- adjustmentSets(dag, exposure = X, outcome = Z, type = adj_type)
    Wsets  <- normalize_sets(W_raw)

    Wsets  <- Filter(function(W) isAdjustmentSet(dag, W, exposure = X, outcome = Z), Wsets)

    if (!length(Wsets)){
      if (verbose) message(fmt_set(Z), ": NO W found — drop this Z.")
    } else {
      if (verbose) message(fmt_set(Z), ": found ", length(Wsets), " W set(s). Example: ", fmt_set(Wsets[[1]]))
      Z_with_W[[length(Z_with_W)+1]] <- Z
      W_sets_all[[length(W_sets_all)+1]] <- Wsets
    }
  }
  if (!length(Z_with_W)){
    if (verbose) message("No Z passed Adjustment A.")
    cat("No valid front-door\n"); return(invisible(NULL))
  }

  # ---- STEP 3B: Adjustment B for Z->Y (must remain valid when X is added) ----
  if (verbose) message("---- Adjustment B (block Z <-> Y given X) ----")
  rows <- list()

  for (i in seq_along(Z_with_W)){
    Z   <- Z_with_W[[i]]
    Ws  <- W_sets_all[[i]]

    T_raw  <- adjustmentSets(dag, exposure = Z, outcome = Y, type = adj_type)
    Tsets  <- normalize_sets(T_raw)

    # If no T proposed at all, accept {} iff X alone is a valid adjustment set:
    if (length(Tsets) == 0L && isAdjustmentSet(dag, X, exposure = Z, outcome = Y)) {
      Tsets <- list(character(0))  # meaning T = {}
    }

    # Keep only those T for which {X} ∪ T is still a valid adjustment set
    Tsets <- Filter(function(T) isAdjustmentSet(dag, c(X, T), exposure = Z, outcome = Y), Tsets)

    # If filtering removed all T but X alone works, record T = {}
    if (length(Tsets) == 0L && isAdjustmentSet(dag, X, exposure = Z, outcome = Y)) {
      Tsets <- list(character(0))
    }

    # If still nothing, drop this Z
    if (length(Tsets) == 0L){
      if (verbose) message(fmt_set(Z), ": NO T found — drop this Z.")
      next
    }

    if (verbose) message(fmt_set(Z), ": ", length(Tsets), " T set(s). Example: ", fmt_set(Tsets[[1]]))

    # Cross product: list every (Z, W, T) combo
    for (W in Ws){
      for (T in Tsets){
        rows[[length(rows)+1]] <- data.frame(
          "Front-door"          = fmt_set(Z),
          "Adjustment A (X-Z)"  = fmt_set(W),
          "Adjustment B (Z-Y)"  = fmt_set(T),
          stringsAsFactors = FALSE,
          check.names      = FALSE
        )
      }
    }
  }

  # ---- STEP 4: final table (always print something) ----
  if (!length(rows)){
    cat("No valid front-door\n"); return(invisible(NULL))
  }
  out <- do.call(rbind, rows)
  cat("==== Final front-door solutions ====\n")
  print(out, row.names = FALSE)
  invisible(out)
}
