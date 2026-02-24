#' Find conditional FDs in an ADMG/DAG
#'
#' @param dag ADMG (in dagitty) on which FDs should be found.
#' @param X Exposure (String).
#' @param Y Outcome (String).
#' @param verbose Whether to print more detailed output (boolean).
#' @param adj_type Type of adjustment sets to be returned ("minimal"/"canonical"/"all").
#'
#' @returns S3 object of adjustment_X_M (list of adjustment sets between X and M), adjustment_M_Y (list of adjustment sets between M and Y), M (list of front-door mediators)
#' @importFrom dagitty dagitty adjustmentSets isAdjustmentSet children
#' @importFrom utils combn
#' @export
#'
#' @examples
#' dag1 <- dagitty::dagitty("dag {
#' X -> M
#' M -> Y
#' X [pos=\"0,0\"]
#'   M [pos=\"1,0\"]
#'   Y [pos=\"2,0\"]
#' }")
#'
#' find_fd(dag1, "X", "Y")
#'
#' dag2 <- dagitty::dagitty("dag {
#' A -> B
#' B -> C
#' U1 -> A
#' U1 -> B
#' U2 -> B
#' U2 -> C
#' U1 [pos=\"0,1\"]
#'   A  [pos=\"0,0\"]
#'   B  [pos=\"1,0.5\"]
#'   C  [pos=\"2,0\"]
#'   U2 [pos=\"2,1\"]
#' }")
#'
#' find_fd(dag2, "A", "C", verbose=FALSE, adj_type="canonical")



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

  # ---- STEP 2A: Adjustment A for X->M (use adjustmentSets) ----
  M_with_W <- list(); W_sets_all <- list()
  if (verbose) message("---- Exposure-Mediator Adjustment (block X <-> M) ----")
  for (M in cand_sets){
    W_raw  <- adjustmentSets(dag, exposure = X, outcome = M, type = adj_type)
    Wsets  <- normalize_sets(W_raw)

    Wsets  <- Filter(function(W) isAdjustmentSet(dag, W, exposure = X, outcome = M), Wsets)

    if (!length(Wsets)){
      if (verbose) message(fmt_set(M), ": NO W found - drop this M.")
    } else {
      if (verbose) message(fmt_set(M), ": found ", length(Wsets), " W set(s). Example: ", fmt_set(Wsets[[1]]))
      M_with_W[[length(M_with_W)+1]] <- M
      W_sets_all[[length(W_sets_all)+1]] <- Wsets
    }
  }
  if (!length(M_with_W)){
    if (verbose) message("No M passed Exposure-Mediator Adjustment.")
    cat("No valid front-door\n"); return(invisible(NULL))
  }

  # ---- STEP 2B: Adjustment B for M->Y (must remain valid when X is added) ----
  if (verbose) message("---- Mediator-Outcome Adjustment (block M <-> Y given X) ----")
  rows <- list()

  adjustment_X_M <- list()
  adjustment_M_Y <- list()
  M_list <- list()

  for (i in seq_along(M_with_W)){
    M   <- M_with_W[[i]]
    Ws  <- W_sets_all[[i]]

    T_raw  <- adjustmentSets(dag, exposure = M, outcome = Y, type = adj_type)
    Tsets  <- normalize_sets(T_raw)

    # If no T proposed at all, accept {} iff X alone is a valid adjustment set:
    if (length(Tsets) == 0L && isAdjustmentSet(dag, X, exposure = M, outcome = Y)) {
      Tsets <- list(character(0))  # meaning T = {}
    }

    # Keep only those T for which {X} U T is still a valid adjustment set
    Tsets <- Filter(function(T) isAdjustmentSet(dag, c(X, T), exposure = M, outcome = Y), Tsets)

    # If filtering removed all T but X alone works, record T = {}
    if (length(Tsets) == 0L && isAdjustmentSet(dag, X, exposure = M, outcome = Y)) {
      Tsets <- list(character(0))
    }

    # If still nothing, drop this M
    if (length(Tsets) == 0L){
      if (verbose) message(fmt_set(M), ": NO T found - drop this M.")
      next
    }

    if (verbose) message(fmt_set(M), ": ", length(Tsets), " T set(s). Example: ", fmt_set(Tsets[[1]]))

    # Cross product: list every (M, W, T) combo
    for (W in Ws){
      for (T in Tsets){
        rows[[length(rows)+1]] <- data.frame(
          "Front-door | "          = fmt_set(M),
          "Adjustment I (block X <-> Z) | "  = fmt_set(W),
          "Adjustment II (block Z <-> Y)"  = fmt_set(T),
          stringsAsFactors = FALSE,
          check.names      = FALSE
        )
        M_list[[length(M_list)+1]]                 <- M
        adjustment_X_M[[length(adjustment_X_M)+1]] <- W
        adjustment_M_Y[[length(adjustment_M_Y)+1]] <- T
      }
    }
  }

  # ---- STEP 3: final table (always print something) ----
  if (!length(rows)){
    cat("No valid front-door\n"); return(invisible(NULL))
  }
  out <- do.call(rbind, rows)
  cat("==== Final front-door solutions ====\n")
  print(out, row.names = FALSE)

  result <- structure(
    list(
      adjustment_X_M = adjustment_X_M,
      adjustment_M_Y = adjustment_M_Y,
      M              = M_list
    ),
    class = "front_door"
  )
  invisible(result)
}
