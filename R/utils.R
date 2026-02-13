# ---- helpers ----
fmt_set <- function(s){
  s <- if (is.null(s)) character(0) else if (is.list(s)) unlist(s, use.names = FALSE) else s
  s <- as.character(s)
  paste0("{", paste(sort(unique(s)), collapse = ","), "}")
}
normalize_sets <- function(S){
  L <- as.list(S)
  if (length(L) == 0) return(list())
  lapply(L, function(s){
    if (is.null(s)) character(0) else as.character(unlist(s, use.names = FALSE))
  })
}

find_paths <- function(g, X, Y, curr_path = c(X), seen = c()) {
  if (X == Y) {
    return(list(curr_path))
  }

  seen <- c(seen, X)
  paths <- list()

  for (nbr in c(children(g, X))) {
    if (nbr %in% seen) next

    subpaths <- find_paths(
      g,
      nbr,
      Y,
      curr_path = c(curr_path, nbr),
      seen = seen
    )

    if (length(subpaths)) {
      paths <- c(paths, subpaths)
    }
  }

  paths
}

subset_paths <- function(paths, X, Y) {
  # For each path, generate all non-empty subsets (excluding X and Y)
  path_subsets <- list()

  for (path in paths) {
    # Remove X and Y from the path
    path_nodes <- setdiff(path, c(X, Y))

    # Skip if no intermediate nodes
    if (length(path_nodes) == 0) next

    path_subs <- list()
    for (i in 1:length(path_nodes)) {
      path_subs <- c(path_subs, combn(path_nodes, i, simplify = FALSE))
    }
    path_subsets[[length(path_subsets) + 1]] <- path_subs
  }

  # Now generate Cartesian product of all path subsets
  if (length(path_subsets) == 0) return(list())

  result <- lapply(path_subsets[[1]], function(x) list(x))

  if (length(path_subsets) > 1) {
    for (i in 2:length(path_subsets)) {
      new_result <- list()
      for (existing_combo in result) {
        for (new_subset in path_subsets[[i]]) {
          combined <- unlist(c(existing_combo, list(new_subset)), use.names = FALSE)
          new_result[[length(new_result) + 1]] <- combined
        }
      }
      result <- new_result
    }
  } else {
    result <- lapply(result, function(x) x[[1]])
  }

  # Remove duplicates
  result <- unique(lapply(result, function(x) sort(unique(x))))

  result
}

