#' Get names of all user-defined functions
#'
#' @export
#' @param code full stan code without any includes
#' @family code analysis functions
#' @return a data frame
get_functions <- function(code) {
  r <- find_functions(code)
  df <- count_functions_usage(r, code)
  return(df)
}

#' Find a block from Stan code
#'
#' @param code Stan code as a string
#' @param block name of block
#' @return a named list with three parts
find_block <- function(code, block) {
  pattern <- paste0(block, "*[{]")
  idx_op <- stringr::str_locate(code, pattern)[1, 2]
  if (is.na(idx_op)) {
    before <- code
    middle <- ""
    after <- ""
  } else {
    L <- nchar(code)
    before <- substr(code, 1, idx_op)
    rem <- substr(code, idx_op + 1, L)
    idx_cl <- find_closing(rem, "[}]", "[{]", 0)
    middle <- substr(rem, 1, idx_cl - 1)
    after <- substr(rem, idx_cl, nchar(rem))
  }
  out <- list(
    before = before,
    middle = middle,
    after = after
  )
  check_split(code, out)
  return(out)
}

#' Check that code split matches original code
#'
#' @param code full code
#' @param rec split
#' @param verbose should the message be printed
#' @return same as \code{readLines(file)}
check_split <- function(code, rec) {
  reconst <- paste0(rec$before, rec$middle, rec$after)
  if (code != reconst) {
    msg <- "incorrect code split, please report a bug!"
    stop(msg)
  }
  invisible(code)
}

find_closing <- function(code, split, increaser, base) {
  op <- stringr::str_locate_all(code, increaser)[[1]][, 1]
  cl <- stringr::str_locate_all(code, split)[[1]][, 1]
  N <- length(cl)
  for (i in seq_len(N)) {
    idx <- cl[i]
    if (sum(op < idx) + base < i) {
      return(idx)
    }
  }
  return(nchar(code) + 1)
}

find_functions <- function(code) {
  block <- find_block(code, "functions")$middle
  functions <- list()
  go <- TRUE
  j <- 0
  while (go) {
    split <- get_first_function(block)
    block <- split$rem
    func <- split$func
    if (nchar(func) > 0) {
      j <- j + 1
      func <- parse_function(func)
      functions <- c(functions, list(func))
    } else {
      go <- FALSE
    }
  }
  return(functions)
}

get_first_function <- function(code) {
  idx <- find_closing(code, "[}]", "[{]", -1)
  if (idx > nchar(code)) idx <- 0
  func <- substr(code, 1, idx)
  rem <- substr(code, idx + 1, nchar(code))
  out <- list(func = func, rem = rem)
  return(out)
}

parse_function <- function(code) {
  idx_op <- stringr::str_locate(code, "[(]")
  pre <- substr(code, 1, idx_op - 1)
  pre <- strsplit(pre, split = "\n")[[1]]
  pre <- pre[length(pre)]
  post <- substr(code, idx_op + 1, nchar(code))

  # Parse arguments
  idx_cl <- find_closing(post, "[)]", "[(]", 0)
  args <- substr(post, 1, idx_cl - 1)
  args <- trimws(args)
  args <- strsplit(args, split = ",")[[1]]

  # Parse name and return type
  loc <- stringr::str_locate_all(pre, " ")[[1]]
  idx <- loc[nrow(loc), 1]
  rtype <- substr(pre, 1, idx - 1)
  name <- substr(pre, idx + 1, nchar(pre))

  # Return
  out <- list(
    name = trimws(name),
    return_type = trimws(rtype),
    args = lapply(args, trimws),
    code = code
  )
  return(out)
}

count_functions_usage <- function(functions_list, code) {
  L <- length(functions_list)
  nams <- rep("foo", L)
  rtypes <- rep("foo", L)
  occurrences <- rep(0, L)
  for (j in seq_len(L)) {
    f <- functions_list[[j]]
    nams[j] <- f$name
    occurrences[j] <- nrow(stringr::str_locate_all(code, nams[j])[[1]]) - 1
    rtypes[j] <- f$return_type
  }
  df <- data.frame(nams, rtypes, occurrences)
  colnames(df) <- c("Name", "ReturnType", "Occurrences")
  return(df)
}
