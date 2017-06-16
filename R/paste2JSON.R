# paste2JSON

#' @keywords internal
isCharArray <- function(x) {
  if (is.character(x) && length(x) > 0L) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Paste a mixed list of R objects and JSON strings to a valid JSON string
#' 
#' @param x List of objects.
#' @param keys Keys to use if return should be an object.
#' @param ... Further arguments passed on to jsonlite::toJSON.
#' @return If \code{is.null(keys)} JSON array else JSON object.
#'
#' @export
paste2JSON <- function(x, keys=NULL, ...) {
  stopifnot(is.list(x),
            is.null(keys) || isCharArray(keys) && length(keys) == length(x))
  # setup
  NULLKEYS <- is.null(keys)
  i <- 0L
  # peep through
  y <- lapply(x, function(obj) {
    i <<- i + 1L
    if (tryCatch(jsonlite::validate(obj),
                 error=function(e) FALSE)) {
      if (NULLKEYS) obj else paste0('"', keys[i], '":', obj)
    } else {
      if (NULLKEYS) {
        jsonlite::toJSON(obj, ...)
      } else {
        paste0('"', keys[i], '":', jsonlite::toJSON(obj, ...))
      }
    }
  })
  # packing
  z <- paste0(if (NULLKEYS) '[' else '{', 
              paste0(unlist(y), collapse=','), 
              if (NULLKEYS) ']' else '}')
  # serving
  return(structure(z, class='json'))
}