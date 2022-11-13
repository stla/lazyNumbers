c_lv <- function(lv1, lv2) {
  lv2 <- as.lazyVector(lv2)
  xptr <- lazyConcat(lv1@xptr, lv2@xptr)
  l <- lv1@length + lv2@length
  new("lazyVector", xptr = xptr, length = l)
}

c_lm_lv <- function(lm, lv) {
  lv <- as.lazyVector(lv)
  xptr <- lazyConcat(lazyFlatten(lm@xptr), lv@xptr)
  l <- lm@nrow*lm@ncol + lv@length
  new("lazyVector", xptr = xptr, length = l)
}

#' @name concat-lazyObjects
#' @aliases c,lazyVector-method c,lazyMatrix-method
#' @title Concatenation of lazy vectors
#' @description Concatenate two or more \code{lazyVector} or \code{lazyMatrix} 
#'   objects.
#' @param x a \code{lazyVector} object or a \code{lazyMatrix} object
#' @param ... \code{lazyVector} objects or \code{lazyMatrix} objects or numeric 
#'   vectors or numeric matrices
#' @return A \code{lazyVector} object. 
setMethod(
  "c",
  signature(x = "lazyVector"),
  function(x, ...) {
    if(nargs() == 1L) {
      x
    } else if(nargs() == 2L) {
      c_lv(x, c(...))
    } else {
      c_lv(x, Recall(...))
    }
  }
)

#' @rdname concat-lazyObjects
setMethod(
  "c",
  signature(x = "lazyMatrix"),
  function(x, ...) {
    if(nargs() == 1L) {
      as.lazyVector.lazyMatrix(x)
    } else if(nargs() == 2L) {
      c_lm_lv(x, c(...))
    } else {
      c_lm_lv(x, Recall(...))
    }
  }
)


cbind_lm <- function(lm1, lm2) {
  stopifnot(lm1@nrow == lm2@nrow)
  xptr <- lazyCbind(lm1@xptr, lm2@xptr)
  n <- lm1@ncol + lm2@ncol
  new("lazyMatrix", xptr = xptr, nrow = lm1@nrow, ncol = n)
}

rbind_lm <- function(lm1, lm2) {
  stopifnot(lm1@ncol == lm2@ncol)
  xptr <- lazyRbind(lm1@xptr, lm2@xptr)
  m <- lm1@nrow + lm2@nrow
  new("lazyMatrix", xptr = xptr, nrow = m, ncol = lm1@ncol)
}

as.lazyRowMatrix <- function(lv) {
  lvx <- lv@xptr
  lmx <- lazyRowMatrix(lvx)
  new("lazyMatrix", xptr = lmx, nrow = 1L, ncol = lv@length)
}


#' @name bind2-lazyMatrices
#' @aliases cbind2,lazyMatrix,missing-method cbind2,lazyMatrix,lazyMatrix-method cbind2,lazyVector,missing-method cbind2,lazyVector,lazyMatrix-method cbind2,lazyMatrix,lazyVector-method cbind2,lazyVector,lazyVector-method cbind2,lazyMatrix,numeric-method cbind2,numeric,lazyMatrix-method rbind2,lazyMatrix,missing-method rbind2,lazyMatrix,lazyMatrix-method rbind2,lazyVector,missing-method rbind2,lazyVector,lazyMatrix-method rbind2,lazyMatrix,lazyVector-method rbind2,lazyVector,lazyVector-method cbind2,lazyVector,numeric-method cbind2,numeric,lazyVector-method cbind2,lazyVector,matrix-method cbind2,matrix,lazyVector-method cbind2,lazyMatrix,matrix-method cbind2,matrix,lazyMatrix-method rbind2,lazyVector,numeric-method rbind2,numeric,lazyVector-method rbind2,lazyVector,matrix-method rbind2,matrix,lazyVector-method rbind2,lazyMatrix,matrix-method rbind2,matrix,lazyMatrix-method cbind2,lazyMatrix,numeric-method cbind2,numeric,lazyMatrix-method rbind2,lazyMatrix,numeric-method rbind2,numeric,lazyMatrix-method
#' @title Concatenation of lazy matrices
#' @description Concatenate two \code{lazyMatrix} objects.
#' @param x,y \code{lazyMatrix} or \code{lazyVector} objects
#' @return A \code{lazyMatrix} object.
#' @importFrom methods rbind2 cbind2
#' @exportMethod rbind2
#' @exportMethod cbind2
setMethod(
  "cbind2",
  signature(x = "lazyMatrix", y = "missing"),
  function(x, y) {
    x
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "cbind2",
  signature(x = "lazyVector", y = "missing"),
  function(x, y) {
    as.lazyMatrix.lazyVector(x)
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "cbind2",
  signature(x = "lazyMatrix", y = "lazyMatrix"),
  function(x, y) {
    cbind_lm(x, y)
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "cbind2",
  signature(x = "lazyVector", y = "lazyMatrix"),
  function(x, y) {
    cbind_lm(as.lazyMatrix.lazyVector(x), y)
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "cbind2",
  signature(x = "lazyMatrix", y = "lazyVector"),
  function(x, y) {
    cbind_lm(x, as.lazyMatrix.lazyVector(y))
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "cbind2",
  signature(x = "lazyVector", y = "lazyVector"),
  function(x, y) {
    cbind_lm(as.lazyMatrix.lazyVector(x), as.lazyMatrix.lazyVector(y))
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "cbind2",
  signature(x = "lazyVector", y = "numeric"),
  function(x, y) {
    cbind_lm(as.lazyMatrix.lazyVector(x), as.lazyMatrix.matrix(cbind(y)))
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "cbind2",
  signature(x = "numeric", y = "lazyVector"),
  function(x, y) {
    cbind_lm(as.lazyMatrix.matrix(cbind(x)), as.lazyMatrix.lazyVector(y))
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "cbind2",
  signature(x = "lazyVector", y = "matrix"),
  function(x, y) {
    cbind_lm(as.lazyMatrix.lazyVector(x), as.lazyMatrix.matrix(y))
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "cbind2",
  signature(x = "matrix", y = "lazyVector"),
  function(x, y) {
    cbind_lm(as.lazyMatrix.matrix(x), as.lazyMatrix.lazyVector(y))
  }
)

setMethod(
  "cbind2",
  signature(x = "lazyMatrix", y = "matrix"),
  function(x, y) {
    cbind_lm(x, as.lazyMatrix.matrix(y))
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "cbind2",
  signature(x = "matrix", y = "lazyMatrix"),
  function(x, y) {
    cbind_lm(as.lazyMatrix.matrix(x), y)
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "cbind2",
  signature(x = "lazyMatrix", y = "numeric"),
  function(x, y) {
    cbind_lm(x, as.lazyMatrix.matrix(cbind(y)))
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "cbind2",
  signature(x = "numeric", y = "lazyMatrix"),
  function(x, y) {
    cbind_lm(as.lazyMatrix.matrix(cbind(x)), y)
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "rbind2",
  signature(x = "lazyMatrix", y = "missing"),
  function(x, y) {
    x
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "rbind2",
  signature(x = "lazyVector", y = "missing"),
  function(x, y) {
    as.lazyRowMatrix(x)
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "rbind2",
  signature(x = "lazyMatrix", y = "lazyMatrix"),
  function(x, y) {
    rbind_lm(x, y)
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "rbind2",
  signature(x = "lazyVector", y = "lazyMatrix"),
  function(x, y) {
    rbind_lm(as.lazyRowMatrix(x), y)
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "rbind2",
  signature(x = "lazyMatrix", y = "lazyVector"),
  function(x, y) {
    rbind_lm(x, as.lazyRowMatrix(y))
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "rbind2",
  signature(x = "lazyVector", y = "lazyVector"),
  function(x, y) {
    rbind_lm(as.lazyRowMatrix(x), as.lazyRowMatrix(y))
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "rbind2",
  signature(x = "lazyVector", y = "numeric"),
  function(x, y) {
    rbind_lm(as.lazyRowMatrix(x), as.lazyMatrix.matrix(rbind(y)))
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "rbind2",
  signature(x = "numeric", y = "lazyVector"),
  function(x, y) {
    rbind_lm(as.lazyMatrix.matrix(rbind(x)), as.lazyRowMatrix(y))
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "rbind2",
  signature(x = "lazyVector", y = "matrix"),
  function(x, y) {
    rbind_lm(as.lazyMatrix.lazyVector(x), as.lazyMatrix.matrix(y))
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "rbind2",
  signature(x = "matrix", y = "lazyVector"),
  function(x, y) {
    rbind_lm(as.lazyMatrix.matrix(x), as.lazyMatrix.lazyVector(y))
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "rbind2",
  signature(x = "lazyMatrix", y = "matrix"),
  function(x, y) {
    rbind_lm(x, as.lazyMatrix.matrix(y))
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "rbind2",
  signature(x = "matrix", y = "lazyMatrix"),
  function(x, y) {
    rbind_lm(as.lazyMatrix.matrix(x), y)
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "rbind2",
  signature(x = "lazyMatrix", y = "numeric"),
  function(x, y) {
    rbind_lm(x, as.lazyMatrix.matrix(rbind(y)))
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "rbind2",
  signature(x = "numeric", y = "lazyMatrix"),
  function(x, y) {
    cbind_lm(as.lazyMatrix.matrix(rbind(x)), y)
  }
)


#' @exportS3Method cbind lazyVector
cbind.lazyVector <- function(..., recursive = FALSE) {
  nargs <- length(list(...))
  if(nargs <= 2L) {
    cbind2(...)
  } else {
    xs <- list(...)
    cbind2(xs[[1L]], do.call(Recall, xs[-1L]))
  }
}

#' @exportS3Method cbind lazyMatrix
cbind.lazyMatrix <- function(..., recursive = FALSE) {
  nargs <- length(list(...))
  if(nargs <= 2L) {
    cbind2(...)
  } else {
    xs <- list(...)
    cbind2(xs[[1L]], do.call(Recall, xs[-1L]))
  }
}

#' @exportS3Method rbind lazyVector
rbind.lazyVector <- function(..., recursive = FALSE) {
  nargs <- length(list(...))
  if(nargs <= 2L) {
    rbind2(...)
  } else {
    xs <- list(...)
    rbind2(xs[[1L]], do.call(Recall, xs[-1L]))
  }
}

#' @exportS3Method rbind lazyMatrix
rbind.lazyMatrix <- function(..., recursive = FALSE) {
  nargs <- length(list(...))
  if(nargs <= 2L) {
    rbind2(...)
  } else {
    xs <- list(...)
    rbind2(xs[[1L]], do.call(Recall, xs[-1L]))
  }
}