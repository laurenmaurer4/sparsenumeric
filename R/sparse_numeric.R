#' Package Imports
#' @name sparsenumeric-imports
#' @keywords internal
#' @importFrom methods new show setClass setGeneric setMethod setValidity setAs
#' @importFrom graphics plot points legend
NULL

#' Sparse Numeric Vector Class
#'
#' @rdname sparse_numeric-class
#' @name sparse_numeric-class
#'
#' @slot pos Integer vector of non-zero indices
#' @slot value Numeric vector of non-zero values
#' @slot length Integer giving total vector length
#'
#' @export
setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)


#' Validity method for sparse_numeric
#' @rdname sparse_numeric-class
#' @name sparse_numeric-validity
setValidity(
  Class = "sparse_numeric",
  method = function(object){

    # Check value validity
    check <- any(object@value == 0)
    if(check)
      return("Some values are equal to zero")
    check <- is.numeric(object@value)
    if(!check)
      return("Some values are not numeric")

    # Check position validity
    check <- length(object@pos) == length(object@value)
    if(!check)
      return("pos and value must be the same length")
    check <- is.integer(object@pos)
    if(!check)
      return("pos must be an integer vector")
    check <- all(object@pos >= 1L & object@pos <= object@length)
    if(!check)
      return("Some positions are outside the valid range")
    check <- !any(duplicated(object@pos))
    if(!check)
      return("pos must not contain duplicates")

    # Check length
    check <- length(object@length) == 1L && is.integer(object@length)
    if(!check)
      return("length must be a single integer")
    check <- object@length > 0L
    if(!check)
      return("length must be >0")

    TRUE

  }
)

# Set generic functions

#' Sparse Vector Addition
#' @rdname sparse_add
#' @aliases sparse_add
#' @param x A sparse_numeric vector
#' @param y A sparse_numeric vector
#' @param ... Additional arguments
#' @export
setGeneric("sparse_add",
           function(x, y, ...) {
             standardGeneric("sparse_add")
           })

#' Sparse Vector Multiplication
#' @rdname sparse_mult
#' @aliases sparse_mult
#' @param x A sparse_numeric vector
#' @param y A sparse_numeric vector
#' @param ... Additional arguments
#' @export
setGeneric("sparse_mult",
           function(x, y, ...) {
             standardGeneric("sparse_mult")
           })

#' Sparse Vector Subtraction
#' @rdname sparse_sub
#' @aliases sparse_sub
#' @param x A sparse_numeric vector
#' @param y A sparse_numeric vector
#' @param ... Additional arguments
#' @export
setGeneric("sparse_sub",
           function(x, y, ...) {
             standardGeneric("sparse_sub")
           })

#' Sparse Vector Crossproduct
#' @rdname sparse_crossprod
#' @aliases sparse_crossprod
#' @param x A sparse_numeric vector
#' @param y A sparse_numeric vector
#' @param ... Additional arguments
#' @export
setGeneric("sparse_crossprod",
           function(x, y, ...) {
             standardGeneric("sparse_crossprod")
           })

#' Standardize Sparse Numeric Vector
#' @rdname standardize
#' @aliases standardize
#' @param x A sparse_numeric vector
#' @param ... Additional arguments
#' @export
setGeneric("standardize",
           function(x, ...){
             standardGeneric("standardize")
           })


## Addition
#' Add two sparse_numeric vectors
#'
#' @rdname sparse_add
#' @export
setMethod("sparse_add", c("sparse_numeric", "sparse_numeric"), function(x,y, ...){
  if(x@length != y@length)
    stop("Sparse vectors must have the same length")

  allpos <- sort(unique(c(x@pos, y@pos)))
  vx <- numeric(length(allpos))
  vy <- numeric(length(allpos))

  vx[match(x@pos, allpos)] <- x@value
  vy[match(y@pos, allpos)] <- y@value

  add_vals <- vx + vy

  keep <- which(add_vals != 0)
  if(length(keep) == 0L){
    new("sparse_numeric", value = numeric(0), pos = integer(0), length = x@length)
  }
  else {
    new("sparse_numeric",
        value = as.numeric(add_vals[keep]),
        pos = as.integer(allpos[keep]),
        length = x@length)
  }
})



## Multiplication
#' Multiply two sparse_numeric vectors elementwise
#' @rdname sparse_mult
#' @export
setMethod("sparse_mult", c("sparse_numeric", "sparse_numeric"), function(x,y, ...){
  if(x@length != y@length)
    stop("Sparse vectors must have the same length")

  allpos <- sort(unique(c(x@pos, y@pos)))
  vx <- numeric(length(allpos))
  vy <- numeric(length(allpos))

  vx[match(x@pos, allpos)] <- x@value
  vy[match(y@pos, allpos)] <- y@value

  mult_vals <- vx * vy

  keep <- which(mult_vals != 0)
  if(length(keep) == 0L){
    new("sparse_numeric", value = numeric(0), pos = integer(0), length = x@length)
  }
  else {
    new("sparse_numeric",
        value = as.numeric(mult_vals[keep]),
        pos = as.integer(allpos[keep]),
        length = x@length)
  }
})

## Subtraction
#' Subtract two sparse_numeric vectors
#' @rdname sparse_sub
#' @export
setMethod("sparse_sub", c("sparse_numeric", "sparse_numeric"), function(x,y, ...){
  if(x@length != y@length)
    stop("Sparse vectors must have the same length")

  allpos <- sort(unique(c(x@pos, y@pos)))
  vx <- numeric(length(allpos))
  vy <- numeric(length(allpos))

  vx[match(x@pos, allpos)] <- x@value
  vy[match(y@pos, allpos)] <- y@value

  sub_vals <- vx - vy

  keep <- which(sub_vals != 0)
  if(length(keep) == 0L){
    new("sparse_numeric", value = numeric(0), pos = integer(0), length = x@length)
  }
  else {
    new("sparse_numeric",
        value = as.numeric(sub_vals[keep]),
        pos = as.integer(allpos[keep]),
        length = x@length)
  }
})



## Cross product
#' Cross-product (inner product) of two sparse_numeric vectors
#' @rdname sparse_crossprod
#' @export
setMethod("sparse_crossprod", c("sparse_numeric", "sparse_numeric"), function(x,y, ...){
  if(x@length != y@length)
    stop("Sparse vectors must have the same length")

  allpos <- sort(unique(c(x@pos, y@pos)))
  vx <- numeric(length(allpos))
  vy <- numeric(length(allpos))

  vx[match(x@pos, allpos)] <- x@value
  vy[match(y@pos, allpos)] <- y@value

  crossprod_vals <- sum(vx * vy)

  crossprod_vals

})

## Mean
#' mean of sparse_numeric
#' @rdname mean
#' @aliases mean
#' @param x A sparse_numeric vector
#' @param ... Additional arguments (ignored)
#' @return A numeric scalar
#' @export
setMethod("mean", "sparse_numeric",
          function(x, ...){
            total <- sum(x@value)
            n <- as.numeric(x@length)
            total/n
          })

## Norm
#' Norm of sparse_numeric
#' @rdname norm
#' @aliases norm
#' @param x A sparse_numeric vector
#' @param type Norm type
#' @param ... Additional arguments
#' @return A numeric scalar
#'
#' @export
setMethod("norm", "sparse_numeric",
          function(x, type, ...){
            total <- sum((x@value)^2)
            sqrt(total)
          })

## Standardize
#' Standardize a sparse_numeric vector
#' @rdname standardize
#' @export
setMethod("standardize", "sparse_numeric",
          function(x, ...){
            v <- numeric(x@length)
            v[x@pos] <- x@value

            V_standard <- as.numeric(scale(v, center = TRUE, scale = TRUE))

            new_vec <- which(V_standard != 0)
            new("sparse_numeric",
                value = V_standard[new_vec],
                pos = as.integer(new_vec),
                length = x@length)
          })

## Implement +, -, *

#' Add two sparse_numeric vectors
#' @param e1 A sparse_numeric vector
#' @param e2 A sparse_numeric vector
#' @return A sparse_numeric vector
#' @export
setMethod("+", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))

#' Subtract two sparse_numeric vectors
#' @param e1 A sparse_numeric vector
#' @param e2 A sparse_numeric vector
#' @return A sparse_numeric vector
#' @export
setMethod("-", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))

#' Multiple two sparse_numeric vectors
#' @param e1 A sparse_numeric vector
#' @param e2 A sparse_numeric vector
#' @return A sparse_numeric vector
#' @export
setMethod("*", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))


# Coerce
setAs("numeric", "sparse_numeric",
      function(from){
        pos <- which(from != 0)
        value <- from[pos]
        len <- length(from)
        new("sparse_numeric", value = value, pos = as.integer(pos), length = as.integer(len))
      })

setAs("sparse_numeric", "numeric",
      function(from){
        out <- numeric(from@length)
        out[from@pos] <- from@value
        out
      })


## Show
#' Show a sparse_numeric object
#' @param object A sparse_numeric object
#' @export
setMethod("show", "sparse_numeric",
          function(object){
            cat("sparse_numeric vector\n")
            cat("Length:", object@length, "\n")
            cat("Positions:", object@pos, "\n")
            cat("Values:", object@value, "\n")
          })


## Plot
#' Plot two sparse_numeric vectors for comparison
#' @param x A sparse_numeric vector
#' @param y A sparse_numeric vector
#' @param ... Additional arguments passed to plot()
#' @export
setMethod("plot", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...){
            if(x@length != y@length)
              stop("Vectors must have the same length")


            allpos <- sort(unique(c(x@pos, y@pos)))

            # handle empty case
            if(length(allpos) == 0){
              return(invisible(NULL))
            }

            vx <- numeric(length(allpos))
            vy <- numeric(length(allpos))

            vx[match(x@pos, allpos)] <- x@value
            vy[match(y@pos, allpos)] <- y@value

            plot(allpos, vx, type = "p", col = "lightblue",
                 xlab = "Position", ylab = "Value", main = "Sparse Vector Comparison", ...)
            points(allpos, vy, col = "darkgreen")

            legend("topright", legend = c("x", "y"),
                   col = c("lightblue", "darkgreen"))
          })


## Additional method
#' Summary for sparse_numeric
#' @param object A sparse_numeric vector
#' @return Prints summary statistics to console
#' @export
setMethod("summary", "sparse_numeric",
          function(object) {
            cat("Summary for sparse_numeric\n")
            cat("Length:", object@length, "\n")
            cat("Nonzero count:", length(object@value), "\n")
            cat("Min value:", min(object@value), "\n")
            cat("Max value:", max(object@value), "\n")
            cat("Mean (nonzero):", mean(object@value), "\n")
          })

