#' This is an implementation of a caching matrix.  It demonstrates
#' how one might create cacheable objects, in this case, matrix inverses, that
#' can be quickly retrieved without recomputation if the inverse has been
#' computed previously.
#' 
#' Note the comment syntax is an attempt to follow R documentation standards.
#' see http://r-pkgs.had.co.nz/man.html


#' Create and return caching "matrix".  The exported methods are:
#'   get -- retrieve the actual matrix
#'   set -- set the acutal matrix.  If different from the currently held
#'          matrix, invalidates the cached inverse, if any
#'   get.inverse -- retrieves the cached inverse matrix.  Pure accessor, does
#'                  no computation
#'   set.inverse -- sets the cached inverse matrix.  Pure mutator, does no
#'                  computation, and does not check if the given inverse is
#'                  in fact the inverse of the current matrix
#'
#' @param mat the actual matrix to use; if not given it is the empty matrix
#' @return a list of named values which are the exported "methods" of this
#'         "object"
makeCacheMatrix <- function(mat = matrix()) {
  inverse <- NULL
  
  #' Retrieve the current matrix value.  This will be the original matrix
  #' passes to the "constructor" makeCacheMatrix, unless it has been subsequently
  #' altered by a call to the set() "method" on the returned cache matrix
  #' object.
  #' 
  #' @return the current matrix value
  #' 
  get <- function() {
    mat
  }
  
  #' Change the matrix that's in use.  Invalidates the cached inverse
  #' so the next request for the inverse (if any) will cause it to be
  #' recomputed, if the new matrix is not the same as the existing matrix.
  #
  #' @param new.matrix the new matrix to use as the value of the cache matrix
  #' object.
  #' 
  set <- function(new.matrix) {
    # Be smart and don't invalidate the cache unless we have to
    if (!identical(mat, new.matrix)) {
      mat <<- new.matrix
      inverse <<- NULL
    }
  }
  
  #' Retrieves the cached inverse of the matrix.  This can be null
  #' if there is no currently captured inverse.  This is purely an accessor
  #' function; it does no computation on its own.
  #' 
  #' @return the inverse of the matrix, if it has been stored with set.inverse()
  #'         and the matrix has not been changed via set(), otherwise null.
  #'         
  get.inverse <- function () {
    inverse
  }
  
  #' Updates the cached inverse of the matrix.  Does no computation on its
  #' own, this is merely a state mutator.  It does not verify that
  #' the given inverse is in fact the inverse of the current matrix object.
  #' The new inverse matrix can be null, which will remove the cached version
  #' of the index.
  #
  #' @param new.inverse the new inverse matrix to cache
  set.inverse <- function(new.inverse) {
    inverse <<- new.inverse
  }
  
  # Our "methods" are made available as named members of the list
  # To think about: is there any way to have the list yield our internal
  # delegate matrix, or is there a way to define accessor and mutator 
  # methods more directly, like "mat()" (accessor) and "mat()=" (mutator).
  list(get = get, set = set, 
       get.inverse = get.inverse, set.inverse = set.inverse)
}


#' Returns the inverse of a matrix, utilizing a previously computed
#' cached value if it can.  Note that changing the matrix with the $set
#' "method" on the "matrix" object will invalidate the cached inverse and
#' result in the inverse being computed for the new version of the matrix.
#' 
#' Parameters:
#' @param cache.matrix an augmented matrix with caching capabilities, as created
#'                     by the makeCacheMatrix function. 
#' @param ... additional parameters to the matrix inversion ("solve") function
cacheSolve <- function(cache.matrix, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- cache.matrix$get.inverse()
  if (!is.null(inverse)) {
    message("cache hit; retrieving cached inverse")
    return(inverse)
  }
  
  # cache miss, have to replace the cached value
  inverse <- solve(cache.matrix$get(), ...)
  cache.matrix$set.inverse(inverse)
  inverse
}

