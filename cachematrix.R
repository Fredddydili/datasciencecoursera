## 2 functions to cache the inverse of a matrix

## To create a special "matrix" object

makeCacheMatrix <- function(matrix_input = matrix()) {
    if (!is.matrix(matrix_input)){
      "Please input a matrix"
    }
    
    matrix_inverted <- NULL
    
    set <- function(y) {
      matrix_input <<- y
      matrix_inverted <<- NULL
    }
    get <- function() matrix_input
    set_invert <- function(solve) matrix_inverted <<- solve
    get_invert <- function() matrix_inverted
    list(set=set, get=get,set_invert=set_invert,get_invert=get_invert)
}


## To compute the inverse of the special " matrix" returned by function 'makeCacheMatrix'
## If the inverse has been calculated then directly retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrix_inverted <-x$get_invert()
  if(!is.null(matrix_inverted)) {
    message("getting cached matrix")
    return(matrix_inverted)
  }
  matrix_input <- x$get()
  matrix_inverted <-solve(matrix_input,...)
  x$set_invert(matrix_inverted)
  matrix_inverted
}
