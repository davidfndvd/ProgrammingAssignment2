## These are  functions that cache and compute the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(x) {
    mtx <<- x;
    inverse <<- NULL;
  }
  get <- function() return(mtx);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## This function computes the inverse of the matrix returned by `makeCacheMatrix` 
## above. If the inverse has already been calculated (and the matrix has not changed),
##  then `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- mtx$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- mtx$get()
  invserse <- solve(data, ...)
  mtx$setinv(inverse)
  return(inverse)
}
