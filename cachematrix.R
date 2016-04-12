## These functions allow for computing the inverse of a square matrix
## and caching the results. Further calls to calculate the inverse will
## use the cached results instead of of re-calcuating

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)  
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ##attempt to get the cached value
  m <- x$getsolve()
  
  ## return the cache is it is already caclulated
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## get the matrix
  data <- x$get()
  
  ## calcuate the inverse
  m <- solve(data, ...)
  
  ## store the inverse in the cache
  x$setsolve(m)
  
  ## return the inverse values
  m
}
