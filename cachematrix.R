## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.
## The first function, mackCacheMatrix creates a special "matrix", wich is a list containing a function to
##           1.set the value of the matrix
##           2.get the value of the matrix
##           3.set the value of the inverse of matrix
##           4.set the value of the inverse of matrix
## The second function first check to see if the inverse has been calculated already. If so, it gets the inverse from
## the cache and skips the computation of inverse. Otherwise, it calcualtes the inverse of the matrix and sets the 
## inverse in the cache via the setinv function.

## This function is to create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function is to compute the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been clculated, the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
