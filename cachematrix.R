## makeCacheMatrix() creates a special "matrix" object that can cache its inverse
## The function first sets the value of the matrix. 
## It then gets the value of the matrix.
## It sets the inverse of the matrix.
## It gets the value of the inverse.

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


## cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix.
# If the inverse has been calculated then the function retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
