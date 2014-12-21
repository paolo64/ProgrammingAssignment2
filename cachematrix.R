## The two functions below show how to cache and caclulate the inverse of a matrix.
## functions do

### This function returns a special matrix that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() return(x)
  setsolve <- function(inv) i <<- inv
  getsolve <- function() return(i)
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


### This function return the inverse of "matrix object" created via "makeCacheMatrix". 
### If that value is already in cache, it will be returned; otherwise the inverse of matrix is calculated and stored into the cache.  

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getsolve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(i)
  return(i)
}
