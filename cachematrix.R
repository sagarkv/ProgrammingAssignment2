## This function creates a special "matrix" object that can cache its inverse.
## Note that for caching, cacheSolve method must first be called on the object
## before calling the getinv method

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inversemtx) inv <<- inversemtx
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function is a cached version of solve function (matrix inverse) 
## It works on special makeCacheMatrix functiona object 
## It caches the result and on subsequent calls will return result from cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

