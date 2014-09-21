### These two functions that compute matrix's inverse
### and store results in cach, so that less resources are needed

## This funtion creates an object that holds a square, invertable
## matrix. It also creates a list of four functions:
#   get() - return the matrix
#   set() - assign a new set of values
#   getinverse() - return the inverse of the matrix
#   setinverse()- assign a new value to inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function validate whether the cached value already exist.
## Stored in "i"
## If exists - returns the cached value
## Else - performe with the solve() the inverse, cache the value,
## and return the answer.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}