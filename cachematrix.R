## The two functions below allow to create a special object that stores a matrix 
## and cache's its inverse.

## This function creates a special 'matrix' object that can cache its inverse.
## This object is a list containing a function to set and get the value of the matrix,
## set and get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  invmx <- NULL
  set <- function(y) {
    x <<- y
    invmx <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invmx <<- inverse
  getinverse <- function() invmx
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of the special 'matrix' created with the makeCacheMatrix() 
## function. The inverse is computed with the solve() function if it have not been already 
## calculated.

cacheSolve <- function(x, ...) {
  invmx <- x$getinverse()
  if(!is.null(invmx)) {
    message("getting cached data")
    return(invmx)
  }
  data <- x$get()
  invmx <- solve(data, ...)
  x$setinverse(invmx)
  invmx
}
