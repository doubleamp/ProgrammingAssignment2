## Solution by A.M.P. to Programming Assignment 2

## Creates the 'special list' vector for the matrix, following the same pattern that makeVector

makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   set <- function(y) {
      x <<- y
      i <<- NULL
   }
   get <- function() x
   setInverse <- function(solve) i <<- solve
   getInverse <- function() i
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Calculates the inverse of a matrix if it is not already cached; otherwise it returns the cached result; it follows the same approach that cachemean

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   i <- x$getInverse()
   if(!is.null(i)) {
      message("getting cached data")
      return(i)
   }
   data <- x$get()
   i <- solve(data, ...)
   x$setInverse(i)
   i
}
