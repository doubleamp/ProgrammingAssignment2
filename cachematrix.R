## Solution by A.M.P. to Programming Assignment 2

## Creates the 'special list' vector for the matrix, following the same pattern that makeVector

makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   set <- function(y) {     # set function to set values
      x <<- y
      i <<- NULL
   }
   get <- function() x      # get function to obtain existing values
   setInverse <- function(solve) i <<- solve   # setInverse function to set inverse matrix
   getInverse <- function() i                  # getInverse function to retrieve already calculated inverse matrix
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)    # lists all defined functions
}


## Calculates the inverse of a matrix if it is not already cached; otherwise it returns the cached result; it follows the same approach that cachemean

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   i <- x$getInverse()   # check to see if the inverse is already calculated
   if(!is.null(i)) {     # if so, warn that the inverse has been obtained from cached data and return it
      message("getting cached data")
      return(i)
   }
   data <- x$get()       # if no inverse existed, calculate it and return it
   i <- solve(data, ...)
   x$setInverse(i)
   i
}
