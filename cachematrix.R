## These function calculate and cache the inverse of a matrix
## saving the need to do (costly) recalculations in the future.

## This function creates a special "matrix" object that can 
##cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

            invrs <- NULL  #placeholder inverse variable set to NULL.
            set <- function(y) {
                  x <<- y
                  invrs <<- NULL
            }
            get <- function() x
            setinvrs <- function(inverse) invrs <<- inverse
            getinvrs <- function() invrs
            list(set = set, get = get,
                 setinvrs = setinvrs,
                 getinvrs = getinvrs)     
      
}

## The function finds the inverse of a mtrix created by the 
## makeCacheMatrix function. If it already exists, the cache matrix
## is called. If it does not exist, a new inverse matrix is created.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

            invrs <- x$getinvrs()
            if(!is.null(invrs)) {
                  message("getting cached data")
                  return(invrs)
            }
            data <- x$get()
            invrs <- solve(data, ...)
            x$setinvrs(invrs)
            invrs
}
