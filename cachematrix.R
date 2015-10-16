## This function uses a similar structure to the
## makeVector example. It inputs a matrix, then sets the 
## value of the matrix. Next, it gets the value of the matrix. 
## Finally, the function sets the value of the inverse of 
## this matrix (using the solve function), then gets the value of ## the inverse.



makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL   }
     get <- function() x
     setsolve <- function(solve) m <<- solve
     getsolve <- function() m
     list(set = set, get = get,
          setsolve = setsolve, getsolve = getsolve)
}


## This function uses a similar structure to the cachemean 
## function in the example. It first checks if the inverse of the 
## matrix has been computed. If so, the function grabs the 
## inverse from the cache. If not, the function computes the
## inverse of the matrix.

cacheSolve <- function(x,...) {
     m <- x$getsolve()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)  }
     data <- x$get()
     m <- solve(data,...)
     x$setsolve(m)
     m
}
