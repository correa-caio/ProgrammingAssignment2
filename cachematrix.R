## Put comments here that give an overall description of what your
##The function makeCacheMatrix returns antoher function with four inner functions
##These inner functions allow to "set" and "get" the values for a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setInverse <- function(inverse) {inv <<- inverse}
     getInverse <- function() {inv}
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##cacheSolve checks if there is a result already cached for the inverted matrix
##If there is no cached result, it calculates the matrix inversion

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inv <- x$getInverse()
     if (!is.null(inv)){
          message("obtaining cached data")
          return(inv)
     }
     mt <- x$get()
     inv <- solve(mt, ...)
     x$setInverse(inv)
     inv
}