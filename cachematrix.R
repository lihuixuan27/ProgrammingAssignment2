## Put comments here that give an overall description of what your
## functions do

## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly. This pair of functions could cache the 
## inverse of a matrix.

## Write a short comment describing this function

## The makeCacheMatrix() could create a list which 
## contains the inverse of input matrix, and when
## you need the inverse, you could just call the 
## cacheSolve functions.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve 
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
## The cacheSovle function should run after the 
## makeCacheMatrix function, and the return of the 
## cacheSolve function is the cache inverse which 
## has been created in the makeCacheMatrix funcion.
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
