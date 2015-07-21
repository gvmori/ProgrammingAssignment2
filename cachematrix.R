## Matrix Inversion Caching
## a pair of functions to facilitate efficient re-determination of a matrix's
## inverse, by caching the inverse the first time it is calculated.

## makeCacheMatrix
## generate a simple object allowing a matrix's inverse to be cached 
## usage:
##   cm <- makeCacheMatrix(data)
##   data <- cm$get()
makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL

    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }

    get <- function() {
        x
    }

    setInverse <- function(inverse) {
        inverseMatrix <<- inverse
    }

    getInverse <- function(y) {
        inverseMatrix
    }

    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve
## returns the inverse of a CacheMatrix object and caches the inverse in the object
## usage: 
##   cm <- makeCacheMatrix(data)
##   inverse <- cacheSolve(cm) 
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse(x)

    ## getInverse will return null if the matrix has changed,
    ## recalculate the inversion if so
    if (is.null(inverse)) {
        inverse <- solve(x$get())
        x$setInverse(inverse)
    }

    inverse
}
