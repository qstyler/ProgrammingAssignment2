## Functions for manipulating the cacheable matrices

## Creates a cacheable matrix.
## List of the functions
## which operate with the variables within its own scope
## set(y)
## get()
## set.inverted(inverted)
## get.inverted()

makeCacheMatrix <- function(matrix.original = matrix()) {
    # since we don't have type control in R
    # we have to check whether the matrix was passed
    if (!is.matrix(matrix.original)) {
        stop("makeCacheMatrix expects argument to be a matrix")
    }

    matrix.inverted <- NULL

    set <- function(y) {
        matrix.original <<- y
        matrix.inverted <<- NULL
    }
    get <- function(){
        matrix.original
    }

    get.inverted <- function() {
        matrix.inverted
    }
    set.inverted <- function(inverted) {
        matrix.inverted <<- inverted
    }

    list(
        set = set,
        get = get,
        set.inverted = set.inverted,
        get.inverted = get.inverted
    )
}


## checks if cacheable matrix contains a cached inverted matrix
## and returns

## I have a strong feeling that
## this function should be in a makeCacheMatrix
## and implement a singleton pattern
## But there are probably some specific things about R
## that don't allow this to happen

cacheSolve <- function(matrix.cacheable, ...) {
    matrix.inverted <- matrix.cacheable$get.inverted()
    #return matrix.inverted if it was computed and cached before
    if (!is.null(matrix.inverted)) {
        message("getting cached data")
        return(matrix.inverted)
    }
    # otherwise: get the original one
    matrix.original <- matrix.cacheable$get()
    # calculate the inversed
    matrix.inverted <- solve(matrix.original)
    # store it for the future use
    matrix.cacheable$set.inverted(matrix.inverted)
    # and return it
    matrix.inverted
}
