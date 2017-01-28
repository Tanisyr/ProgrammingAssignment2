## Mike Wheelock
## R Programming
## Programming Assignment 2 (Week 3)

## These functions, makeCacheMatrix and cacheSolve, taken together, accomplish
## the task of persisting a matrix and caching its inverse along with it
## so that the inverse only needs to be calculated once

## The makeCacheMatrix function returns an object meant to store a matrix
## and it's inverse. The list object returned contains four methods:
##
## get - gets the stored value of the matrix
## set - sets the value of the matrix (will also reset the inverse to NULL)
## getInverse - gets the stored value of the inverse matrix
## setInverse - sets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## This function takes a cached matrix object from the makeCacheMatrix function
## and calculates its inverse. This inverse is persisted internally in the
## cached matrix object

cacheSolve <- function(x) {
    inv <- x$getInverse()
    if(is.null(inv)) {
        message("calculating inverse")
        inv <- solve(x$get())
        x$setInverse(inv)
    }
    inv
}
