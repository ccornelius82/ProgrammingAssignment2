## Since calculating the inverse of a matrix is time-consuming, the makeCacheMatrix and cacheSolve functions
## can cache this inverse matrix. 

## The makeCacheMatrix function will create an object with four functions: set, get, setInv, getInv.
## The object will also have two data objects, the matrix x and the null value m.

makeCacheMatrix <- function(x = matrix()) {
        m <- null
        # The function 'set' calls x as argument and resets m to null if it's been cached before
        set <- function(y) {
                x <<- y
                m <<- NULL
                }
        # The function 'get' retrieves the matrix.
        get <- function() x
        # The function 'setInv' will fix the inverse matrix.
        setInv <- function(inverse) m <<- inverse
        # The function 'getInv' will retrieve the inverse matrix.
        getInv <- function() m
        # Now the list of all functions in makeCacheMatrix is defined.
        list(set = set, get = get, setInv = setInv, getInv = getInv)
        }

## cacheSolve will first look to see if there's an inverse matrix already cached, and if there is,
## it will return it. If the inverse matrix hasn't been cached, cacheSolve will calculate it, cache it,
## and return it.

cacheSolve <- function(x, ...) {
        ## First, check if the inverse matrix is cached. If it is, find and return that value, and exit
        ## the function.
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data...")
                return(m)
                }
        ## If the inverse matrix is not cached, the cacheSolve function continues here.
        ## It finds the matrix, solves it, caches the inverse, and returns the inverse.
        data <- x$get()
        m <- solve(data)
        x$setInv(m)
        m
        }
