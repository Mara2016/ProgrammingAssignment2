## Put comments here that give an overall description of what your
## functions do

## trasform a matrix in a way such that cacheSolve can work on it

makeCacheMatrix <- function(x = matrix()) {
        I<-NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) I <<- solve
        getsolve <- function() I
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Given the "formatted" matrix, look for its inverse. 
## If alreafy existing it, return it, otherwise compute it and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I<-x$getsolve()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data<-x$get()
        I<-solve(data,...)
        x$setsolve(I)
        I
}
