## This program will implement a functionality to return a inversed matrix with Cacha mechanism

## This function is a generic function to implement cache mechanism
makeCacheMatrix <- function(x = matrix()) {
        m < NULL
        set <- function( y=matrix()) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function() m <<- solve(x)
        getSolve <- function() m
        list(set = set, get = get,
             setSolve  = setSolve ,
             getSolve = getSolve)
}


## This function is a wrapper of solve function and cache mechanism
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}
