## Functions below are used for calculating matrix inversion with caching.

## Provides access to a source and an inverted matrices through functions 'set', 'get', 'getInverse', 'setInverse'.
## If the 'set' function gets identical to 'x' matrix it don't change 'm' variable.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        if (!identical(x,y))
            m <<- NULL
       
        x <<- y
    }
    get <- function() x
    setInverse <- function(solvedMatrix) m <<- solvedMatrix
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Inverts matrix using cached data if possible.
## If it's impossible to use cached inverted matrix, 
## then the function calculates matrix inverstion first, stores the result.
## Every time when user gets cached data the function prints a message "getting cached data".

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(is.null(m)) {
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
    }
    else {
        message("getting cached data")
    }
    
    m
}
