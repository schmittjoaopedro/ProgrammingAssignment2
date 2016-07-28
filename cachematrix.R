## This functions are used to create a matrix that can cache
## the calculations of the inverse matrix.
## This program calculate the inverse matrix and define if these
## can be retrieved from cache or not.

## Create an matrix object that contains the original matrix and
## the inverse cached matrix

makeCacheMatrix <- function(x = matrix()) {
    if(class(x) == "matrix") {
        i <- NULL
        set <- function(y) {
            x <<- y
            i <<- NULL
        }
        get <- function() return(x)
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
    } else {
        message("X not is a matrix")
    }
}


## Calculate the inverse matrix or retrieve from cache

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
            message("getting from cache")
            return(i)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
