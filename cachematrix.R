## These functions will create a matrix and give its inverse.

## This function will create a matrix and cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL # sets inverse value, if present in parent environment
        set <- function(y) { #caches value of x argument in global environment and initializes inverse to NULL
                x <<- y
                i <<- NULL
        }
        get <- function() x # retrives cached value of matrix
        setinv <- function(inv) i <<- inv #inverse cached to i in parent environment
        getinv <- function() i 
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)

}


## This function gives the inverse of the cached matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i

}
