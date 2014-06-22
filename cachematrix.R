## makeCacheMatrix creates a list containing a function to set and get a matrix and to set and get its inverse, using 
## the R function solve.

## pfml, 20140622, v1.4

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <-function(solve) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve returns the inverse of the matrix created by the above function. it first sees if this inverse
## has been already computed; in this case it returns the cache value.
## Otherwise, assuming the matrix is a sqaure inversible matrix, it uses the function solve() to calculate its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
