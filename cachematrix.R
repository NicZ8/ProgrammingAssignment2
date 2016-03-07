## Assignment 2: Caching the inverse of a matrix


# Function to create a special matrix object that can cache its inverse, 
# i.e. a list containing four functions to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv_m <- NULL
        set <- function(y) {
                x <<- y
                inv_m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv_m <<- solve
        getinverse <- function() inv_m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# Function to compute the inverse of the special matrix.
# If the inverse has already been calculated (and the matrix has not changed), 
# then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv_m <- x$getinverse()
        if(!is.null(inv_m)) {
                message("getting cached data")
                return(inv_m)
        }
        mdata <- x$get()         
        inv_m <- solve(mdata, ...)
        x$setinverse(inv_m)
        inv_m
}