# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather 
# than compute it repeatedly. The following two functions are used to cache the inverse of a matrix.

# The first function, makeCacheMatrix creates a list containing a function to
# a) set the value of the matrix
# b) get the value of the matrix
# c) set the value of inverse of the matrix
# d) get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverseerse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function calculates the inverse of the matrix. However, it first checks if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. Otherwise,it computes the inverse and sets the value in the cache 
# via setinverse function. 

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

