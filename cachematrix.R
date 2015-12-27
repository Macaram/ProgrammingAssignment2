# The following functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing functions to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
# whereas inv <- NULL is going to store the cashed inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The following function returns the inverse of the matrix. It gets the result and skips the consequent 
# computation if the inverse has already been computed, otherwise it computes the inverse and sets the value 
# in the cache by using setinverse function. x$setinverse(inv) cases the inverse. 

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

# A sample to run:
# s = rbind(c(1, -1/3), c(-1/3, 1))
# m = makeCacheMatrix(s)
# m$get()

# There is no cache in the first attempt. Start.time estimates the time of execution.

# start.time <- Sys.time()
# cacheSolve(m)
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken

# Retrieving from the cache in the second run.

# start.time <- Sys.time()
# cacheSolve(m)
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken