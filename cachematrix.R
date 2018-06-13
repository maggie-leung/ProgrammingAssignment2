## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    # `makeCacheMatrix` creates a matrix, which is
    #  caches a list containing a function to
    # 1.  set the value of the vector
    # 2.  get the value of the vector
    # 3.  set the value of the inverse
    # 4.  get the value of the inverse
    
        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # inverse of the special "matrix" returned by `makeCacheMatrix` 
        # Print message if the inverse has been created, and get the inverse
    
        inv <- x$getinverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
