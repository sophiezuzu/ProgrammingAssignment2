## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
# 1. set the value of the vector
		m <- NULL

# 2. get the value of the vector
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
		
# 3. set the value of the inverse
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
		
# 4. get the value of the inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

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
