
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        # Set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # Get the matrix
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Computes the inverse of the object created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        
		inv <- x$getInverse()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)  # Compute the inverse
        x$setInverse(inv)        # Cache the inverse
        inv
}
