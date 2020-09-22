## Functions to cache a matrix and calcultate its inverse
## 

## Caches a matrix

makeCacheMatrix <- function(x = matrix()) {
    # Inverse itinialization
    i <- NULL
    
    # Sets the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # Gets the matrix
    get <- function() x
    
    # Sets the inverse
    setinverse <- function(inverse) i <<- inverse

    # Gets the inverse
    getinverse <- function() i
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns the inverse of a cached matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # Get inverse
    i <- x$getinverse()
    
    # If set, return inverse
    if(!is.null(i)) {
        message("Getting chached data")
        return(i)
    }
    
    # Get matrix
    data <- x$get()
    
    # Solve for inverse
    i <- solve(data)
    
    # Set inverse
    x$setinverse(i)
    
    # Returns inverse matrix
    i
}
