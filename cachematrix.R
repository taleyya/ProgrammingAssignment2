makeCacheMatrix <- function(x = matrix()) {
    #initialize the inverse matrix cache
    m <- NULL
    #set the matrix value and reset m
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m 
    #return the list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
        }
    
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    #check if inverse matrix is cached
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix_data <- x$get()
    m <- solve(matrix_data,...)
    x$setinverse(m)
    m
}
