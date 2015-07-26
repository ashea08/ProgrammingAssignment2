## The functions below take a matrix as input and prep it to be solved
## for its inverse and cached for future use. 

## The makeCacheMatrix takes a matrix as input and preps it to be used in the
## cacheSolve function. The function sets the matrix and creates a getinverse
## function that is used in cacheSolve to access the matrix. The function
## returns a list.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {      ## matrix passed into function is set here.
        x <<- y
        m <<- NULL
    }
    get <- function() x       ## matrix is assigned to get function here.     
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## cacheSolve takes the matrix assigned to "m" in makeCacheMatrix and solves
## for its inverse or if the inverse already exists, it returns the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()         ## Accessing just the getinverse subset of x 
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()             
    m <- solve(data, ...)
    x$setinverse(m)             ## Assigns inverse to "setinverse" subset
    m                           ## Returns inverse
                        
}
