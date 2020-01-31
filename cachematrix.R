## The two functions below are meant to be able to create a matrix
## with a cachable value and also calculate the inverse of the matrix
## if it isn't cached.  If the inverse of the matrix has been cached
## then the value in the cache will be retrieved.
## 

## This function turns a matrix into an object that with a the value
## of the matrix, the value of its matrix (or NULL) and methods
## to get and set the matrix values and get and set the inverse
## of the matrix values

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse 
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function takes a cachMatrix object as its argument
## and either retrieves the inverse from the cache if the inverse
## value is non-NULL, otherwise it returns the calculated inverse
## value using the solve() function.


cacheSolve <- function(x, ...) {
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
