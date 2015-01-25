## The purpose of the following functions is to cache the matrix and compute its inverse

## This function is to create a special 'matrix' which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set=set, get=get, 
         setsolve=setsolve, getsolve=getsolve)
}


## This function is to compute the inverse of the matrix returned by makeCacheMatrix function
## The first line is to retrieve the matrix returned in makeCacheMatrix in case 'm' is assigned
## to another object
## The 'if' function checks whether the inverse has already been calculated. If so, it gets the 
## inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data
## and set the inverse matrix in the cache via the setsolve function

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
    ## Return a matrix that is the inverse of 'x'
}
