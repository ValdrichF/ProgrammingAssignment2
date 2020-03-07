## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    set = function(y){
        x <<- y
        inv <<- NULL
    }
    get = function() x
    setinv = function(solve) inv <<- solve
    getinv = function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Function to compute the inverse of the special matrix returned above
## And checks if the inverse is already calculated

cacheSolve <- function(x, ...) {
    inv = x$getinv()
    if (!is.null(inv)){
        message('getting cached data')
        return(inv)
    }
    data = x$get()
    inv  = solve(data,...)
    x$setinv(inv)
    inv
}
