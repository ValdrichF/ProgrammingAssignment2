## The first function creates an object in its environment to which the inverse
# can be cached and other functions to get the original obeject and its previous
# The second function first checks if the inverse is present in the cache. 
# If not, it calculates the inverse and saves it to the enclosing environment.
# not sure how it would handle edits to the original matrix

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
