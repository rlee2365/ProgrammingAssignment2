# Returns a closure that allows the user to store a matrix and cache the 
# result of inverting it.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL # Reset the matrix inverse after setting it
        }
        get <- function() x
        setinv <- function(invers) inv <<- invers
        getinv <- function() inv
        list(set = set, get = get,
                setinv = setinv, getinv=getinv)
}


# Matrix inversion function that caches its result for future calls
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if (!is.null(inv)) {
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
