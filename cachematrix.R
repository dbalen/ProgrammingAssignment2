# Below are two functions that are used to create a
# special object that stores a matrix and caches its inverse matrix.
# Second function is used to retrive inverse matrix from the special object
# or calculate inverse matrix (and cache it) if it is called for the first time.


# The following function, `makeCacheMatrix` creates a special "matrix",
# which is really a list containing a function to
# 
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse matrix
# 4.  get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


# The following function 'cacheSolve' calculates the inverse of the special
# "matrix" created with the above function. However, it first checks to see if
# the inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets the value of the inverse in the cache via the `setinv`
# function.
cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
