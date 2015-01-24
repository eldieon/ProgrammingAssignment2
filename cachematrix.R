## makeCacheMatrix() initializes an empty inverse matrix i and,
## in a new enclosed function, sets a value for x and i with the <<- operator
## such that they can be looked up by cacheSolve(), not in the environment in which
## cacheSolve() was run, but rather up the list, in the environment created when makeCacheMatrix
## was run. cacheSolve uses x and i, by first looking for an existing value for i, and otherwise returning 
##the inverse using the solve() function.

## makeCacheMatrix sets an empty i (inverse) variable, and gets the value of the x argument, sets and then gets the value of inverse
## (i, here NULL until otherwise solved).

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL 
        set <- function(y) {
                x <<- y
                i <<- NULL 
        }
        get <- function() x
        setinv <- function(inverse) i <<- inverse 
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## cacheSolve solves for the inverse of the matrix x (from makeCacheMatrix) if one does not already exist.
## It does this by looking up i in the environment x (the environment where the makeCacheMatrix() function was created)

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

