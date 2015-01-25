## write a pair of functions that cache the inverse of a matrix.

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the reverse
## 4. get the value of the reverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list (set = set, get = get, setinv = setinv, getinv = getinv)

}


## 1. test the existance of inv
## 2. get the inv, and skips the computation
## 3. calculate the inv and set the inv in the cache

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinv(inv)
        inv
        
}

