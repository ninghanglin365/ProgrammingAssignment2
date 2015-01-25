## write a pair of functions that cache the inverse of a matrix.

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the reverse
## 4. get the value of the reverse

makeCacheMatrix <- function(x = matrix()) {
        rev <- NULL
        set <- function(y) {
                x <<- y
                rev <<- NULL
        }
        get <- function() x
        setrev <- function(solve) rev <<- solve
        getrev <- function() rev
        list (set = set, get = get, setrev = setrev, getrev = getrev)

}


## 1. test the existance of rev
## 2. get the rev, and skips the computation
## 3. calculate the rev and set the rev in the cache

cacheSolve <- function(x, ...) {
        rev <- x$getrev()
        if(!is.null(rev)){
                message("getting cached data")
                return(rev)
        }
        matrix <- x$get()
        rev <- solve(matrix, ...)
        x$setrev(rev)
        rev
        
}


amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
amatrix$get()         # Returns original matrix
cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
amatrix$getrev()  # Returns matrix inverse
cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse
amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
amatrix$get()         # Returns matrix
amatrix$getrev()  # Returns matrix inverse