## Put comments here that give an overall description of what your
## functions do

## Creates a special vector containing
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix
##
## params [x] matrix
## return special vector
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, 
         setinv = setinv,
         getinv = getinv)
}


## If the inverse of the matrix was already calculated
## return the cached data, otherwise calculate it
##
## params [x] special vector from function makeCacheMatrix
## return inverse of matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    message("calculating inv")
    x$setinv(inv)
    inv
}
