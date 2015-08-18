
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
    
    # set function
    set <- function(y) {
        x <<- y
        m <<- NULL # on set, must clear cache
    }
    
    # get function
    get <- function() x
    
    # set inverse function
    setinv <- function(inverse) inv <<- inverse
    
    # get inverse function
    getinv <- function() inv
    
    # return vector with 4 functions
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
    inv <- x$getinv() # get cached inverse matrix
    
    # if cached, return from cache
    if (!is.null(inv)) { 
        message("getting cached data")
        return(inv)
    }
    
    # if not cached
    data <- x$get() # get data
    inv <- solve(data) # calculate inverse
    x$setinv(inv) # set inverse to cache
    inv # return inverse
}
