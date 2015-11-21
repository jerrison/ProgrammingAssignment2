## The programming assignment consists of two functions.  The first one,
## "makeCacheMatrix" will be able to output a list containing a matrix and 
## possibly the matrix's inverse.
## The second function, "cacheSolve" will take the list from the 
## "makeCacheMatrix", check whether the inverse of the matrix is already solved
## and that the matrix didn't change.  If both conditions are satisfied, the 
## cached inverse is used.  Otherwise, the inverse is calculated.


## Function "makeCacheMatrix" output is a list containing functions for a 
## matrix:
## 1. Set value of matrix
## 2. Get value of matrix
## 3. Set inverse of matrix
## 4. Retrieve inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x                             # returns matrix
    setinverse <- function(inverse) inv <- inverse  # cache happens here
    getinverse <- function() inv                    # returns inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function "cacheSolve" checks that a matrix has not changed and checks whether
## an inverse has already being calculated.  If either of those conditions is 
## not satisfied.  A new inverse matrix is calculated.

cacheSolve <- function(x, ...) {
    
    inv <- x$getinverse
    data <- x$get
    
    #check inverse is already solved and whether matrix changed
    if(!is.null(inv) & data == y) {
        message("using cached inverse")
        return(data)
    } # it is equal and there is cached data so use cached inverse matrix
    else {
        data <<- y
        inv <- solve(data, ...)   
        x$setinverse(inv)
        inv
    } # not equal or no cached data, so get new matrix and calculate inverse
}
