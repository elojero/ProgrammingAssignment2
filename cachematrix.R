## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

    inverse=NULL
    
    set<- function(y){
        x <<- y
        inverse <<- NULL
    }
    
    get<- function() x
    
    setinv<- function(inv) inverse <<- inv
    
    getinv <- function() inverse
    
    list(set = set, get = get,
         setinv = setinv,
         getinv= getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    
    ## Call the function that get the inverse matrix, if it has already calculated.
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## Solve  and print the inverse matrix If it has not been calculated yet
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinv(inverse)
    inverse

}
