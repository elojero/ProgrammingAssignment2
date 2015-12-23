## Put comments here that give an overall description of what your
## functions do
    ## These two functions are used to create a special object that
    ## stores a numeric matrix and cache's its inverse.

## Write a short comment describing this function
    ##  makeCacheMatrix creates a special "matrix", which is really a list 
    ##  containing a function to:
    ##  1) set the value of the matrix
    ##  2) get the value of the matrix
    ##  3) set the inverse matrix 
    ##  4) get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

    ## "inverse" will contain the inverse matrix
    inverse=NULL
    
    ##"Set" function will store the "original" matrix wich inverse should be calculated 
    set<- function(y){
        x <<- y
        inverse <<- NULL
    }
    
    ##"Get" function will print the "original" matrix.
    get<- function() x
    
    ##"Setinv" function will store the inverse matrix in the "inverse" variable that will
    ## be the "cache"
    setinv<- function(inv) inverse <<- inv
    
    ##"getinv function prints the inverse matrix saved in the "inverse" variable (the "cache")
    getinv <- function() inverse
    
    list(set = set, get = get,
         setinv = setinv,
         getinv= getinv)
}


## Write a short comment describing this function
    ##  This function computes the inverse of the special "matrix" returned by 
    ##  makeCacheMatrix above. If the inverse has already been calculated
    ##  (and the matrix has not changed), then the cachesolve should retrieve
    ##  the inverse from the cache.

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
