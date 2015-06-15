## Put comments here that give an overall description of what your
## functions do

## This function will return a special matrix object 
## that can catch and store it's inverse. It also
## has get and set methods for it's inverse.


makeCacheMatrix <- function(x = matrix()) {
  
           inv <- NULL
           
           # if this set function is used to set a new matrix,
           # inverse is set to null
           
           set <- function(y){
                x   <<- y
                inv <<- NULL
                   
           }
           get    <- function() x
           
           setinv <- function(inverse) inv <<- inverse
           
           getinv <- function() inv
           
           list(set    = set,
                get    = get,
                setinv = setinv,
                getinv = getinv)
  
}


## This function calculates the inverse of the special
## matrix created by the makeCacheMatrix function. 
## If a cached inverse value exists, and the matrix  
## hasn't changed, it uses that value, otherwise 
## it calculates and stores a new inverse value.

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
  
        inv <- x$getinv()
        
        if(!is.null(inv)){
          
          message("getting the chached inverse")
          
          return(inv)
        }
        
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        
        inv
 
}
