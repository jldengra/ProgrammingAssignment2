## Caching the Inverse of a Matrix

## The scoping rules of the R language make possible to preserve the state
## inside of an R object, and one of its applications is to provide a cache
## to store the inverse of a matrix such that it is only computed the first 
## time. We assume that the matrix supplied is always invertible.

## This function makeCacheMatrix creates a special "matrix" object that can 
## cache its inverse. This special object is a list of functions to set and 
## get the value of the matrix and its inverse. To be able to preserve, the
## set functions make their assignments in an environment that is different 
## from the current environment by making use of the <<- operator. 

makeCacheMatrix <- function(x = matrix()) {
        ## The inverse (i) of the matrix is initialized empty
        i <- NULL
        
        ##  1. Sets the value of the matrix 
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ##  2. Gets the value of the matrix
        get <- function() x
        
        ##  3. Sets the value of the inverse
        setinverse <- function(solve) i <<- solve
        
        ##  4. Gets the value of the inverse
        getinverse <- function() i
        
        ## A list containing the set and get functions is returned
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function returns the inverse of the special "matrix" created
## with the previous function. It checks if the inverse has already been computed.
## If so, it takes the inverse from the cache and avoids the calculation. 
## Otherwise, it calculates the inverse matrix and stores the value in the cache
## by using the setinverse function.

cacheSolve <- function(x, ...) {
        ## Returns a matrix i that is the inverse of 'x'
        
        ## Checks the inverse from the cache
        i <- x$getinverse()   
        
        ## Computes the inverse only when not in cache
        if(!is.null(i)) {                
                message("getting cached data")                
        }
        else {
                data <- x$get()
                i <- solve(data, ...)
                x$setinverse(i) 
        }      
        
        ## The inverse matrix is returned
        i
}


######################################################################
## Sample of usage: 

## > m <- makeCacheMatrix(matrix(1:4, 2, 2))
## > inverse <- cacheSolve(m)
## > inverse
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > inverse2 <- cacheSolve(m)
## getting cached data
## > inverse2
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
