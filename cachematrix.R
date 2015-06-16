## cachematrix contains two functions: makeCacheMatrix and cacheSolve
## 
## makeCacheMatrix creates a "special matrix" object (a list indeed) that 
## can cache its inverse.
## This "special matrix" is given with some tools that permit to interact with it
## $set is used to set (store) a new matrix to be inverted
## $get is used to get (show/return) the matrix to be inverted
## $setinverse is used to set (store) the inverse matrix
## $getinverse is used to get (show/return) the inverse matrix
##
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.
##
## How to use:
## b=matrix(c(1,3,2,4),2,2)
## a<-makeCacheMatrix() 
## a$set(b) stores b
## a$get() shows b
## cacheSolve(a) calculates inverse matrix and store it
## cacheSolve(a) now does not calculate the inverse but returns tha cached one


makeCacheMatrix <- function(x = matrix()) {
        ## inv_matrix is defined as NULL object
        inv_matrix <- NULL
        ## set is a function that takes y and define x as y in the parent enviroment
        ## (it permits to substitutes previously stored matrix with a new one)
        ## and redefine inv_matrix as NULL (to clean previously stored inv_matrix if any)
        set <- function(y) {
                x <<- y
                inv_matrix <<- NULL
        }
        ## get is a function that shows/returns the stored matrix
        get <- function() x
        ## setinverse is a function that  stores the inverse matrix
        ## in the parent enviroment
        setinverse <- function(inverse) inv_matrix <<- inverse
        ## getinverse is a function that shows/returns the stored inverse matrix
        getinverse <- function() inv_matrix
        ## the following lines create the list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cachesolve computes the inverse of the "special matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## get the inverse matrix (if any) from 
        inv_matrix <- x$getinverse()
        ## if the inverse matrix was previously stored (not NULL) 
        ## return stored inverse matrix
        if(!is.null(inv_matrix)) {
                message("getting cached data")
                return(inv_matrix)
        }
        ## in any other case get the stored matrix
        data <- x$get()
        ## make the inverse
        inv_matrix <- solve(data, ...)
        ## store the inverse
        x$setinverse(inv_matrix)
        ## return the inverse
        inv_matrix
}
