## Put comments here that give an overall description of what your
## functions do


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
         mat1 <- NULL 
         set <- function(y){
                 x <<- y
                 mat1 <<- NULL
         }
         
         ##Get Matrix from the calling function
         get <- function() x
         
         ##Creating and seeting the inverse in cache
         setinverse <- function(solve) mat1 <<- solve
         
         ##Returing the cached inverse matrix 
         getinverse <- function() mat1
         
         list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
     
}


##This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
}
