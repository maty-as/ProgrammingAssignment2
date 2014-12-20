## This file contains functions for caching inverse of a matrix. There are two 
## functions. First called makeCacheMatrix is for creating structure that will
## contain a matrix, its inverse (once it is called), and set and get functions.
## Second file is a wrapper for the first function that provides inverse of 
## given matrix.
## Author: matyas.theuer@gmail.com
## Last edit: 20-12-2014 by MT

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL     # initialize null inverse
      
      # this function will set inner variable x to a given matrix
      set <- function(y) {
            x <<- y # passing argument to inner variable x
            inv <<- NULL # initialize null inverse
      }
      
      # this function returns inner x
      get <- function() x
      
      # this function sets inverse to a given matrix
      setinverse <- function(inverse) inv <<- inverse
      
      # this function returns inverse of matrix or null if is not yet computed
      getinverse <- function() inv
      
      # returns list of functions
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()   # geting inverse for given matrix x
      if(!is.null(inv)) {    # checking if it is already computed 
            message("getting cached inverse")   # note that it is just loaded
            return(inv) # returns cached value
      }
      # if it is not computed
      data <- x$get() # get stored matrix
      inv <- solve(data, ...) # computes inverse of the matrix
      x$setinverse(inv) # sets inverse to a cache
      inv # returns inverse
}

## That's all =)
