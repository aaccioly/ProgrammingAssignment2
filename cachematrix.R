## This file implements the second assignment of the Coursera / Johns Hopkins R
## Programming course. It provides a function to wrap a matrix with a list data
## structure able to cache its inverse. It also provides a convenience function 
## to compute and quickly retrieve cached results from the cache matrix.

## Builds a cacheable matrix out of a invertible numeric matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Convenience method to compute and store the inverse of a matrix into a cache
## matrix (see the makeCacheMatrix function above). It will only compute the 
## inverse matrix if x$i is NULL (which means that x is either a new cache 
## matrix or one that has been recently mutated with x$set / x$setinverse), else 
## it will return the cached value avoiding unnecessary recomputations.

cacheSolve <- function(x) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}

