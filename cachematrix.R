## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


## My test example
##
## > testm <- matrix(5:8, nrow = 2, ncol = 2)
## > testm
## [,1] [,2]
## [1,]    5    7
## [2,]    6    8
## > inv = makeCacheMatrix(testm)
## > inv$get()
## [,1] [,2]
## [1,]    5    7
## [2,]    6    8
## > inv$getinverse()
## NULL
## > cacheSolve(inv)
## [,1] [,2]
## [1,]   -4  3.5
## [2,]    3 -2.5
## > cacheSolve(inv)
## getting cached data
## [,1] [,2]
## [1,]   -4  3.5
## [2,]    3 -2.5
## > inv$getinverse()
## [,1] [,2]
## [1,]   -4  3.5
## [2,]    3 -2.5
## > inv$get()
## [,1] [,2]
## [1,]    5    7
## [2,]    6    8