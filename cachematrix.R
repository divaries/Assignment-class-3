## Coursera Programming Assignment - Class 3

## Function 'makCacheMatrix' makes a matrix that can cache it's inverse. 
## It uses the inbuilt functions set, get, setmean, and getmean to achieve this.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## 'cacheSolve' function computes the inverse of the matrix (this is the input from cachemean) 
## given by the function created above 'makeCacheMatrix. 'cacheSolve' should return the inverse from the cache.

cacheSolve <- function(x, ...) {
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

