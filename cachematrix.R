## These functions create and then solve (invert) a "matrix-object", that can
## cache a matrix and its inverse but can also be altered

## This creates a "matrix object" that caches a matrix and allows its inverse
## to be cached, but also allows both to be overwritten

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y){
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) i <<- solve
      getinverse <- function() i
      list(set = set, get = get, 
           setinverse = setinverse,
           getinverse = getinverse)
}


## This takes a "matrix object" created by the above function, checks to see
## if an inverse has been stored; if not, it calculates and sets the inverse

cacheSolve <- function(x, ...) {
        i <-x$getinverse()
        if(!is.null(i)){
              message("getting cached data")
              return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
