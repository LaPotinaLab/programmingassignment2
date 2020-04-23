#makeCacheMatrix creates a special “matrix”, which is really a list containing a function toet the value of the matrix
# 1 get the value of the matrix
# 2 set the value of the inverse
# 3 get the value of the inverse:
# 4 get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# cachesolve his function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

#example 
B <- matrix(c(1,2,3,4),2,2)
B1 <- makeCacheMatrix(B)
cacheSolve(B1)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
