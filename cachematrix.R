## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This is somewhat of a copy of the functions created in the example, but the functionality is changed

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y)
    {
      x <<- y
      i <<- NULL
    }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  i <- x$getinverse()
  if (!is.null(i)) 
    {
      message("getting cached data")
      return(i)
    }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## Test case 
K <- matrix(c(12,45,12,15,6,27,5,78,93),3,3)
K1 <- makeCacheMatrix(K)
cacheSolve(K1)
cacheSolve(K1)

