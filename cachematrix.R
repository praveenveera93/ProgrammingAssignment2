## combination of functions that stores inverse of matrix in 
## cache to retrieve at a later time in order to save computing
## power and time

## Creates a matrix like object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Computes inverse of matrix like object return by makecachematrix function.
##  If inverse already calculated, retrieves inverse from cache instead of computing again.


cacheSolve <- function(x, ...){
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    meassage("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  inv$setinverse(inv)
  inv
}

