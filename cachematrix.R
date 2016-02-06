## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# ********************************************************************
# ********************************************************************

## set() is used to set the value of matrix. get() is used to retrive the matrix. setInverse() sets the value of inverse and  getInverse() returns the 
##value of inverse

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(set = set,get = get,setInverse = setInverse, getInverse = getInverse)
}


## Cachesolve Computes the inverse of the matrix returned by `makeCacheMatrix` above. If the inverse hasalready been calculated (and the matrix has not changed), then 
##cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
  
  inv <- x$getInverse()
  
  if (!is.null(inv)) 
  {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  
  x$setInverse(inv)
  inv
}