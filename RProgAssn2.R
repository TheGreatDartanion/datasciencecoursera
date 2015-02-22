## R Programming Assignment 2
## Dartanion D. Williams


makeCacheMatrix <- function(x = matrix())
{
  inv <- NULL 
  set <- function(y)
  { 
    x <<- y 
    inv <<- NULL 
  } 
  
  get <- function() x 
  
  setinverse <- function(inverse) inv <<- inverse 
  
  getinverse <- function() inv 
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
  
}


cacheSolve <- function(x, ...)
{
  
  i <- x$getInverse() 
  
  if(!is.null(i))
  { 
    return(i) 
  } 
  
  data <- x$get() 
  
  i <- solve(data, ...) 
  
  x$setInverse(i) 
  
  i 
}

