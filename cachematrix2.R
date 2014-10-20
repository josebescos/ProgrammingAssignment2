## This functions cache the inverse of a matrix, saving
## computing time


##  Following function creates a matrix-like object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
    s <- NULL
    set <- function(y)
    {
      x <<- y
      s <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) s <<- solve
    getinverse <- function() s
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Following function computes the inverse of the "matrix" returned by previous function 

cacheSolve <- function(x, ...) 
  {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}
