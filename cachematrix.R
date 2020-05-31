
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) { 
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x  #function to get values of matrix
  setinverse <- function(inverse) inv <<- inverse #function to set inverse matrix.
  getinverse <- function() inv #function to get inverse matrix.
  
  list(set = set, get = get,    #list of all functions 
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) { #if inv is not null i.e. if inverse of matrix is calucated and cached 
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)  
  x$setinverse(inv)
  inv
  
}

