## The purpose of these functions is to provide a way to create and cache the inverse of a matrix

## The makeCacheMatrix is creates a matrix and provides a list of methods to work with that matrix

makeCacheMatrix <- function(x = matrix()) {
  #initialize the inverse matrix to null
  i <- NULL
  #this method provides a way to set the matrix x to y
  set <- function(y) {
    x <<- y
    i <- NULL
  }
  #get the matrix
  get <- function() x
  #set the inverse of the matrix
  setinverse <- function(inverse) i <<- inverse
  #get the inverser of the matrix
  getinverse <- function() i
  #return a list of all the functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## The cacheSolve matrix checks if the inverse of a matrix exists, 
## if it does it returns the inverse,
## if nor it calculates the inverse and caches it for future use

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){ #if the inverse exists
    message("Getting cached inverse matrix")
    return(i)
  }
  #get the matrix
  data <- x$get()
  #calculate the invese
  i <- solve(data)
  #cache the invese
  x$setinverse(i)
  #return the inverse
  i
}
