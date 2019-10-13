
##The functions described below are designed to create a special object that sets the matrix and caches its inverse. 
##First, we create a matrix object, initialize the inverse property, set and then get the matrix. 
##After that we set the inverse matrix and get the inverse of the matrix. Finally, we return a list of all the methods.

makeCacheMatrix <- function(x = matrix()) {
  i = NULL
  set <- function(y)  {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) 
    i<<- inverse
  getinverse <- function() 
    i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#MakeCacheMatrix is a function that creates a special matrix - a list containing a function to:
##1. set the value of the matrix
##2.get the value of the matrix
##3.set the value of the inverse
##4.get the value of the inverse

#The function below calculates the inverse of the special "matrix" created with the above function. 
##It first checks to see if the inverse has already been calculated. 
##If the inverse of the matrix has already been calculated and the matrix has not been changed, it gets the inverse from the cache and skips the computation. 
##In other words, the cacheSolve will retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' - we compare the matrix to what we had and see
  ##if an inverse has already been calculated 
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
