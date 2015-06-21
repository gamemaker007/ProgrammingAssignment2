## Put comments here that give an overall description of what your
## functions do


## I have retained most of the makeVector and cachemean program structure 
## and made the necessary changes.

## Write a short comment describing this function

## makecacheMatrix function returns a list containing a function to
## 1.set the value of the vector 
## 2.get the value of the vector
## 3.set the value of the inverse
## 4.get the value of the inverse

##  The " <<- " operator which can be used to assign a value to an object 
##  in an environment that is different from the current environment.

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL
  set <- function(y) {
  x <<- y
  inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse <<- inverse
  getinverse <- function() inverse
  list(set = set, get = get,
  setinverse = setinverse,
  getinverse = getinverse)
}




## Write a short comment describing this function

## The following function calculates the inverse of the special "vector" 
## created with the above function. However, it first checks to see 
## if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value 
## of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
