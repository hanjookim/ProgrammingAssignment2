## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix function creates a special "matrix" object
## that can cache its inverse.
## It is really a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Create a matrix object x and an associated function
  inv <- NULL
  ## Define the cache inv
  set <- function(y) {
      x <<- y
  ## Assign the input matrix y to the variable x
  ## in the parent environment
      inv <<- NULL
  ## Re-initialize inv in the parent environment to null
  }
  get <- function() x
  ## Return the matrix x
  setinverse <- function(inverse) inv <<- inverse
  ## Set the cache inv equal to the inverse of the maxtrix x
  getinverse <- function() inv
  ## Return the cached inverse of x
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

## cachSolve function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.  If the inverse has already
## been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.  This
## function assumes that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
  ## Check if the inverse has already been calculated
  ## If yes, then get the inverse from the cache
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  ## Calculate the matrix inverse and set the value of the
  ## inverse in the cache via the 'setinverse' function.
}
