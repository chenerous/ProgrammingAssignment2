## This R file contains two functions:  

## 1.  makeCacheMatrix -- This function creates a special "matrix" object
## that can cache its inverse.

## 2.  cacheSolve -- This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.



## makeCacheMatrix creates a list containing 4 functions:
## 1.  set the value of the matrix 
## 2.  get the value of the matrix 
## 3.  set the value of the matrix inverse 
## 4.  get the value of the matrix inverse
## Within the function, the matrix is denoted by x and the inverse by inv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## 'set' the matrix by assigning the matrix to be the input of the below 
  ## function
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## 'get' the matrix by returning the matrix
  get <- function() x
  
  ## 'set' the inverse by assigning the inverse matrix to be the input of the 
  ## below function
  setinverse <- function(inverse) inv <<- inverse
  
  ## 'get' the inverse by returning the inverse matrix 
  getinverse <- function() inv

  ## return a list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates the inverse of the matrix created with the 
## makeCacheMatrix function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Within the function, x denotes the 
## list returned by makeCacheMatrix and inv denotes the matrix inverse
 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' (where 'x' is the output 
  ## of makeCacheMatrix)
  
  inv <- x$getinverse() ## check to see if inverse is already calculated
  
  ## if so, print message and return inverse
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## if not, calculate inverse 
  ## (note, no 'else' is needed here because the if statement above triggers a 
  ## function return)
  data <- x$get()
  inv <- solve(data, ...)
  
  ## cache the inverse
  x$setinverse(inv)
  
  ## return the inverse
  inv
}
