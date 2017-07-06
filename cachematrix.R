## Put comments here that give an overall description of what your
## functions do

## This function creates a "matrix" in the form of a list with functions that
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) i <<-inverse
  
  getInverse <- function() i
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function calculates the inverse of a "matrix" created with the above function. It first checks to see if the inverse
## has been calculated and if so, it retrieves the inverse from the cache. Otherwise, it calculates the inverse and sets the
## value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}


