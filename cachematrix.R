##R Programming - Sebastian Cedeño Fonseca 

## This function allows creating a square matrix, 
## calculating its inverse and caching 
##it in order to avoid the repetitive calculation of the inverse


## The first function allows creating an array 
##with the necessary properties to store cache

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
          x <<- y
          inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## With the help of the "solve" function, 
##the inverse of the matrix is calculated

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
          message("Extracting cached data")
          return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
