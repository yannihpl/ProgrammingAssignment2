## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  #initializing inverse as NULL
    set <- function(y){
      x <<- y
      inv <<- NULL
    }
    get <- function()x  #function to get matrix x
    setInverse <- function(inverse)inv <<- inverse
    getInverse <- function() {
      inver<-ginv(x)
      inver%*%x
    }
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
  }
  


cacheSolve <- function(x, ...) {   #get cache data
        
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)){   #checking if inverse is NULL
      message("getting cached data")
      return(inv)   #return inverse value
    }
    data <- x$get()
    inv <- solve(data,...)  #calculate inverse value
    x$setInverse(inv)
    inv  #return a matrix as the inverse of x
  
}
