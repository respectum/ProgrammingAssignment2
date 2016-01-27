## functions that enabling creation of cached matrix as a list
## using advantage of lexical scoping for cached inverse calculation

##This function creates matix as a list of functions

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x<<-y
    inv<<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv<<- inverse
  getinverse <- function() inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  
}


## This function checks if the inverse was already calculated
## and returns calculated inverse or calculates it on the fly

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  data<-x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
