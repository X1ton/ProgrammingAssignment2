## The functions within these file are creating a "matrix"-object
## capable of storing the inverse of a matrix for caching purpose.

## This function creates a special "matrix"-object capable of
## storing the matrix as well as caching its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  getinverse <- function() i
  setinverse <- function(inv) i <<- inv
  list(set=set,get=get,getinverse=getinverse,setinverse=setinverse)
}


## This function takes a "matrix"-object created by makeCacheMatrix returning
## the inverse of the matrix. In the case this object already contains a cached
## inverse it is returned, otherwise the inverse is calculated and stored within
## the objects cache.

cacheSolve <- function(x, ...) {
  cached_inverse <- x$getinverse()
  if(!is.null(cached_inverse))
    return(cached_inverse)
  data_mat <- x$get()
  inverse <- solve(data_mat)
  x$setinverse(inverse)
  inverse
}
