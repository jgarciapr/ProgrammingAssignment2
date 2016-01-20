
## For a given matrix (suposing its invertible), caches the matrix and its inverse
## Returns list of functions for operating the cache

makeCacheMatrix <- function(x = matrix()) {
  #initialize
  inv <- NULL
  mat <- NULL
  #functions for operating the matrix
  setmat <- function(y) { mat <<- y }
  setinv <- function(y) { inv <<- solve(y)  }
  getmat <- function() mat
  getinv <- function() inv
  #operates depending if the parameter is a matrix or not
  if (!is.matrix(x))
  { message("parameter is not a matrix") 
    return (NULL)
  } else 
  {
    #saves the matrix passed and its inverse in the cache in the initialization
    #setmat(x)
    #setinv(x)
    #returns list of functions to operate the cache
    list(setmat = setmat, setinv = setinv, getmat = getmat, getinv=getinv)
  }
}
  


## function cacheSolve is used to check if the list object passed as parameter 
## has the matrix and its inverse cached

cacheSolve <- function(x, ...) {
 
  mat <- x$getmat()
  inv <- x$getinv()
 #check if the list has values for the matrix and the inverse and returns the inverse from cache
    if(!is.null(mat) && !is.null(inv)) {
    message("getting cached data")
    return(x$getinv())
    } else 
 #If the matrix is not set it returns NULL
    if (is.null(mat)) {
    message("no matrix is cached")
    return (NULL)
    } else
 #If the matrix is set (but not the inverse) it calculates, cachesand returns the inverse
    message("calculating inv of cached matrix")
    mat<-x$getmat()
    inv<-solve(mat)
    x$setinv(mat)
    inv
  }