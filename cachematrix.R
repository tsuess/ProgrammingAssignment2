## Put comments here that give an overall description of what your
## functions do

## This function is setter and getter function controlling the access and saving of the cached Matric


makeCacheMatrix <- function(x = matrix()) {
      solved <- NULL
      set <- function(y) {
            x <<- y
            solved <<- NULL
      }
      get <- function() x
      setsolved <- function(inverse) solved <<- inverse
      getsolved <- function() solved
      list(set=set, get=get, setsolved=setsolved, getsolved=getsolved)
}




## This function controlles the calculation of the inverting of a matrix in cooperation with corresponding makeCacheMatrix

cacheSolve <- function(x, ...) {
      solved <- x$getsolved()
      if(!is.null(solved)) {
            message("getting cached data.")
            return(solved)
      }
      data <- x$get()
      solved <- solve(data)
      x$setsolved(solved)
      solved
}