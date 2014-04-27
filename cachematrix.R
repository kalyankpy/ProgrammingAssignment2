# The concept of this script is to create a set of functions that will enable the user to compute the inverse of a matrix (take matrix input from user)
# This script also enables the user to check if the inverse of the matrix is either previously set by user or computed. If the inverse matrix exists, the value is
# returned otherwise it is computed. Additionally, the script will also enable the user to get the matrix and set new values to the matrix.


# This 'makeCacheMatrix' function will take a matrix as input (user should make sure that it is a square invertible matrix)
# and creates a list of four functions that enable the user to retrieve the value of the matrix (get), to modify the matrix (set)
# to set the inverse of the matrix (setiv) and also to retrieve the value of the inverse matrix for the user input matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y=matrix()){
    x <<-y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(minv) inv <<- minv
  getinv <- function() inv
  list(set=set,
       get=get,
       setinv=setinv,
       getinv=getinv)
}


# This 'cacheSolve' function computes the inverse of the matrix if inverse of tha matrix doesnt exist. If the inverse of the matrix
# exists (ie., either set by user of computed previously), it simply retreives the inverse matrix from the cache. This function
# internally calls the functions created as a list in the 'makeCacheMatrix' function

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
