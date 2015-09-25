# a vector containing list of functions that:
# -set the value of the matrix
# -get the value of the matrix
# -set the value of the inverse of matrix
# -get the value of the inverse of matrix
makeCacheMatrix<- function(x_Matrix = matrix()) {
  # Sets Inverse Matrix to null and assigns Y to X
  Inv <- NULL
  set <- function(y) {
    x_Matrix <<- y_Matrix
    Inv <<- NULL
  }
  # function that returns x_matrix
  get <- function() x_Matrix
  # function that assigns Cashed Inverse to Inv
  setInv <- function(CachedInv) Inv <<- CachedInv
  # function that returns Inv
  getInv <- function() Inv
  # list of function names
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

# Function that checks if an Inverse already exists and returns cached inverse if true
# computes the inverse if it is not cached.
# note that as per instructions this function does not check if the matrix is singular
cacheSolve<- function(x=matrix(), ...) {
  # gets the inv matrix from the environment where function was defined
  Inv<-x$getInv()
  # if inverse matrix exists the retreives from cache
  if(!is.null(Inv)){
    message("getting cached data")
    return(Inv)
  }
  # if not in cache then calculte inverse (no error hanfling if matrix singular)
  data <-x$get()
  Inv<-solve(data)
  x$setInv(Inv)
  Inv
}
