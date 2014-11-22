## This function creates a special "matrix" object that can cache its inverse. 
makeCacheMatrix <- function(input_matrix = numeric()) {
  inv_mat <- NULL                # initialize this matrix to NULL
  # Definition of the set funtion
  set <- function(y) {                
    input_matrix <<- y           # save the input matrix
    inv_mat <<- NULL             # need to reset to NULL when new object created
  }
  # Definition of the get function to return the matrix
  get <- function() { input_matrix }
  # Definition of the setInverse function
  setInverse <- function(my_mat) { inv_mat <<- my_mat }
  # Definition of the getInverse function to return an inverse matrix
  getInverse <- function() { inv_mat }
  # Returns list methods to call that are defined in this function
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
} 


## This function will return the inverse of the input matrix.
## If the matrix's inverse was already stored from a previous
## call,  it will just retrieve it and return it.
## If the matrix's inverse has not been stored yet,
## need to solve it and store it away and then return it.

## Return a matrix that is the inverse of 'my_matrix' 
cacheSolve <- function(input_matrix, ...) {  # the input is an object created by makeCacheMatrix
  inv_mat <- input_matrix$getInverse()       # accesses the object 'input_matrix' and gets the inverse
  if(!is.null(inv_mat)) {                    # if inverse matrix was already cached (not NULL) ...
    message("getting cached data")           # ... print a message to the console
    return(inv_mat)                          # ... and return the inverse matrix ... "return" ends function
  }
  data <- input_matrix$get()                 # reach this  if input_matrix$getInverse() returned NULL
  inv_mat <- solve(data, ...)                # if inv_mat was NULL then we have to "solve" it
  input_matrix$setInverse(inv_mat)           # store the inverse matrix just solved in input_matrix
  inv_mat                                    # return the solved matrix
} 

