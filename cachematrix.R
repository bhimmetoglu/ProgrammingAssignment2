## makeCahceMatrix: 
## Creates a special matrix object that caches 
## (i.e. stores it in the memory) its inverse. Output of makeCahceMatrix
## is a list of functions.
## 
## cacheSolve:
## It is a function that computes the inverse of the matrix created from
## makeCacheMatrix. If the inverse is already stored in memory,
## and the matrix has not been reset, it fetches the inverse from
## memory. Otherwise, it simply computes the inverse.

## This function returns a list of functions, setting up a special matrix
## that can store its inverse in cahce.
## Example:
## m <- matrix(c(1,2,3,4), 2, 2) # 2x2 matrix
## a <- makeCacheMatrix(m)
## a$get() # Returns the matrix a
## a$set(matrix(1:6,3,3)) # Resets the matrix to a new one
##                        # If a$get() is used after this, it will print the new matrix
##
## a$setInv() # Will set the inverse, and store it in cache (see below). It is used
##            # in the function cacheSolve.
## a$getInv() # Will print the inverse. If inverse not yet set, it will print NULL

makeCacheMatrix <- function(x = matrix()) {
  InvMat <- NULL  # Locally set the inverse to NULL
  set <- function(y) {
    x <<- y # Once set is called, assign y to x in parent env.
    InvMat <<- NULL # Once set is called, assign InvMat to NULL in parent env. 
                    # This ensures that once the matrix is re-set, it inverse is back to NULL
  } 
  get <- function() x # Once get is called, x taken from parent env. and assigned
  setInv <- function(Inv) InvMat <<- Inv # Once setInv is called, InvMat is set to Inv in parent env.
  getInv <- function() InvMat # Once getInv is called, InvMat is taken from parent env.
  list(set=set,get=get,setInv=setInv,getInv=getInv) # makeCacheMatrix returns this list of functions.
}


## This functions either calculates the inverse of the matrix set from 
## makeCacheMatrix, or calls its value from the memory.
## Example:
## cacheSolve(a)  # This will initially compute the inverse of matrix a directly,
##                # since it is not stored in memory before
## cacheSolve(a)  # The second time called, it will fetch the inverse from the memory
##                # A message will be printed: "Getting Inverse Marix from cache"
## After these steps, if we call getInv method from makeCacheMatrix, it will also
## fetch the inverse from the memory:
## a$getInv()   # It will not print NULL anymore

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  InvMat <- x$getInv() # Get InvMat from Global env. 
  
  # If matrix inverse is already calculated (InvMat != NULL) and the matrix itself has not changed
  if (!is.null(InvMat)){
    message("Getting Inverse Marix from cache")
    return(InvMat)
  }
  m <- x$get() # Get the matrix from makeCacheMatrix's env.
  Inv <- solve(m) # Calculate inverse
  x$setInv(Inv) # Set the Inverse in makeCacheMatrix's env. So that if getInv is called, the value Inv
                # is returned
  Inv
}