## The file has 2 methods. 1st function is to create a special vector which
## stores the matrix, holds the value of the inverse of the matrix and 
## has functions to set/get the matrix and its inversion. The 2nd function 
## calculates the inversion of the stored matrix if it does not exist

# -------------------
# Function makeCacheMatrix 
# -------------------
# Create a matrix which returns list of the following functions
#   get - return the matrix
#   set - store the matrix
#   getInv - return the inversion of the original matrix
#   setInv - store the inversion of the matrix

makeCacheMatrix <- function(x = matrix()) {
  # Initialise the matrix inversion
  mI <- NULL
  
  # Define the set function
  set <- function(y){
    
    # Store the matrix into a variable
    x <<- y
    
    # Ensure that when the matrix is newly added the inversion is cleared from cache
    mI <<- NULL
  }
  
  # Define the get function to return the stored matrix
  get <- function() x
  
  # Define the setInv function which will store the matrix Inversion
  setInv <- function(matInv) mI <<- matInv
  
  # Define the getInv function which returns the matrix Inversion.
  getInv <- function() mI
  
  # Return list all the functions that are applicable on the vector created.
  list(set = set,get = get, setInv = setInv, getInv = getInv)
}


# -------------------------
# Function - cacheSolve
# -------------------------
# Function to check if inversion of the matrix exists
# If it exists, return the same from cache

cacheSolve <- function(x, ...) {
  # Try and retreive the matrix inversion from cache
  mI <- x$getInv()
  
  # If the inversion exists, simply return the inversion
  if(!is.null(mI)){
    return(mI)
  }
  # Since the inversion doesnot exist in cache, 
  # 1st get the original matrix
  # create the inversion and store it in cache
  data <- x$get()
  mI <- solve(data)
  x$setInv(mI)
  
  # Then return the matrix inversion.
  mI
}
