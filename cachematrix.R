## Since functions work in their own 'scope' and only return the value of the last item in the function, 
## being able to send values to the parent environment for use by other functions later can be helpful.
## The use of assignment operator <<- in this case will create the variable in the parent environments.
## This example incorporates functions to generate, cache and read from cache an inverse matrix.
## Two functions will be written here.  One will create a NULL matrix and define the functions to set
## and get either a new matrix or the inverse matrix.  The second function will check for an already
## cached inverse matrix and use it, or if the inverse matrix does not exist it will create it.

## makeCacheMatris will create a 1x1 NULL matrix to start with and then define 4 functions
## 1) a function to set the values for an inverse matrix object and cache to the parent environment
## 2) a function to get the values for matrix 'x'
## 3) a function to set the inverse of matrix 'x' should it not exist
## 4) a function to get the inverse of matrix 'x' for use by another function

makeCacheMatrix <- function(x = matrix()) {
     im <- matrix()                                      # create a NULL 1x1 matrix
     set <- function(y) {                                # function to set the initial value of the matrices
          x <<- y
          im <<- matrix(data = list(NULL))
     }
     
     get <- function() x                                 # function to get the matrix 'x'
     setinvMat <- function(invMat) im <<- invMat         # function to set the inverse matrix to cache
     getinvMat <- function() im                          # function to get the inverse matrix from cache (parent environment)
     
     list(set = set, get = get, setinvMat = setinvMat, getinvMat = getinvMat)
}


## cacheSolve is a function that will first check to see if a solved inverse matrix has been cached.
## Since the variable 'im' is not passed to the function, the function will look for it as a 'free variable'
## in the parent environment.
## If there is a solved matrix cached, (!is.null(im) is TRUE) the function will grab the matrix from
## the cache and stop with the use of the call return(im).
## If the solved matrix is NULL (!is.null(im) is FALSE) then the inverse of the matrix will be calculated
## and cached to the parent environment

cacheSolve <- function(x, ...) {
     im <- x$getinvMat()                                # get the inverse matrix from cache (parent evironment)
     if (!is.null(im)) {                                # if the matrix 'im' is NOT NULL, then return 'im' and exit the function
          message("getting cached data")
          return(im)
     }                                                  # if the matrix 'im' is NULL then continue the function
     data <- x$get()                                    # use the 'get' function defined earlier to define the matrix as 'data'
     im <- solve(data, ...)                             # invert the matrix using the function solve()
     x$setinvMat(im)                                    # cache the solved inverse matrix for use by another function later if needed
}
