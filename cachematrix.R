################################################################################
## Since functions work in their own 'scope' and only return the value of the ##
## last item in the function, being able to send values to the parent         ##
## environment for use by other functions later can be helpful.               ##
## The use of assignment operator <<- in this case will create the variable   ##
## in the parent environment.                                                 ##
##                                                                            ##
## This example incorporates functions to generate, cache and read from cache ##
## an inverse matrix.  Two functions will be written here.  One will create   ##
## a NULL variable 'im' and define the functions to set and get either a new  ##
## matrix or the inverse matrix.  The second function will check for cached   ##
## inverse matrix to use, or if the inverse matrix does not exist, create it. ##
################################################################################

# makeCacheMatris will create a NULL 'im' variable to start with and then
# define 4 functions:
# 1) a function to set the values for a matrix and cache to the parent env
# 2) a function to get the values for matrix 'x'
# 3) a function to set the inverse of matrix 'im' should it not exist
# 4) a function to get the inverse of matrix 'im' for use by another function

makeCacheMatrix <- function(x = matrix()) {
     im <- NULL                              # set the variable 'im to NULL
     set <- function(y) {                    # function to set the value of the
          x <<- y                            # matrices and reset im to NULL
          im <<- NULL
     }
     
     get <- function() x                          # fun to get the matrix 'x'
     setinvMat <- function(invMat) im <<- invMat  # fun to set 'im' to cache
     getinvMat <- function() im                   # fun to get 'im'from cache

     # create a list object with all functions in it for easier use later
     list(set = set, get = get, setinvMat = setinvMat, getinvMat = getinvMat)    
}


# cacheSolve is a function that will first check to see if a solved inverse
# matrix has been cached.  
# If 'im' is cached, (!is.null(im) is TRUE) the function will grab the matrix
# from the cache and stop with the use of return(im).
# If 'im' is NULL - a solved 'im' does not exist (!is.null(im) is FALSE) then
## 'im' will be calculated and cached to the parent environment

cacheSolve <- function(x, ...) {
     im <- x$getinvMat()                     # call getinvMat to retreive 'im'
     if (!is.null(im)) {                     # if im' is NOT NULL, 
          message("getting cached data")     # print message and then
          return(im)                         # return 'im'
     }
                                             # if 'im' is NULL;
     data <- x$get()                         # use 'get' to load new matrix
     im <- solve(data, ...)                  # invert matrix using solve()
     x$setinvMat(im)                         # cache 'im' to parent env
     im
}
