## This assignment solves the inverse of a matrix by caching the 
# result inside a lexical scope of a function:  "makeCacheMatrix" and 
# "cacheSolve".

##############################################################################
## The function "makeCacheMatrix" creates a new environment. 
## The inverse matrix is cached inside the empty object "InverseMatrix" 
## in the main environment, which is new each time the function is called.
## The output of the function is a list with the four functions that are used: 
## "set", "get", "setInverse" and "getInverse".
##############################################################################

makeCacheMatrix <- function(x = matrix()) {
    InverseMatrix <- NULL # assigns NULL to a variable in the environment 
    set <- function(y){ # Set matrix value
      x <<- y # cache the matrix - assigns value y from other environment
      InverseMatrix <<- NULL # search through the environments for an existing definition of the variable and assign to NULL
    }
    get <- function(){x} # Get the matrix value cached with set
    setInverse <- function(calculatedInverse) {InverseMatrix <<- calculatedInverse} # Cached value of calculatedInverse is saved in InverseMatrix
    getInverse <- function() {InverseMatrix}  # Get the saved value of InverseMatrix that was saved with setInverse
    list(set = set, get = get, # creates list to store the four functions  
         setInverse = setInverse,
         getInverse = getInverse)
        }

######################################################################
## The function "cacheSolve" returns the inverse of the matrix that is 
# returned by makeCacheMatrix function.
######################################################################
cacheSolve <- function(x, ...) { 	## Return a matrix that is the inverse of 'x'
    InverseMatrix <- x$getInverse() # this gets the inverse of a matrix if it has already been calculated
    if(!is.null(InverseMatrix)) { # check to see if cacheSolve has been run before and if it hasn't, sends a text message and returns the cached matrix
        message("getting cached data")
        return(InverseMatrix)
    }
    tato <- x$get()  #run the getmatrix function to get the value of the input matrix
    InverseMatrix <- solve(tato,...) # compute the value of the inverse of the input matrix
    x$setInverse(InverseMatrix) # run the setInverse function on the inverse to cache the inverse
    InverseMatrix ## Return a matrix that is the inverse of 'x'
}
