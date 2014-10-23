## The following set of functions are meant to cache the computation of the inverse of a matrix with the goal
## of caching the computed inverse matrix and avoid redundant computations of the same inverse matrix if it 
## is needed to be computed several times. In short, the initial inverse matrix is computed, then cached, and if
## there is no change to the initial inputed matrix, then the cached inverse matrix is re-used as the output
## of the function checking for existance of an inverse matrix. The implementation of this goal was done in two
## sequential parts. The first obtains the current state of the a matrix, and of the inverse matrix. The function
## has the property of initializing the inverse matrix variable to NULL, with the goal of forcing the subsequent
## function to compute the inverse of the inputted matrix. If there is no change on the inputted matrix, then
## the subsequent function returns the existing inverse matrix, if one exists. Otherwise, the inverse matrix
## is returned as the output of the function. 

## The following function creates a list object containing 4 different functions that in short:
## sets the parameters for efficientely caching a computes Inverse Matrix, by initializaing a
## inv variable to store the current status of the Inverse Matrix. Accordingly, it passes and sets the inputted matrix,
## as well as gets and passess the computed inverse matrix from the global and local environment.
## The outputted list object is then inputted to the "cacheSolve" function for computing the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()){
        Inv <- NULL                                             # Initiallizing inverse variable
        set <- function(y) {                                    # Function to reset Inv to NULL, and to assign
                Inv <<- NULL                                    # a new matrix y as the current matrix in the 
                x <<- y                                         # global environment
        }
        get <- function() {                                     # Assigning input matrix to function's environment
                x
        }
        setInverse <- function(Inverse) {                       # Assign the current Inverse Matrix to the functions Inv variable
                Inv <<- Inverse
        }
        getInverse <- function(){                               # Assign the Inv matrix to the function getInverse variable 
                Inv
        }        
        list(                                                   # Output all computed results in a list format                    
                set = set, 
                get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}


## The following function takes the outputted list object from the "makeCacheMatrix" function and
## computes the inverse of the inputted matrix in the x$get() function. Additionally, it checks whether
## a computes inverse matrix already exits. If it exists, then the inverse matrix is not calculated, but
## rather the existing matrix is cached and returned as the output of this function. In the case that the
## inverse of a matrix does not exists, then the function computes the inverse of the matrix inputted to 
## this function.

cacheSolve <- function(x, ...){
        inv <- x$getInverse()                                   # get current inverse matrix
        if(!is.null(inv)){                                      # in the case that an inverse matrix
                message("getting cached inverse matrix")        # already exists, return its current value
                return(inv)                                     # and do not execute the rest of the function
        } else{                                                 # Otherwise,
                data <- x$get()                                 # get the current matrix
                Inv <- solve(data)                              # compute its inverse
                Current_InvMatrix <- x$setInverse(Inv)          # and output the computed inverse matrix
        }
}
