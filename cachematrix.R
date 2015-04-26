## makeCacheMatrix function returns:
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
## this list is the input to the function cacheSolve()
makeCacheMatrix <- function(x) {       
        ## x is a square invertible matrix
        inverse <- NULL
        set <- function(y) {
                # `<<-` is used to assign a value to an object in an environment 
                # different from the current environment.
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve returns inverse of the original matrix (input to makeCacheMatrix())
cacheSolve <- function(x, ...) {
        ## x is the output of makeCacheMatrix()
        inverse <- x$getinverse()
        # if the inverse has already been calculated
                if(!is.null(inverse)) {
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inverse)
        }
        # otherwise, calculates the inverse 
        data <- x$get()
        inverse <- solve(data, ...)
        # sets the value of the inverse in the cache via the setinv function.
        x$setinverse(inverse)
        return(inverse)
}

#Test makeCacheMatrix and cacheSolve
testinvmatrix = function(mat){
        temp = makeCacheMatrix(mat)
        cacheSolve(temp)      
}

# Create input variable for the makeCacheMatrix using random numbers
r = rnorm(64)
mat = matrix(r, nrow=8, ncol=8)

# Execute the test using the above variable
testinvmatrix(mat)
