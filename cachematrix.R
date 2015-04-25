## makeCacheMatrix function returns:
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
## this list is the input to the function cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
        ## x is a square invertible matrix
        inv = NULL
        set = function(y) {
                # `<<-` is used to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) 
                inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## cacheSolve returns inverse of the original matrix (input to makeCacheMatrix())
cacheSolve <- function(x, ...) {
        inv = x$getinv()
        # if the inverse has already been calculated
        # get it from the cache and skips the computation.
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}

#Test makeCacheMatrix and cacheSolve
testinvmatrix = function(mat){
        temp = makeCacheMatrix(mat)
        cacheSolve(temp)      
}

# Create input variable for the makeCacheMatrix using random numbers
r = rnorm(36)
mat = matrix(r, nrow=6, ncol=6)

# Execute the test using the above variable
testinvmatrix(mat)


