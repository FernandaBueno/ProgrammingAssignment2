## Put comments here that give an overall description of what your
## functions do

# function makeCacheMatrix creates an object (a list) that caches a matrix 
# (that we want to compute its inverse). To test the functions, please consider
# that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
    invemat <- NULL 
    set <- function(y) {
        x <<- y
        invemat <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invemat <<- inverse
    getinverse <- function() invemat
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# Function cacheSolve retrieves the inverse of matrix 'x' cached in 
# the special mtrix object created in the function above

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invemat <- x$getinverse()
    if(!is.null(invemat)) {
        message("getting cached data")
        return(invemat)
    }
    data <- x$get()
    invemat <- solve(data, ...)
    x$setinverse(invemat)
    invemat
}
##############################################

# Testing the function

# Create a square matrix (assume that the matrix supplied is always invertible)
ExampleMatrix <- matrix(c(3,6, 10, 4), 2, 2)
# to check the inverse matrix, type 'solve(ExampleMatrix)'. 
# The result of solve() should match the final result 

# Creates the object SpecialMatrix
SpecialMatrix <- makeCacheMatrix(ExampleMatrix)

# Our original matrix is actually here:
SpecialMatrix$get()

# Now, let's compute the inverse matrix cached in the object SpecialMatrix
cacheSolve(SpecialMatrix)

# Check the result by writting 'solve(ExampleMatrix)' in the command line


# Another example with a bigger matrix (4x4)
ExampleMatrix2 <- matrix(rnorm(16),4) 
SpecialMatrix2 <- makeCacheMatrix(ExampleMatrix2)
cacheSolve(SpecialMatrix2)
# Check the result by writting 'solve(ExampleMatrix2)' in the command line







