## This set of functions caches the inverse of a matrix for reuse, offsetting the need to repeat this costly computation.
    
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = Matrix()) {

## Step 1: Set the value of the matrix
        InverseMatrix <- NULL
        set <- function(y) {
                x <<- y
                InverseMatrix <<- NULL
        }

## Step 2: Get the value of the matrix
        get <- function() x

## Step 3: Set the inverse value of the matrix
        setInverseMatrix <- function(c) InverseMatrix <<- c

## Step 4: Get the inverse value of the matrix
        getInverseMatrix <- function() InverseMatrix

## Step 5: Coerce all values into a list
        list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}

##  cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {

## Step 1: Get the cached inverse value of the matrix
        InverseMatrix <- x$getInverseMatrix()
        
## Step 2: Test if value is null, and return the value otherwise
        if(!is.null(InverseMatrix)) {
                message("getting cached value of inverse matrix")
                return(InverseMatrix)
        }

## Step 3: If value was null, calculate the value
        message("computing value of inverse matrix")
        Matrix <- x$get()
        InverseMatrix <- solve(Matrix, ...)

## Step 4: Cache the calculated value
        x$setInverseMatrix(InverseMatrix)
        InverseMatrix
        
}