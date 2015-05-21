## The functions in this R-file are used to cache the inverse of a matrix
## in memory. When the user wants to obtain the inverse of a matrix and calls
## these functions, it first check to see if the inverse for this particular
## matrix is already in memory. It returns the cached inverse if the matrix
## hasn't changed. If the matrix has changed, then it calculates the new
## inverse, caches the new inverse, and then returns the inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(aMatrix = matrix()) {
    CachedInverseMatrix <- NULL   # initialize the local variable in this function environment
    CachedMatrix <-aMatrix
    # This function stores the matrix in cache memory and initializes the 
    # stored inverse matrix to null.
    
    # @param: aMatrixToStore
    
    setMatrix<- function(aMatrixToStore) {
        CachedMatrix <<- aMatrixToStore   # store the new matrix in cache
        CachedInverseMatrix <<- NULL   # reinitialize the stored inverse matrix to null
    }
    
    # This function stores returns the matrix stored in cache memory
    
    # @param: N/A
    
    getMatrix <- function(){
        CachedMatrix
    } 
    
    # This function stores the inverted matrix in cache memory
    
    # @param: invertedMatrix - the inverted matrix that is to be cached
    
    setInverseMatrix <- function(invertedMatrix){
        CachedInverseMatrix <<- invertedMatrix  
    } 
    
    # This function stores returns the inverted matrix stored in cache memory
    
    # @param: N/A    
    
    getInverseMatrix <- function(){
        CachedInverseMatrix
    }
    
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(aMakeCacheMatrix, ...) {
    inverseMatrix <- aMakeCacheMatrix$getInverseMatrix()
    if(!is.null(inverseMatrix)) {
        message("getting cached inverted matrix")
        return(inverseMatrix)
    }
    data <- aMakeCacheMatrix$getMatrix()
    inverseMatrix <- solve(data, ...)
    aMakeCacheMatrix$setInverseMatrix(inverseMatrix)
    
    return(inverseMatrix)
    
    ## Return a matrix that is the inverse of 'aMakeCacheMatrix'
}
