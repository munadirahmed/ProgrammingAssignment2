## The functions in this R-file are used to cache the inverse of a matrix
## in memory. When the user wants to obtain the inverse of a matrix and calls
## these functions, it first check to see if the inverse for this particular
## matrix is already in memory. It returns the cached inverse if the matrix
## hasn't changed. If the matrix has changed, then it calculates the new
## inverse, caches the new inverse, and then returns the inverse.

##############################

## The function takes a square matrix and creates an object. It can be thought
## of as a function that creates a class of type makeCacheMatrix. Each object 
## of this type exits in its own environment where it stores the value of the 
## matrix and it's inverse. The methods of these objects allow the user to 
## retrieve or reset the matrix and/or its inverse. 

## When the setMatrix() method used on an existing object the inverse is reset to 
## null as the matrix stored in the object has changed. The user must then use
## the setInverseMatrix() method to store the inverse of the new matrix

makeCacheMatrix <- function(aMatrix = matrix()) {
    CachedInverseMatrix <- NULL   # initialize the local variable in this function environment
    CachedMatrix <-aMatrix
    
    # This function stores the matrix in cache memory and initializes the 
    # stored inverse matrix to null.
    
    # @param: aMatrixToStore - the desired matrix to be stored in memory
    
    setMatrix<- function(aMatrixToStore) {
        CachedMatrix <<- aMatrixToStore   # store the new matrix in cache
        CachedInverseMatrix <<- NULL   # reinitialize the stored inverse matrix to null
    }
    
    # This function stores returns the matrix stored in cache memory
    
    # @param: N/A
    
    getMatrix <- function(){
        CachedMatrix
    } 
    
    # This function stores the inverted matrix in cache memory. Note, it does
    # not invert the matrix itself. It must be given the inverted matrix that is
    # to be stored
    
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

##############################

## This function takes an obect of type makeCacheMatrix and checks to see
## if the inverse of the matrix stored in the object has already been cached.
## If the inverse has been calculated previously and stored then it just 
## returns the inverse. If the inverse has not been calculated and stored previously
## the inverse is calculated and cached for future use.

cacheSolve <- function(aMakeCacheMatrix, ...) {
    
    # store the inverse matrix in a local variable
    inverseMatrix <- aMakeCacheMatrix$getInverseMatrix()
    
    # check to see if the inverse exists or if it is null
    if(!is.null(inverseMatrix)) {
        
        #return the inverse if it has already been cached
        message("getting cached inverted matrix")
        return(inverseMatrix)
    }
    
    # store matrix in a local variable
    matrix <- aMakeCacheMatrix$getMatrix()
    
    # calculate the inverse of a matrix and store in a local variable
    inverseMatrix <- solve(matrix, ...)
    
    # store the inverted matrix in the object
    aMakeCacheMatrix$setInverseMatrix(inverseMatrix)
    
    # returns the inverted matrix
    return(inverseMatrix)
}
