## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix ## rather than compute it repeatedly.

## Following are a pair of functions that cache the inverse of a matrix.:

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


makeCacheMatrix<-function(m=matrix()){
        ## This function creates a special "matrix" object that can cache its inverse
        
        
        inverseMatrix<-NULL
        
        set<-function(y){
                m<<-y
                inverseMatrix<<-NULL
        }
        
        get<-function() m
        
        ## Store the inverse of the matrix
        setInverse<-function(inverse) inverseMatrix<<-inverse
        
        ## Retrun the inverse of the matrix
        getInverse <- function() inverseMatrix
        
        ## Retrun the list of functions to check or set the inverse
        list(set=set, get=get, setInverse=setInverse,getInverse=getInverse)
} # end makeCacheMatrix





cacheInverseMatrix <- function(makeCacheMatrixObject,...) {
        
        ## This function computes the inverse of the special "matrix" returned 
        ## by makeCacheMatrix above. If the inverse has already been calculated 
        ## (and the matrix has not changed), then the cachesolve should retrieve 
        ## the inverse from the cache.
        
        ## This function checks if a cached version of inverse exist for the
        ## makeCacheMatrixObject
        
        ## Check if an inverse already exists
        
        inverse<-makeCacheMatrixObject$getInverse()
        if(!is.null(inverse)) {
                message("Matrix Inverse Exists getting cached inverse")
                return (inverse)
        } # end if
        
        ## If the inverse does not exist create one
        
        ## Get the matrix
        myMatrix<-makeCacheMatrixObject$get()
        
        ## Create the inverse
        inverse<-solve(myMatrix)
        
        ## Store the inverse
        makeCacheMatrixObject$setInverse(inverse)
        
        ## Return the inverse
        inverse

} # end cacheInverseMatrix
