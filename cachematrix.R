## Matrix inversion is a costly computation and their is benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. 
## This is accomplished by tow functions makeCacheMatrix & cacheSolve       

## This function creates a special "matrix" object that can cache its inverse.
## This function returns a list which contains a function to 
## 1.Initialize the value of the matrix
## 2.Return the matrix
## 3.Set the value of the inverse of the matrix
## 4.Return the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
                
                inverseMatrix <- NULL
                initializeMatrix <- function(y) {
                        
                                x <<- y
                                inverseMatrix <<- NULL
                }

                getMatrix <- function() {
                                x
                }
                setInverse <- function(inverse){
                                inverseMatrix <<- inverse
                }
                getInverse <- function(){
                                inverseMatrix
                }
                list(initializeMatrix = initializeMatrix, getMatrix = getMatrix, setInverse = setInverse,
                     
                     getInverse = getInverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
                matrix1 <- x$getInverse()
                if(!is.null(matrix1)){
                                message("getting cached data")
                                return(matrix1)
                        
                }
                
                data <- x$getMatrix()
                matrix1 <- solve(data)
                x$setInverse(matrix1)
                matrix1
}
