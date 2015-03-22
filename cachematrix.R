
## Nasir Ali Mirza
## R Programming Course Assignment 2 (Lexical Scoping)
## Date: March 21, 2015

#Function Name: makeCacheMatrix()
#Parameter: Invertable matrix
#Purpose: stores matrix in global environment variable mtrx, and cache's its inverse in another global variable inv_mtrx 
#Returns: A list having reference to following 4 functions:
#               1.getm to get matrix stored in mtrx
#               3.getim to get inverse of matrix cached in inv_mtrx
#               3.setm to store matrix in mtrx
#               4.setim to get inverse of matrix cached in inv_mtrx

makeCacheMatrix <- function(x = matrix()) {
        inv_mtrx <- NULL
        
        #function to store matrix in global environment
        set_mtrx <- function(y) {
                mtrx <<- y
                inv_mtrx <<- NULL
        }
        
        #function to get matrix stored in global environment
        get_mtrx <- function() mtrx
        
        #function to store (cache) inverse of a matrix
        setinv_mtrx <- function(inv) inv_mtrx <<- inv
        
        #function to get inverse of a matrix from a global environment
        getinv_mtrx <- function() inv_mtrx
        
        #list with 4 elements named setm, getm, setim, getim. Each list element contains a corresponding function
        list(setm = set_mtrx, getm = get_mtrx, setim = setinv_mtrx, getim = getinv_mtrx)
}


#Function Name: cacheSolve()
#Parameter: List returned by makeCacheMatrix
#Purpose: Calculates the inverse of a matrix created with makeCacheMatrix. However, it first checks to see if the 
#         inverse has already been calculated If so, it gets the inverse from the cache and skips the computation. 
#         Otherwise, it calculates the matrix inverse of the data and sets the value of the matrix inverse (inv_mtrx) 
#         in the cache via the setim function.
#
#Returns: Inverse of a matrix from cache if present otherwise by calculating 

cacheSolve <- function(fn_list,...) {
        
        #pulls the inverse of a matrix from the cache if it already exists and returns
        im <- fn_list$getim()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        
        #incase inverse of a matrix doesn't exist in the cache, it is calculated and stored in cache and returned
        data <- fn_list$getm()  #getting data from global cache
        inv_mtrx <- solve(data,...) #calculating inverse of a matrix
        fn_list$setim(inv_mtrx) #storing inverse in a cache
        return(inv_mtrx)
}


#Usage and example
# 1. Create invertible matrix
# 2. Call makeCacheMatrix() and pass invertible matrix as parameter
# 3. Save returned List by makeCacheMatrix
# 4. Call cacheSolve and pass list returned by makeCacheMatrix
# 5. Try to call cacheSolve or getim again, this time data will be pulled from cache instead of recalculation

#step-1
mtrx <- matrix(c(3,2,-1,2,-2,4,-1,.5,-1), 3,3) #Invertible matrix of 3x3

#step-2 & 3
fn_list <- makeCacheMatrix(mtrx)

#step-4
cacheSolve(fn_list)

#step-5
cacheSolve(fn_list)
