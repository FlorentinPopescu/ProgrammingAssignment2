# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly. The 
# following two functions are used to cache the inverse of a matrix. 
 

# makeCacheMatrix creates a special "matrix" object that can cache the inverse of a matrix.

# The steps in construction makeCacheMatrix are similar with the makeVector function. 
# Below the makeCacheMatrix is constructed on the scheleton of makeVector

# Steps:
# 1. set the value of the matrix  
# 2. get the value of the matrix 
# 3. set the value of the inverse of the matrix 
# 4. get the value of the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) { 
                inv_mat <- NULL 
                set <- function(y) { 
                x <<- y 
                inv_mat <<- NULL 
                } 
                get <- function() x 
                setinverse <- function(inverse) inv_mat <<- inverse 
                getinverse <- function() inv_mat 
                list(set=set, get=get, 
                     setinverse=setinverse, 
                     getinverse=getinverse) 
        } 


# The cacheSolve function below calculates the inverse of the matrix. It first checks if 
# the inverse has already been calculated. If so, it gets the inverse and skips the 
# computation. Otherwise, it calculates the inverse and sets the value of the inverse 
# in the cache via setinverse function. 

# We assume that the matrix is always invertible. 
cacheSolve <- function(x, ...) { 
           inv_mat <- x$getinverse() 
           if(!is.null(inv_mat)) { 
                message("getting cached data.") 
                return(inv_mat) 
                } 
        data <- x$get() 
        inv_mat <- solve(data) 
        x$setinverse(inv_mat) 
        inv_mat 
        } 

## Sample run: 
## will use the 2x2 matrix discussed on "https://www.mathsisfun.com/algebra/matrix-inverse.html"

## > x = rbind(c(4, 2), c(7, 6)) 
x = rbind(c(4, 7), c(2, 6))
 
## > m = makeCacheMatrix(x)
m = makeCacheMatrix(x)

## > m$get() 
##       [,1]  [,2] 
## [1,]  4     7 
## [2,]  2     6 
m$get() 

## No cache in the first run 
## > cacheSolve(m) 
##       [,1]   [,2] 
## [1,]  0.6   -0.7 
## [2,] -0.2    0.4 
cacheSolve(m)

## Retrieving from the cache in second run 
## > cacheSolve(m) 
## getting cached data. 
##       [,1]   [,2] 
## [1,]  0.6   -0.7 
## [2,] -0.2    0.4 
## >  
cacheSolve(m)

