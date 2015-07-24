### Introduction

## Put comments here that give an overall description of what your
## functions do

## This second programming assignment will write a R function that 
## is able to cache potentially time-consuming computations.
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. The assignment is to write a pair of functions that
## cache the inverse of a matrix.

## The next function is example from coursera
makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}


## Write a short comment describing this function

## The function, `makeCacheMatrix` creates a special "Matrix" object
## which is really a list containing a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse matrix
## 4.  get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                setmatrix <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                getmatrix <- function() x
                setinverse <- function(inverse) m <<- inverse
                getinverse <- function() m
                list(setmatrix = setmatrix, getmatrix = getmatrix,
                     setinverse = setinverse,
                     getinverse = getinverse)
                
    }

## Write a short comment describing this function
## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

## Computing the inverse of a square matrix can be done with the `solve`
## function in R. For example, if `X` is a square invertible matrix, then
## `solve(X)` returns its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getmatrix()
        m <- solve(data,...)
        x$setinverse(m)
        m
}
