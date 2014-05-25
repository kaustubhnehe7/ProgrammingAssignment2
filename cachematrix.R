## Programming Assignment 2
## Name : Kaustubh Nehe

## Usage: source('cachematrix.R')
##        x <- makeCacheMatrix(mat)  # mat is an invertible matrix
##        cacheSolve(x)              # get the inverse result


## This program provides a set of functions takes a square matrix 
## as the input and return
## 1. The matrix in its original form
## 2. Inverse of the matrix
## The makeCacheMatrix also caches the inverse, so it can be reused 

## Function :  makeCacheMatrix
## 1.Takes matrix as the input
## 2.Calculates the inverse of the matrix
## Returns a set of options to print the matrix in original
##  or inverse form. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

##Function Name: cacheSolve
##1. Takes the matrix to be inversed as the input
##2. Checks to see if the inverse was already cached before
##   if yes then prints a message that the data is in cache and 
##   prints the matrix
##3. Calculates the inverse and prints the inverse.



cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
   message("Data is already cached")
   return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}


