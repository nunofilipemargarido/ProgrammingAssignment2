## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function takes a matrix and saves 
## the values in a 

makeCacheMatrix <- function(x = matrix()) {
  ##storing inverse matrix
  ## This variable is put in the envioment 
  ## of the anonimos fuction 
  minv=NULL;
  ##this function  put the variable y in the enviroment of the parent 
  ##function
  set = function(y){
    x<<- y;
  }
  
  
  ## Store the matrix inverse in the enviroment
  ## of the parent fuction
  setInvMatrix = function(imx){
    minv <<- imx;
  }

  ##Return The original stored Matrix
  ## of the parent enviromment
  get = function()x;
  ## Return the inverser metrix off 
  ## the parent enviroment
  getInvMatrix = function()minv;
  
  ##Put the functions in a list to be used
  ##By the variable
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix,
       calcInverseMatrix=calcInverseMatrix)
  
}


## Write a short comment describing this function
## This function is to calculate the inverse matrix and store it 
## in the makeCacheMatrix 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## that is stored in x
        imx <- x$getInvMatrix()
        ## if x do  have inverse stored 
        ## they returne without calc
        if(!is.null(imx)) {
            message("getting cached data")
            return(imx)
        }
        
        #else they calculate the inverse
        mat <- x$get()
        ##verify if the matrix has  inverse
        aux <- det(mat)
        ## if matrix has inverse calculate the inverse
        if(aux!=0){
          message("Computing the inverse")
          imx=solve(mat)
          x$setInvMatrix(imx);
        }
        imx;
}

##sample matrix
mat = matrix(c(1,2,3,4),nrow=2, ncol=2,byrow = TRUE)

##passing a list of functions and one enviroment to xx object
xx=makeCacheMatrix(mat);
##xx$get();

##test the solution
cacheSolve(xx);##first to calculate the inverse
cacheSolve(xx);##next to verify retrive from cache

xx$getInvMatrix();





