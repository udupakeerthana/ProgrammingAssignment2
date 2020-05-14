#entering the values and creating a matrix
MakeCachematrix<-function(dimension){
  y=matrix(NA,dimension,dimension)
  l=dimension*dimension
  cat("enter the",l,"numbers by clicking enter after every inputs")
  for (i in 1:dimension) {
  for (j in 1:dimension) {
    y[i,j]=as.numeric(readline(" "))
  }  
  }
  y
}
Cachematrix<-MakeCachematrix(dimension)


#Function for inverse
CacheSolve<-function(Cachematrix,dimension){
  #finding the determinant of given matrix
  det<-det(Cachematrix)
  #Finding the minor of each eliments
  minor<-function(Cachematrix,i,j){
    Cachematrix[-i,-j]
  }
  #Creating a cofactor matrix
  Cof<-matrix(NA,dimension,dimension)
  for (i in 1:dimension) {
    for (j in 1:dimension) {
      Cof[i,j]<-(-1)^(i+j)*minor(Cachematrix,i,j)
    }
  }
  Cof
  #Finding the inverse
  (1/det)*t(Cof)
}
CacheSolve(Cachematrix,dimension)

