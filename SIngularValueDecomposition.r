# Rows are  users (5 users) and # columns are songs (6 songs)
N=matrix(c(5,0,3,4,3,2,
           2,1,4,0,0,5,
           1,1,1,0,2,1,
           1,0,0,0,5,5,
           4,3,2,3,4,1),byrow=T,ncol=6)
rownames(N)= c("User1","User2","User3","User4","User5")
colnames(N) = c("Song1","Song2","Song3","Song4","Song5","Song6")

# UserId = c("User1","User2","User3","User4","User5")
# N= cbind(UserId,N)
rownames(N)= c("User1","User2","User3","User4","User5")
Users = c(1,2,3,4,5)
colnames(N) = c("Song1","Song2","Song3","Song4","Song5","Song6")
songs = colnames(N)
# Compute the Singular-value decomposition

svd<-svd(N)
S <- diag(svd$d)
u <- svd$u
v <- svd$v
vt = t(v)

N_hat = u %*% S %*% vt

eigenval = svd$d
e_sqare_energy = (eigenval/sum(eigenval))*100
cumsum(e_sqare_energy)


# with first 3 values itself, almost 90% is covered hence, 3 dimensions are enough
svd <- svd(N,nu=3,nv=3)
S <- diag(svd$d[1:3])
svd$u %*% S %*% t(svd$v) 

SVDresultusers = svd$u
SVDresultsongs = svd$v

#New user ratings  0,0,2,0,4,1
Newuserratings = c(0,0,2,0,4,1)

# ratings should be multipled with songs space matrix
# A = us(t(v))
# Av = us (t(v))(v)   & (t(v))(v) = I  
#  =>  Av = us(I) 
#  =>  Av = us
# => AV(inverse of S) = u, new user's user space = newuserrmatrix * svd$v * inverse of s
b <-matrix(Newuserratings,byrow=T, ncol=6)
S1 <- diag(1/svd$d[1:3])
# building a decomposition of new user ratings
newuserrating = b %*% svd$v %*% S1   



### correlation

# Creating function to calculate the cosine between two vectors
getCosine <- function(x,y) 
{
  cosine <- abs(sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y))))
  return(cosine)
}

#user based recommendation

distances = apply(svd$u,1,getCosine,newuserrating) 
userid = which((distances==max(distances))==TRUE)
similaruser = N[userid,]
Newuserratings
zeroposofNewUser = which((Newuserratings==0)==TRUE)
recom = which(similaruser ==max(similaruser[zeroposofNewUser]))
recom
# Example : recom = c("song1"=1,"song6" = 6)

if(length(recom)>1){
 # do popular movie/song
 
  moviewiseavgratings = apply(N[,recom],2,median)
  pos = which((moviewiseavgratings == max(moviewiseavgratings))==TRUE)
  Recommendation = names(recom[pos])
  
}else{
  Recommendation = names(recom[1])
  }
print(Recommendation)