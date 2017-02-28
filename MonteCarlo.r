
rm(list=ls(all=TRUE))
fnBirthDay <- function(simulations){
  set.seed(1234)
  yes = 0
  #no =0
  
  
  x=sample(1:7, simulations,replace = TRUE)
  y=sample(1:7, simulations,replace = TRUE)
  
  
  
#   for (j in 1:simulations) {
#     
#     if(x[j]==y[j]){
#       
#       yes=yes+1
#     }
#     
#   }
  
  
  
  # instead of above for loop we can use the below one line code
  yes=sum(x==y)
  
  probability = yes/simulations
  
  cat("probability is : ", probability, " for ",simulations, " simulations ","\n")
  
}



simulations=c(10,100,1000,10000,100000)
#par(mfrow=c(1,3))
start = Sys.time()
for (i in simulations) {
  probability=fnBirthDay(i)
}
end=Sys.time()-start
end

# instead for loop use lapply
start1 = Sys.time()
probability1 = lapply(simulations,fnBirthDay)
end1=Sys.time()-start1
end1

start2 = Sys.time()
probability2 = lapply(simulations,fnBirthDay)
end2=Sys.time()-start2
end2