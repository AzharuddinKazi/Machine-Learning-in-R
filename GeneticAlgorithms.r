rm(list=ls(all=TRUE))

dataset <- data.frame(item = c("pocketknife", "beans", "potatoes", "onions","phone", "lemons",
                               "sleeping bag", "rope", "compass","umbrella","sweater","medicines","others"), 
                      survivalpoints = c(15, 16, 13, 14, 20,12,17,18,17,19,10,12,11), 
                      weight = c( 5, 6, 3, 4,11,2,7,8, 10,9,1,12,11))
sum(dataset$survivalpoints) 
sum(dataset$weight)
      
weightlimit <- 80
#MaxPossiblesurvivalpoints = sum(dataset$survivalpoints)
#stopping criteria could be either maxpossiblesolution or 
#if there is no much change for last n iteerations you may stop

# Chromosome = c(1, 0, 0, 1, 1, 0, 0)
# gene 0 or 1

# Initital population generation

fnGenerateInitPop <- function(dataset){
  InitPopSize = 100
  initPop = as.data.frame(setNames(replicate(nrow(dataset),numeric(0), simplify = F), dataset$item))
  set.seed(1234)
  seeds = sample(6000:7000,InitPopSize,replace=FALSE)
  for ( i in 1:InitPopSize){
    
    set.seed(seeds[i])
    chromosome = sample(0:1,nrow(dataset),replace=TRUE)
    initPop[i,]= chromosome
    
  }
  
  return(initPop)
}

#We define the Objective function as follows.

fnEvaluate <- function(x){
  current_solution_survivalpoints <- x %*% dataset$survivalpoints
  current_solution_weight <- x %*% dataset$weight
  
  if (current_solution_weight > weightlimit) 
    return(0) else return(current_solution_survivalpoints)
}

# mutation : pick one position and change the value to 0 or 1 as the case may be
fnMutate<-function(individual){
  #change one value to the other value 1 to 0 or o0 to 1 
  
  a=sample(1:length(individual),1)
  individual[a]=1-individual[a]
  return(individual)
}

# Crossover : randomly select a point and swap the tails
fnCrossOver=function(p1,p2){
  a=sample(2:(length(p1)-2),1)
  p11 = c(p1[1:a],p2[(a+1) : length(p2)])
  p12 = c(p2[1:a],p1[(a+1) : length(p1)])
  
  return(list(p11,p12))
}

#  Execute the genetic algorithm
fnRunGeneticAlgo<- function(initPop, mutstartProb,elitepercent, maxiterations,MaxPossiblesurvivalpoints){
  
  counter=0   # is used for stopping criteria
  cat("max iterations =", maxiterations, "\n")
  # How many winners from each generation?
  
  origPopSize=nrow(initPop)
  topElite=round(elitepercent*origPopSize,0)
  fitN=apply(initPop,1,fnEvaluate)
  initPop=data.frame(initPop,fitN)
  #initPop=initPop[sort.list(initPop[,5]), ]
  initPop = initPop[order(-initPop$fitN),]
  # Main loop
  NewPop = initPop
  for (i in 1:maxiterations) {
    cat("i value is :",i,"\n")
    NewPop = NewPop[order(-NewPop$fitN),]
    ElitePop=NewPop[1:topElite,]
    
    NewPop = NewPop[,-c(length(colnames(NewPop)))]
    NewPop = NewPop[-(1:nrow(NewPop)),1:nrow(dataset)]
    mut=mutstartProb/i
    
    # Add mutated and bred forms of the winners
    while (nrow(NewPop)<origPopSize) {
      # Mutation
      if (runif(1,0,1)<mut) {
        c=sample(1:topElite,1)
        NewPop[nrow(NewPop)+1,]=fnMutate(ElitePop[c,1:nrow(dataset)])
        if (nrow(NewPop)==origPopSize){break()}
      }
      
      # Crossover
      else {
        c1=sample(1:topElite,1)
        c2=sample(1:topElite,1)
        ls = (fnCrossOver(ElitePop[c1,1:nrow(dataset)], ElitePop[c2,1:nrow(dataset)]))
        NewPop[nrow(NewPop)+1,]=ls[[1]]
        NewPop[nrow(NewPop)+1,]=ls[[2]]
        if (nrow(NewPop)==origPopSize){break()}
      }
      
    }
    NewPop$fitN=apply(NewPop,1,fnEvaluate)
    NewPop = NewPop[order(-NewPop$fitN),]
    
    
    
    # stopping criteria 
    if(NewPop[1,(nrow(dataset)+1)]==ElitePop[1,(nrow(dataset)+1)]){
      counter=counter+1
      if(counter==5){break()}
    }else{
      counter=0
    }
    #if (NewPop[1,(nrow(dataset)+1)] == MaxPossiblesurvivalpoints) {break()}
    cat("Total survival points in iteration", i, " = ", NewPop[1,(nrow(dataset)+1)], "\n")
    
  }
  # Print current best score
  
  cat("Total survival points in iteration", i, " = ", NewPop[1,(nrow(dataset)+1)], "\n")
  return(NewPop[1,])
}


fnExecuteMain <- function(dataset, mutstartProb,elitepercent, maxiterations,MaxPossiblesurvivalpoints){
  
  
  initPop = fnGenerateInitPop(dataset)
  solution = fnRunGeneticAlgo(initPop, mutstartProb,elitepercent,maxiterations,MaxPossiblesurvivalpoints)
  
  Finalsolution = as.numeric(solution[1,1:nrow(dataset)])
  selecteditems = dataset[Finalsolution == 1, ]
  
  # solution vs available
  cat(paste(Finalsolution %*% dataset$survivalpoints, "/", sum(dataset$survivalpoints),"\n"))
  
  cat("Total Survivalpoints = ",sum(selecteditems$survivalpoints),"\n", "Total weight = ",sum(selecteditems$weight))
  return(selecteditems)
  
}

mutstartProb=0.5
elitepercent=0.2
maxiterations=10
Result = fnExecuteMain(dataset, mutstartProb=0.5,elitepercent=0.2, maxiterations=10,
MaxPossiblesurvivalpoints)