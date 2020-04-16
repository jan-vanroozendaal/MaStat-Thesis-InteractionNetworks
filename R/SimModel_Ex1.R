# EXAMPLE 1: 10 LATENT VARIABLES, ALL TRIPLETS, NO INTERACTION BETWEEN NODES FROM DIFFERENT TRIPLETS ("NULL MODEL")

# Loading packages
library(lavaan)
library(semPlot)
library(qgraph)
library(dplyr)

#=-=-CASE SELECTION=-=-=-#
#e.covs are now arranged in a vector of size 10
#input of ii_choice in order: redundancy/zero interaction/synergy
ii_choice = c(0.22, -0.15, -0.39)

#CASE - ZERO INTERACTION
#ecov <- rep(-0.15, 10L)

#CASE - REDUNDANCY
#ecov <- rep(0.22, 10L)

#CASE - SYNERGY
ecov <- rep(-0.39, 10L)

#CASE - CUSTOM
#ecov <- c(0.22, -0.15, -0.39, 0.22, -0.15, -0.39, 0.22, -0.15, -0.39, 0.22)

#CASE - RANDOM 
#ecov = sample(ii_choice, 10, replace = TRUE)


#=-=-FUNCTIONS=-=-=-#
#Function 1: calculate II from 3x3 correlation matrix
lav_interaction_information_cor_triplet <- function(triplet.cor = NULL) {
  # mi.xy
  cor.xy <- triplet.cor[2,1]
  mi.xy <- -1/2 * log(1 - (cor.xy*cor.xy))
  
  # mi.xy_z
  mi.xy_z <- as.numeric(NA)
  res.cov <- ( triplet.cor[1:2,1:2] -
                 tcrossprod(triplet.cor[1:2,3]) * (1/triplet.cor[3,3]) )
  if(all(diag(res.cov) > 0)) {
    res.cor <- cov2cor(res.cov)[2,1]
    if(abs(res.cor) < 0.999) {
      mi.xy_z <- -1/2 * log(1 - (res.cor*res.cor))
    }
  }
  
  mi.xy_z - mi.xy
}


#=-=-CREATING MODEL VARIABLES-=-=-=-=-=#
#L and T-values
l1 <- sqrt(0.99)
l2 <- sqrt(0.70)
l3 <- sqrt(0.30)

t1 <- 1 - l1^2
t2 <- 1 - l2^2
t3 <- 1 - l3^2

#alphabet string is prepared, substring function to be in used in for-loop to create variable names for the model
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

#for-loop to create the model components
for(i in 1:length(ecov)) {
  #first, take the appropriate capital letter of the alphabet
  letter = substr(alphabet, i, i)
  
  #temporary variables to store the new variable names in
  #first iteration will create A.ecor, A.l2s, ... , second iteration will create B.ecor, B.l2s, ... , etc.
  nam_ecor <- paste(letter, ".ecor", sep="")
  nam_l2s <- paste(letter, ".l2s", sep="")
  nam_l3s <- paste(letter, ".l3s", sep="")
  nam_t2s <- paste(letter, ".t2s", sep="")
  nam_t3s <- paste(letter, ".t3s", sep="")
  
  #convert the string of the variable names ("A.ecor") to actual variables (A.ecor) with numeric values attached to them
  assign(nam_ecor, ecov[i] / (sqrt(t2) * sqrt(t3)))
  assign(nam_l2s, 1 * sqrt(abs(eval(as.name(paste(nam_ecor))))*t2))
  assign(nam_l3s, sign(eval(as.name(paste(nam_ecor)))) * sqrt(abs(eval(as.name(paste(nam_ecor))))*t3))
  assign(nam_t2s, t2 - abs(eval(as.name(paste(nam_ecor))))*t2)
  assign(nam_t3s, t3 - abs(eval(as.name(paste(nam_ecor))))*t3)
  
  #clear the workspace of 'junk' variables after the last iteration
  if(i == length(ecov)) {
    rm(i, letter, nam_ecor, nam_l2s, nam_l3s, nam_t2s, nam_t3s)
  }
}


#=-=-LAVAAN MODEL SYTNAX-=-=-=-=-=-#
pop.model <- c("

    # model A
    A  =~ (", l1, ")*A.x + (", l2, ")*A.y + (", l3, ")*A.z
    A.bf =~ (", A.l2s, ")*A.y + (", A.l3s, ")*A.z

    A.x ~~ (", t1,  ")*A.x
    A.y ~~ (", A.t2s, ")*A.y 
    A.z ~~ (", A.t3s, ")*A.z

    A    ~~ 1*A
    A.bf ~~ 1*A.bf
    A    ~~ 0*A.bf

    # model B
    B  =~ (", l1, ")*B.x + (", l2, ")*B.y + (", l3, ")*B.z
    B.bf =~ (", B.l2s, ")*B.y + (", B.l3s, ")*B.z

    B.x ~~ (", t1,  ")*B.x
    B.y ~~ (", B.t2s, ")*B.y 
    B.z ~~ (", B.t3s, ")*B.z

    B    ~~ 1*B
    B.bf ~~ 1*B.bf
    B.bf ~~ 0*B

    # model C
    C  =~ (", l1, ")*C.x + (", l2, ")*C.y + (", l3, ")*C.z
    C.bf =~ (", C.l2s, ")*C.y + (", C.l3s, ")*C.z

    C.x ~~ (", t1,  ")*C.x
    C.y ~~ (", C.t2s, ")*C.y 
    C.z ~~ (", C.t3s, ")*C.z

    C    ~~ 1*C
    C.bf ~~ 1*C.bf
    C.bf ~~ 0*C
    
    # model D
    D  =~ (", l1, ")*D.x + (", l2, ")*D.y + (", l3, ")*D.z
    D.bf =~ (", D.l2s, ")*D.y + (", D.l3s, ")*D.z

    D.x ~~ (", t1,  ")*D.x
    D.y ~~ (", D.t2s, ")*D.y 
    D.z ~~ (", D.t3s, ")*D.z

    D    ~~ 1*D
    D.bf ~~ 1*D.bf
    D.bf ~~ 0*D
    
    # model E
    E  =~ (", l1, ")*E.x + (", l2, ")*E.y + (", l3, ")*E.z
    E.bf =~ (", E.l2s, ")*E.y + (", E.l3s, ")*E.z

    E.x ~~ (", t1,  ")*E.x
    E.y ~~ (", E.t2s, ")*E.y 
    E.z ~~ (", E.t3s, ")*E.z

    E    ~~ 1*E
    E.bf ~~ 1*E.bf
    E.bf ~~ 0*E
    
    # model F
    F  =~ (", l1, ")*F.x + (", l2, ")*F.y + (", l3, ")*F.z
    F.bf =~ (", F.l2s, ")*F.y + (", F.l3s, ")*F.z

    F.x ~~ (", t1,  ")*F.x
    F.y ~~ (", F.t2s, ")*F.y 
    F.z ~~ (", F.t3s, ")*F.z

    F    ~~ 1*F
    F.bf ~~ 1*F.bf
    F.bf ~~ 0*F
    
    # model G
    G  =~ (", l1, ")*G.x + (", l2, ")*G.y + (", l3, ")*G.z
    G.bf =~ (", G.l2s, ")*G.y + (", G.l3s, ")*G.z

    G.x ~~ (", t1,  ")*G.x
    G.y ~~ (", G.t2s, ")*G.y 
    G.z ~~ (", G.t3s, ")*G.z

    G    ~~ 1*G
    G.bf ~~ 1*G.bf
    G.bf ~~ 0*G
    
    # model H
    H  =~ (", l1, ")*H.x + (", l2, ")*H.y + (", l3, ")*H.z
    H.bf =~ (", H.l2s, ")*H.y + (", H.l3s, ")*H.z

    H.x ~~ (", t1,  ")*H.x
    H.y ~~ (", H.t2s, ")*H.y 
    H.z ~~ (", H.t3s, ")*H.z

    H    ~~ 1*H
    H.bf ~~ 1*H.bf
    H.bf ~~ 0*H
    
    # model I
    I  =~ (", l1, ")*I.x + (", l2, ")*I.y + (", l3, ")*I.z
    I.bf =~ (", I.l2s, ")*I.y + (", I.l3s, ")*I.z

    I.x ~~ (", t1,  ")*I.x
    I.y ~~ (", I.t2s, ")*I.y 
    I.z ~~ (", I.t3s, ")*I.z

    I    ~~ 1*I
    I.bf ~~ 1*I.bf
    I.bf ~~ 0*I
    
    # model J
    J  =~ (", l1, ")*J.x + (", l2, ")*J.y + (", l3, ")*J.z
    J.bf =~ (", J.l2s, ")*J.y + (", J.l3s, ")*J.z

    J.x ~~ (", t1,  ")*J.x
    J.y ~~ (", J.t2s, ")*J.y 
    J.z ~~ (", J.t3s, ")*J.z

    J    ~~ 1*J
    J.bf ~~ 1*J.bf
    J.bf ~~ 0*J
")

fit <- lavaan(pop.model)
Sigma <- lavInspect(fit, "Sigma")

#visualize lavaan SEM
semPaths(fit)

#understand covariances via summary()
#summary(fit)


#=-=-SIMULATING INTERACTION INFORMATION-=-=-=-=-=-=#
#calculating II per triplet
REP <- 100L #repetitions
N <- 2000L #sample size

#prepare list of vectors
ii_list <- list()

for(i in 1:length(ecov)){
  #create dynamic string for variable name for vector
  nam_ii <- paste("ii",i,sep="")
  #append vector to list
  ii_list[[i]] <- numeric(REP)
}

for(j in seq_len(REP)) {
  #simulate 'REP' times a dataset of size N and find the correlation matrix
  Data <- simulateData(pop.model, sample.nobs = N)
  COR <- cor(Data)
  
  #length of ecov also translates in the number of triplets in the model
  for(i in 1:length(ecov)){
    #for each triplet
    #find index numbers to subset the correlation matrix into the relevant 3x3 matrix (per triplet)
    m_low <- ((i-1) * 3) + 1
    m_high <- m_low + 2
    
    #use function lav_interaction_information_cor_triplet
    #and assign for each iteration of REP the value into the 'dynamic value'
    ii_list[[i]][j] <- lav_interaction_information_cor_triplet(COR[m_low:m_high, m_low:m_high])
  }
}

#preparing dataframe for the output, two columns for scores and description of level of II intended
df <- data.frame(ii_score=as.numeric(10),
                 ii_programmed=character(10),
                 stringsAsFactors = FALSE)

for(i in 1:length(ecov)){
  #add mean II values per triplet
  df$ii_score[i] <- mean(ii_list[[i]])
  
  #add description of programmed intention of level of II
  if (ecov[i] == ii_choice[1]) {
    df$ii_programmed[i] <- "redundancy"
  }
  else if (ecov[i] == ii_choice[2]) {
    df$ii_programmed[i] <- "zero interaction"
  }
  else if (ecov[i] == ii_choice[3]) {
    df$ii_programmed[i] <- "synergy"
  }
  else {
    df$ii_programmed[i] <- "custom"
  }
}

#=-=-QGRAPH VISUALIZATION-=-=-=-#
# generate single dataset
set.seed(100) #reproduce same results
Data1 <- simulateData(pop.model, sample.nobs = 2000)

#checking how qgraph performs
qgraph_glasso <- qgraph(cor(Data1), layout="spring", graph="glasso", sampleSize=3000, cut=0)


#=-=-= CHECK EDGELIST FOR SPURIOUS CORRELATIONS FOUND VIA GLASSO-=-=-=#
#create dataframe to retrieve edgelist
node_from <- qgraph_glasso$Edgelist$from
node_to <- qgraph_glasso$Edgelist$to
weight <- qgraph_glasso$Edgelist$weight
edges <- data.frame(node_from, node_to, weight)


#prepare empty dataframe for correlations within triplets
edges_triplet <- data.frame(node_from=as.integer(),
                         node_to=as.integer(),
                         weight=as.numeric())

#step size of 3, to represent each triplet
for (i in seq(1, max(edges$node_to), 3)) {
  #find only the edges that occur within a triplet
  df_temp <- subset(edges, node_from >= i & node_from <= i+2 & node_to >= i & node_to <= i+2)
  #append these rows to the dataframe 'edges_triplet'
  edges_triplet <- rbind(edges_triplet, df_temp)
}

#dplyr package used for the function 'anti_join'
#returns subset of complete edgelist dataframe minus the ones found within triplets
#these are the spurious correlations
edges_spurious <- anti_join(edges, edges_triplet)

#transform numeric values for nodes in edgelist to actual variable names of model
#sequential order, 1/2/3 are A.x/A.y/A.z, etc.
number <- seq(3*length(ecov))

for(i in 1:length(ecov)) {
  #first, take the appropriate capital letter of the alphabet
  letter <- substr(alphabet, i, i)
  
  #inner loop, create three variables for each iteration
  for (j in 1:3) {
    if (j == 1){
      #create A.x, B.x, etc.
      name <- paste(letter, ".x", sep="")
    }
    else if (j == 2){
      #create A.y, B.y, etc.
      name <- paste(letter, ".y", sep="")
    }
    else {
      #create A.z, B.z, etc.
      name <- paste(letter, ".z", sep="")
    }
    #replace each occurrence of current number (first one in vector) to newly created name
    edges_spurious$node_from[edges_spurious$node_from == number[1]] <- name
    #repeat the same for the 'node_to' column
    edges_spurious$node_to[edges_spurious$node_to == number[1]] <- name
    #delete first value of vector, similar to number += 1 in Python
    number <- number[-1]
  }
  if (length(number) == 0) { #if all iterations are complete
    #remove 'junk' variables from the workspace
    rm(number, i, j, name, letter, alphabet)
  }
}

#overview of simulated interaction information per triplet
print(df)

#significant spurious correlations with variable names
options(scipen=999) #change number formatting of correlations for readability
edges_spurious[abs(edges_spurious$weight) >= 0.02,]
