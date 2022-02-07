######Misogynistic Enclave Norms Model######
###Purpose: 
#Simulate the formation of "misogynistic enclaves",concentrated groups of sexual harassment perpetrators,
#and change in group norms based on agents moving to groups in order to minimize cost of intervention or harassment.

#Thompson sampling will be used to determine how agents move.

###Agents:
#2000 agents are randomly assigned to 40 initial groups of agents. 
#Later versions of the model will test different variations of group numbers and size. 
#For this reason, the population size, number of groups, and number of agents in each group will each be parameters.

#Each agent has a harassment probability H, which represents their probability of harassing, intervening (1-H) , and retaliating against intervention (H). 
#An H probability from 0-1 will be assigned to each agent using a beta distribution with alpha & beta parameters 10. 
#Conceptually, these values represent scores on the Rape Myth Acceptance Scale.  

#Agents also have an initial perception of each group's harassment norms, which will be a beta distribution (mean, standard deviation) that is the same for each group. This distribution will be updated throughout the model as agents observe behavior in their groups. 

###Life cycle:

##Behave: 
#Harass: Each agent will be given the opportunity to perpetrate harassment. 
#The likelihood that they do so will be probabilistic, based on their H value.

#Intervene: For every harassing agent, all other agents in that agent's group will be given the opportunity to intervene against the harassing agent. 
#The likelihood that they do so will be probabilistic, based on 1-H value.

#Retaliate: Finally, all agents in the group aside from any who intervened will have the opportunity to retaliate against the intervening agents. 
#The likelihood that they do so will be probabilistic, based on their H value.

##Observe: 
#Agents will observe and store any instance in which they are intervened against or retaliated against by other agents in their group.

#Agents will use these observations to update their perception of norms in the groups. 
#If an agent is targeted by another agent in the group (i.e. intervened upon when harassing or retaliated against for intervening), this will be considered a failure,  and the observation will be used to update the focal agents' beta distribution for that group. 
#In that group, the beta distribution will now reflect a lower probability of success.
#If an agent is not targeted by any other agent in the group (i.e. able to harass without intervention or intervene without retaliation), this will be considered a success, and the observation will be used to update the focal agents' beta distribution for that group.  
#In that group, the beta distribution will now reflect a higher probability of success. 

##Move: 
#Agents will draw a random sample from their beta distributions for each group. 
#The agent will move to the group from which the highest value (which represents the greatest likelihood of not being targeted in the group) is selected. 
#They will behave in and observe this group during the next round of the model. 
#(This is essentially Thompson sampling)

##Repeat:
#Agents will repeat the life cycle between 1 - 10,000 times. 

### Analysis Plan

##Group norm variation:
#The change in the distribution of group norms during the course of the model (i.e. are there more extreme groups after the model is run).

##Group similarity:
#Do groups consist of agents who are more similar to each other after the model is run?

##Rate of harassment, intervention, and retaliation
#What are the rates of harassment, intervention, and retaliation, respectively, within groups and overall, before and after the model is run?


######Packages######

######Parameters######
#number of groups
groupnum <- 5

#agents per group
groupsize <- 4

#number of agents
popsize <- (groupnum*groupsize)

#number of oppotunities to move groups 
rounds <- 10


######Functions######

#Agent Generation
agentgenerate<- function(popsize,groupnum){
  #assign each agent a PIN
  PIN <- sample(1:popsize)
  
  #assign each agent a  group 
  group <- sample(rep(1:groupnum, groupsize))
  
  #assign each agent a random H-value between 0 and 1using a normal distribution
  h<-rnorm(popsize,0,1)
  h<-h+abs(min(h))
  h<-h/max(h)
           
  
  #Make data frame to store agents' group, h value, and most recent behavior
  agents<- data.frame(PIN, group, h, "harass" = 0, "intervene" = 0,"retaliate" = 0 )
  
  return(agents)
}

#check how many agents are in each group
#n<-tapply(agents$PIN,agents$group,function(x) length(x))

######Model Start######

#####Agents#####
#Generate agents
agents <- agentgenerate(popsize, groupnum)

#Store agents' initial groups 
agents$initialgroups <- agents$group

#Create a matrix to store agents' alpha [[1]] and beta [[2]] values for the beta distributions of each group's norms 
#With initial beta distributions of alpha = 1 and beta = 1 (flat)
distmatrix <-  list(matrix(data = 1, nrow = popsize, ncol = groupnum),matrix(data = 1, nrow = popsize, ncol = groupnum))

#Create a blank data frame to store the overall behavior of agents in each group
gbehave <- data.frame(1:groupnum, "harass" = 0, "intervene" = 0,"retaliate" = 0 )

#####Life Cycle#####

####Behave####
#For every group...
for(g in groupnum){
  #make a list of group members... 
  groupmem <- which(agents$group==g)
  
  #and generate the behavior of every agent in the group:

  ##Harass  
  for(a in groupmem){
    
    #If a random value is less than or equal to the focal agent's h-value... 
    randval<- runif(1, min=0, max=1)
    
    #the agent's harass number is changed to a value of 1 to represent perpetration of harassment
    if(randval<= agents[a,"h"]){
      agents[a,"harass"] <- 1
      
      #and the sum of harassers in the focal group is increased by 1
      gbehave[g,"harass"] <- gbehave[g,"harass"] + 1 
      
      }
  }
 
  #Make a vector of agents in the group who harassed
  harassers <- which(agents$group==2 & agents$harass==1)
  
  ##Intervene
  for(a in groupmem){
    
    #If the sum of harassers minus the value of self > 0, the focal agent may intervene...
    if((length(harassers)- agents[a, "harass"])>0){
     
      #If a random value is less than or equal to 1 minus the focal agent's h-value... 
      randval<- runif(1, min=0, max=1)
      
      #the agent's intervene number is changed to a value of 1 to represent intervention
      if(randval<= (1- agents[a,"h"])){
        agents[a,"intervene"] <- 1
        
        #and the sum of interveners in the focal group is increased by 1
        gbehave[g,"intervene"] <- gbehave[g,"intervene"] + 1 
        
      }
    }
  }
  
  #Reset the vector of harassers
  harassers <- NA
  
  #Make a vector of agents in the group who intervened
  interveners <- which(agents$group==g & agents$intervene==1)
  
  ##Intervene
  for(a in groupmem){
    
    #If the sum of interveners minus the value of self > 0, the focal agent may retaliate...
    if((length(interveners)- agents[a, "intervene"])>0){
      
      #If a random value is less than or equal to the focal agent's h-value... 
      randval<- runif(1, min=0, max=1)
      
      #the agent's retaliate number is changed to a value of 1 to represent retaliation
      if(randval<= (agents[a,"h"])){
        agents[a,"retaliate"] <- 1
        
        #and the sum of retaliators in the focal group is increased by 1
        gbehave[g,"retaliate"] <- gbehave[g,"retaliate"] + 1 
        
      }
    }
  }
  
  #Reset the vector of interveners
  interveners <- NA
}
 
####Observe####

#If the focal agent harassed...
  if(agents[a,"harass"]==1){
    
    #set their Beta = Beta + # of agents who intervened
    distmatrix[[2]][a,g] <- (distmatrix[[2]][a,g] + gbehave[g,"intervene"])
    
    #and set their Alpha = Alpha + # of agents who did not intervene
    distmatrix[[1]][a,g] <- (distmatrix[[1]][a,g] + (groupsize - gbehave[g,"intervene"]))
    
  }
 
#If the focal agent intervened...
if(agents[a,"intervene"]==1){
  
  #set their Beta = Beta + # of agents who retaliated
  distmatrix[[2]][a,g] <- (distmatrix[[2]][a,g] + gbehave[g,"retaliate"])
  
  #and set their Alpha = Alpha + # of agents who did not retaliate
  distmatrix[[1]][a,g] <- (distmatrix[[1]][a,g] + (groupsize - gbehave[g,"retaliate"]))
  
}


####Move####
#Every agent draws a random sample from their beta distributions for each group. 
for(p in agents){
  
  betavector <- c(0,1,2)#***placeholder vector*** calculate the beta dist & draw a random value from it
  
  #The focal agent will move to the group from which the highest value is selected. 
  #(This represents the greatest likelihood of not being targeted in the group.)
  agents[p,"group"] <- max(betavector)
  
  #The agent will behave in and observe this group during the next round of the model.
}


####scratch paper - ignore this####
agents[2,1]

####Behave####
#For every group...
for(g in groupnum){
  #make a list of group members... 
  groupmem <- which(agents$group==g)
  #and generate the behavior of every agent in the group:
  for(a in groupmem){
    
    ##Harass
    
    #If a random value is less than or equal to the focal agent's h-value... 
    randval<- runif(1, min=0, max=1)
    
    #the agent's behavior is changed to a value of 1 to represent perpetration of harassment
    if(randval<= agents[a,"h"]){
      agents[a,"behavior"] <- 1
      
      #and the total harassment incidents in the group during this round of behavior is increased by 1...
      gbehave[g,"harass"] <- gbehave[g,"harass"] + 1
      
    }
    ##Intervene
    
  }
  
}

#Create a blank data frame to store the sums of behavior types in groups *should be replaced by behavior columns in agents*
gbehave <- data.frame ((matrix(data = 0, nrow = groupnum, ncol = 3)))
colnames(gbehave) <- c("harass", "intervene", "retaliate")

#and the total harassment incidents in the group during this round of behavior is increased by 1...
gbehave[g,"harass"] <- gbehave[g,"harass"] + 1
