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
#number of agents
popsize <- 2000

#number of groups
groupnum <- 50

#number of oppotunities to move groups 
rounds <- 10

######Functions######

#Agent Generation
agentgenerate<- function(popsize,groupnum){
  #assign each agent a  group 
  group <- sample(c(1:groupnum),popsize,replace = T)
  
  #assign each agent a random H-value using a beta distribution with alpha & beta parameters 10
  #****are these small values right?****
  h <- rbeta(popsize,1,10, ncp = 0)
  
  #Make data frame to store agents' group, h value, and most recent behavior
  agents<- data.frame(group, h, "behavior" = 0)
  
  return(agents)
}


######Model Start######

#####Agents#####
#Generate agents
agents <- agentgenerate(popsize, groupnum)

#Store agents' initial groups 
agents$initialgroups <- agents$group

#Set initial beta distributions of mean = 0 and variance = 1 (normal distribution)

#Create a matrix to store agents' means (initially 0) for the beta distributions of each group's norms
betameans <- matrix(data = 0, nrow = popsize, ncol = groupnum)

#Create a matrix to store agents' variance (initially 1) for the beta distributions of each group's norms
betavar <- matrix(data = 1, nrow = popsize, ncol = groupnum)

#***is there a more efficient/better way to do this?^

#Create a blank data frame to store the sums of behavior types in groups
gbehave <- data.frame ((matrix(data = 0, nrow = groupnum, ncol = 3)))
colnames(gbehave) <- c("harass", "intervene", "retaliate")

#####Life Cycle#####

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
      
      #and all other agents in the group are given the opportunity to intervene.
      
      ##Intervene
      
    }
  }
  
}


####scratch paper - ignore this####
agents[2,1]

