#data is divided into interval and frequency is being generated on excel 
#frequency of the data and states inputed and computed here 



#install.packages("markovchain")
library(markovchain)

#firstorder 
num_increase <<- 140
num_decrease <<- 96
num_stagnant <<- 15
total_freq <<- sum(num_increase,num_decrease,num_stagnant)
total_freq
increase2increase <<- 78
increase2decrease <<- 50
increase2stagnant <<- 12
decrease2increase <<- 54
decrease2decrease <<- 39
decrease2stagnant <<- 3
stagnant2increase <<- 8
stagnant2decrease <<- 6
stagnant2stagnant <<- 1

trial <- matrix(c( decrease2decrease/num_decrease ,decrease2stagnant/num_decrease ,decrease2increase/num_decrease,
                 stagnant2decrease/num_stagnant ,stagnant2stagnant/num_stagnant,stagnant2increase/num_stagnant , #stagnant
                 increase2decrease/num_increase,increase2stagnant/num_increase,increase2increase/num_increase#decrease
                  ),ncol=3,nrow = 3, byrow = TRUE)
trial

print(trial)
trial1 <- new("markovchain", transitionMatrix = trial, states= c("-1", "0", "1"))
plot(trial1)
#install.packages("diagram")
library("diagram")
plotmat(t(trial), pos=c(1,2), #t(TPM_Q1) is transpose 
        lwd = 1, box.lwd = 2,
        cex.txt = 0.8,
        box.size = 0.1,
        box.type = "circle",
        box.prop = 0.5,
        box.col = "light blue",
        arr.length = .1,
        arr.width = .1,
        self.cex = .6,
        self.shifty = -.01,
        self.shiftx = .14,
        main = "Transition Diagram for change in day")
#confirmation
#initial<- c(1/3, 1/3, 1/3) can be used but not needed inital state should be taken as the day we will start 
#out1 <- initial*trial
#out1
trial
twice <- trial1^2
twice
thrice <- trial1^3
thrice
four <- trial1^4
four
five<-trial1^5
five
six<-trial1^6
six
Steady<-steadyStates(trial1)#in long run what will be the process in each of states
Steady
#in the future the market will be in the way steady states
is.irreducible(trial1)
period(trial1) #interval time 
summary(trial1)
initial_state <- c(num_decrease/total_freq,num_stagnant/total_freq,num_increase/total_freq)
initial_state
prob_6th <- initial_state*(trial1^6)
prob_6th
prob_252 <- initial_state*(trial1)
prob_252

recurrentClasses(trial1)
meanRecurrenceTime(trial1)

#recurrent is another name for persistent 

################################

################################
#2nd order tpm
num_incinc_1 <<- 78
num_decdec_2 <<- 39
num_incdec_3 <<- 50
num_decinc_4 <<- 54
num_stasta_5 <<- 1
num_stainc_6 <<- 8
num_stadec_7 <<- 6
num_incsta_8 <<- 11
num_decsta_9 <<- 3

incinc2dec <<- 24
incinc2sta <<- 5
incinc2inc <<- 49

decdec2dec <<- 18
decdec2sta <<- 0
decdec2inc <<- 21

incdec2dec <<- 19
incdec2sta <<- 3
incdec2inc <<- 28

decinc2dec <<- 24
decinc2sta <<- 6
decinc2inc <<- 24

stasta2dec <<- 0
stasta2sta <<- 0
stasta2inc <<- 1

stainc2dec <<- 2
stainc2sta <<- 1
stainc2inc <<- 5

stadec2dec <<- 2
stadec2sta <<- 0
stadec2inc <<- 4

incsta2dec <<- 5
incsta2sta <<- 1
incsta2inc <<- 5

decsta2dec <<- 1
decsta2sta <<- 0
decsta2inc <<- 2

trial2 <- matrix(c(incinc2dec/num_incinc_1, incinc2sta/num_incinc_1, incinc2inc/num_incinc_1,
                   decdec2dec/num_decdec_2, decdec2sta/num_decdec_2, decdec2inc/num_decdec_2,
                   incdec2dec/num_incdec_3, incdec2sta/num_incdec_3, incdec2inc/num_incdec_3,
                   decinc2dec/num_decinc_4, decinc2sta/num_decinc_4, decinc2inc/num_decinc_4,
                   stasta2dec/num_stasta_5, stasta2sta/num_stasta_5, stasta2inc/num_stasta_5,
                   stainc2dec/num_stainc_6, stainc2sta/num_stainc_6, stainc2inc/num_stainc_6,
                   stadec2dec/num_stadec_7, stadec2sta/num_stadec_7, stadec2inc/num_stadec_7,
                   incsta2dec/num_incsta_8, incsta2sta/num_incsta_8, incsta2inc/num_incsta_8,
                   decsta2dec/num_decsta_9, decsta2sta/num_decsta_9, decsta2inc/num_decsta_9),
                 nrow = 9, byrow = TRUE )
print(trial2)

#for making a tpm of 2nd order we need to take the row name and column name same 
#here the row name is like (1,-1) in this -1 should be seen in the column name (-1,0)
#if it is seen that -1 is common then only the row and column intersection will have probability 
#the above eg means the initial state is increasing and decreasing from that it moves to a stagnent state 
trail3 <- matrix(c(0.6283,0,0.3076, 0, 0, 0, 0, 0.0641, 0,
                   0, 0.4615, 0, 0.5385, 0, 0, 0, 0, 0,
                   0, 0.38, 0, 0.56, 0, 0, 0, 0, 0.06,
                   0.4444,0,  0.444, 0, 0, 0, 0, 0.1116, 0,
                   0,0,0,0,0,1,0,0,0,
                   0.625,0,0.25,0,0,0,0,0.125,0,
                   0,0.3333,0,0.6667,0,0,0,0,0,
                   0,0,0,0,0.090,0.4555,0.4545,0,0,
                   0,0,0,0,0,0.6667,0.3333,0,0),nrow=9,byrow=TRUE)
trail3
p <- new("markovchain",transitionMatrix=trail3,states= c("(1,1)", "(-1,-1)", "(1,-1)", "(-1,1)","(0,0)","(0,1)","(0,-1)","(1,0)","(-1,0)"))
p

plotmat(t(trail3), 
        lwd = 1, box.lwd = 1,
        cex.txt = 1,
        box.size = 0.075,
        box.type = "square",
        box.prop = 0.25,
        box.col = "light blue",
        arr.length = .2,
        arr.width = .05,
        self.cex = .5,
        self.shifty = .01,
        self.shiftx = .114,
        main = "Transition Diagram for change in day")


recurrentClasses(p)
meanRecurrenceTime(p)

twice <- p^2
twice
thrice <- p^3
thrice
four <-p^4
four
five<-p^5
five
six<-p^6
six
seven <- p^7
seven


Steady1<-steadyStates(p)#in long run what will be the process in each of states
Steady1













