#NAME : M.D.ISMAIL ZABIULLAH
#REG.NO : 19BDS0084
#SOURCE : https://github.com/mohammedismailz/labda4/blob/main/heartdata.csv
#TOPIC : HEART DISEASE ESTIMATION 
#NOTE : verficiation of output between user defined and pre-defined functions with values is at the end of each section 

# required libraries 
library("base")
library("stats")
library("utils")
library("tidyverse")
library("tidyr")
library("dplyr")
library("naniar")

#obtaining data 
#setwd("")
heart_data <- read.csv("heartdata.csv",header=T, sep=",")
View(heart_data)
str(heart_data)

#visualize NA values
vis_miss(heart_data)

#converting the appropriate columns to factor data type 
heart_data$Sex <- as.factor(heart_data$Sex)
heart_data$ExerciseAngina <- as.factor(heart_data$ExerciseAngina)

#creating column HeartDisease indicating healthy/harmful based 
heart_data$HeartDisease <- as.factor(ifelse(heart_data$Cholesterol>200, "harmful", "healthy"))
str(heart_data)  


#removing columns with >60% missing data => <40% actual data
heart_data<-heart_data[,which(colMeans(!is.na(heart_data)) > 0.4)]
head(heart_data)
#removing rows with >60% missing data => <40% actual data
heart_data<-heart_data[which(rowMeans(!is.na(heart_data)) > 0.4),]
summary(heart_data)
view(heart_data)

#mean/median impudiation:
heart_data[heart_data==0]<-NA
heart_data$Cholesterol[which(is.na(heart_data$Cholesterol))] <- mean(heart_data$Cholesterol,na.rm = TRUE)
#view(heart_data)


#MEAN: 
 vec1 <- heart_data$Cholesterol
 #vec1
  summ = 0
  for (j in vec1){
    summ = summ+j
    }
  meann = summ/length(vec1)
  #meann=244.635
  #mean(vec1)=244.635
  
#MEDIAN:
vec2 <- heart_data$Cholesterol
sorting <- function(x){
  for(i in 1:(length(x)-1)){
    for(j in (i+1):length(x)){
      if(x[i] > x[j]){
        x[c(i,j)] = x[c(j,i)] # +1 for this
      }
    }
  }
  x
}
sorting(vec2)
y <- sorting(vec2)
if((length(y)%%2)!=0){
  mediann = y[length(y)/2]
} else{
  mediann = (y[(length(y)+2)/2]+y[length(y)/2])/2
}
#mediann=244.6354
#median(vec2)=244.6354

#MODE:
modeee<-function(z){
  p<-table(z)
  counts<-p[max(p)==p]
  repeats<-as.numeric(counts[1])
  val<-as.numeric(names(counts))
  op<-list(val=val,freqq=repeats)
  return(op)
  }
#modeee(y) = 244.6354
#which(table(y) == max(table(y)))=244.6354

#INTERQUARTILE RANGE(IQR):
iqrrr <- function(y){
  if(length(y)%%2==0){
    aa = y[1:length(y)/2]
    bb = y[((length(y)+2)/2) : length(y)]
    iqrr = (bb[(length(bb)+2)/2]+bb[length(bb)/2])/2 - (aa[(length(aa)+2)/2]+aa[length(aa)/2])/2 
    } else{
      aa = y[1:(length(y)-1)/2]
      bb = y[(length(y)+3)/2 : length(y)]
      iqrr = bb[length(bb)/2] - aa[length(aa)/2]
    }
  iqrr
}
#IQR(y) = 53
#iqrrr(y) = 53

#STANDARD DEVIATION:
sddd = sqrt(sum((y-mean(y))^2/(length(y)-1)))
#sddd = 53.31803
#sd(y) = 53.31803

#EMPIRICAL RULE ANALYSIS: 
#To find the number of observations within ±1 standard deviation of the mean we use
firstemp = sum(heart_data$Cholesterol>meann-sddd & heart_data$Cholesterol<meann+sddd)
secondemp = sum(heart_data$Cholesterol>meann-(2*sddd) & heart_data$Cholesterol<meann+(2*sddd))
thirdemp = sum(heart_data$Cholesterol>meann-(3*sddd) & heart_data$Cholesterol<meann+(3*sddd))
p1 = firstemp/length(heart_data$Cholesterol)
p2 = secondemp/length(heart_data$Cholesterol)
p3 = thirdemp/length(heart_data$Cholesterol)
#p1=0.7636166
#p2=0.9575163
#p3=0.9869281
#This means that the data set eruptions DOES NOT satisfy theEmpirical rule, which in turn means that the data set is NOT normally distributed.

#Plot the Graph/Histogram/Normal Distribution
t <- rnorm(heart_data$Cholesterol)
#t
qnorm(p=0.5,meann,sddd)
pnorm(244.6354,meann,sddd)
curve(dnorm(x, mean=mean(y), sd=sd(y)))
hist(y,probability = T)

