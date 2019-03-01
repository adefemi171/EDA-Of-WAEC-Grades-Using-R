#NAME: AFUWAPE ADEFEMI MCIHEAL
#MATRIC NO: 206363
#Department: Computer SCience Department
#---------------------------------------------------------------------------------------
#Importing libraries to use in the code
library(psych)
library(pwr)

data = read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)
options(max.print=999999)

data[4:16]
na.omit(data[4:16])
total_Points = na.omit(data[,14])
total_Points


head(total_Points)
str(total_Points)

data_Summary = summary(total_Points)
data_Summary

data_Sum_Psych = describe(total_Points)
data_Sum_Psych


mean_Total_Points = mean(total_Points)
mean_Total_Points

median_Total_Points = median(total_Points)
median_Total_Points


var_Total_Points = var(total_Points)
var_Total_Points

std_Total_Points = sqrt(var_Total_Points)
std_Total_Points


#Plotting Graphs for Data Visualization
#Histogram Graph
hist(total_Points, main="Total points grade", freq = FALSE, xlab = "Grades", col = "blue", border = "black")
#Boxplot Graph, this shows the 25th, 50th, and 75th percentile
boxplot(total_Points)
#Scatter Plot Graph
#I need to convert total_Points to data frame to get a frequency table
freq_table_df = as.data.frame(table(total_Points))
freq_table_df

plot(freq_table_df, xlab = "Grades", ylab = "Frequency", ylim = c(0, 20), las = 1, main = "Frequency against Total points")
#plot(total_Points)

#---------------------------------------------------------------------------------------
#Defining Function for Left Tail Test
#Left tail test
#Where H0: mu >= mu0
#Where H1: mu < mu0
zTestLeft = function(data, mu0, var, alpha)
{
  zCal = (mean(data) - mu0) / (sqrt(var/length(data)))
  zCritical = qnorm(alpha) * (-1)
  
  if (zCal <= zCritical)
  {
    print ("Reject HO---No sufficient evidence to Accept Null Hypothesis")
  }
  else
  {
    print ("Accept HO--- There's is strong evidence to Accept the Null hypothesis")
  }
  print ('Z statistic')
  print(zCal)
  print ('Z critical value')
  print (zCritical)

  return(zCal)
}

#-------------------------------------------------------------------------------------------
#Defining function for Right tail test
#Right tail test
#Where H0: mu <= mu0
#Where H1: mu > mu0
zTestRight = function(data, mu0, var, alpha)
{
  zCal = (mean(data) - mu0) / (sqrt(var / length(data)))
  zCritical = qnorm(alpha)
  
  if(zCal >= zCritical)
  {
    print ("Reject HO---No sufficient evidence to Accept Null Hypothesis")
  }
  else
  {
    print("Accept HO--- There's is strong evidence to Accept the Null hypothesis")
  }
  print ('Z statistic')
  print(zCal)
  print ('Z critical value')
  print (zCritical)


  return(zCal)
}

#------------------------------------------------------------------------------------------
#Defining function for Two tail test
#Two tail z test
#Where H0: mu = mu0
#Where H1: mu != mu0
zTestTwoTails = function(data, mu0, var, alpha)
{
  zCal = ((mean(data) - mu0)) / (sqrt(var / length(data)))
  zCritical = qnorm(alpha/2) * c(-1,1)
  #print(zCritical)
  
  if(zCal < zCritical[1] || zCal > zCritical[2])
  {
    print("Reject HO---No sufficient evidence to Accept Null Hypothesis")
  }
  else
  {
    print("Accept HO--- There's is strong evidence to Accept the Null hypothesis")
  }
  
  print ('Z statistic')
  print(zCal)
  print ('Z critical values')
  print (zCritical)
  
  return(zCal)
}


#----------------------------------------------------------------------------------------------------
#Calling the function on my variables
#function(data, mu0, var,alpha)
#Given the Confidence Interval as 90,95 and 99
#----------------------------------------------------------------------------------------------------
#Calculating Left tail test using 25 as mu0
zTestLeft(total_Points,25,var_Total_Points,0.01)
zTestLeft(total_Points,25,var_Total_Points,0.05)
zTestLeft(total_Points,25,var_Total_Points,0.10)
#-----------------------------------------------------------------------------------------------------
#Calculating Right tail test using 22 as mu0
zTestRight(total_Points,22,var_Total_Points,0.01)
zTestRight(total_Points,22,var_Total_Points,0.05)
zTestRight(total_Points,22,var_Total_Points,0.10)
#-----------------------------------------------------------------------------------------------------
#Calculating Two tail test using 20 as mu0
zTestTwoTails(total_Points,20,var_Total_Points,0.01)
zTestTwoTails(total_Points,20,var_Total_Points,0.05)
zTestTwoTails(total_Points,20,var_Total_Points,0.10)

#---------------------------------------------------------------------------------------
#Function for Testing for pValue

#---------------------------------------------------------------------------------------
#Defining P-Value for Left tail test using pnorm(zCalculated)
#If pValue < alpha we reject null hypothesis

pValueLeft = function(data, mu0, var, alpha)
{
  zCal = (mean(data) - mu0) / (sqrt(var/length(data)))
  pValue = pnorm(zCal)
  if (pValue < alpha)
  {
    print ("Reject HO, The Test is therefore Significant")
  }
  else
  {
    print ("Accept HO, The test is not significant")
  }
  print ('P value')
  print (pValue)
  
  return(pValue)
}

#---------------------------------------------------------------------------------------
#Defining P-Value for Right tail test  using 1-pnorm(zCalculated)
#If pValue < alpha we reject null hypothesis

pValueRight = function(data, mu0, var, alpha)
{
  zCal = (mean(data) - mu0) / (sqrt(var / length(data)))
  pValue = 1 - pnorm(zCal)
  if(pValue < alpha)
  {
    print ("Reject H0, The Test is therefore Significant")
  }
  else
  {
    print("Accept H0, The Test is not Significant")
  }
  
  print ('P value')
  print (pValue)
  
  return(pValue)
}

#---------------------------------------------------------------------------------------
#Defining P-Value for Two tail test  using 2 * (1-pnorm(zCalculated))
#If pValue < alpha we reject null hypothesis

pValueTwoTails = function(data, mu0, var, alpha)
{
  zCal = ((mean(data) - mu0)) / (sqrt(var / length(data)))
  pValue = 2 * (1 - pnorm(zCal))
  if(pValue < alpha)
  {
    print("Reject H0, The Test is therefore Significant")
  }
  else
  {
    print("Accept H0, The Test is not Significant")
  }
  
  
  print ('P value')
  print (pValue)
  
  return(pValue)
}


#----------------------------------------------------------------------------------------------------
#Calling the function on my variables
#Given the Confidence Interval as 90,95 and 99
#----------------------------------------------------------------------------------------------------
pValueLeft(total_Points,25,var_Total_Points,0.01)
pValueLeft(total_Points,25,var_Total_Points,0.05)
pValueLeft(total_Points,25,var_Total_Points,0.10)
#----------------------------------------------------------------------------------------------------
pValueRight(total_Points,22,var_Total_Points,0.01)
pValueRight(total_Points,22,var_Total_Points,0.05)
pValueRight(total_Points,22,var_Total_Points,0.10)
#----------------------------------------------------------------------------------------------------
pValueTwoTails(total_Points,20,var_Total_Points,0.01)
pValueTwoTails(total_Points,20,var_Total_Points,0.05)
pValueTwoTails(total_Points,20,var_Total_Points,0.10)


#---------------------------------------------------------------------------------------------------------------------------
#Calculating for Proportion
proportion = na.omit(data[,17])
proportion

mean_Prop = mean(proportion)
mean_Prop

#---------------------------------------------------------------------------------------
#Defining Proportion Function for Left Tail Test
#Left Tail Test for Proportion

propTestLeft = function(p0,pMean,n,alpha)
{
  zCalculated = pMean - p0 / sqrt((p0 * (1 - p0))/n)
  zCritical = qnorm(alpha) * (-1)
  if ( zCalculated <= zCritical){
    print("Reject HO---No sufficient evidence to Accept Null Hypothesis")    
  }
  else{
    print ("Accept HO--- There's is strong evidence to Accept the Null hypothesis")
  }
  
  print ("Z Critical")
  print(zCritical)
  
  print ("Z Statistic")
  zCalculated
  return(zCalculated)
}

#---------------------------------------------------------------------------------------
#Defining Proportion Function for Right Tail Test
#Right Tail Test for Proportion

propTestRight = function(p0,pMean,n,alpha)
{
  zCalculated = pMean - p0 / sqrt((p0 * (1 - p0))/n)
  zCritical = qnorm(alpha)
  if ( zCalculated >= zCritical){
    print("Reject HO---No sufficient evidence to Accept Null Hypothesis")    
  }
  else{
    print ("Accept HO--- There's is strong evidence to Accept the Null hypothesis")
  }

  print ("Z Critical")
  print(zCritical)
  
  print ("Z Statistic")
  zCalculated
  return(zCalculated)
}

#---------------------------------------------------------------------------------------
#Defining Proportion Function for Two Tail Test
#Two Tail Test for Proportion

propTestTwoTail = function(p0,pMean,n,alpha)
{
  zCalculated = pMean - p0 / sqrt((p0 * (1 - p0))/n)
  zCritical = qnorm(alpha) * c(-1, 1)
  if ( zCalculated <= zCritical[1] || zCalculated >=zCritical[2]){
    print("Reject HO---No sufficient evidence to Accept Null Hypothesis")    
  }
  else{
    print ("Accept HO--- There's is strong evidence to Accept the Null hypothesis")
  }
  
  print ("Z Critical")
  print(zCritical)
  
  print ("Z Statistic")
  zCalculated
  return(zCalculated)
  
}


#----------------------------------------------------------------------------------------------------
#Calling the function on my variables
#function(p0, proportionMean, sample size(5*126),alpha)
#Given the Confidence Interval as 90,95 and 99
#----------------------------------------------------------------------------------------------------

#Calculating Left tail test using 0.5 as p0
#----------------------------------------------------------------------------------------------------
propTestLeft(0.5, mean_Prop, 126, 0.01)
propTestLeft(0.5, mean_Prop, 126, 0.1)
propTestLeft(0.5, mean_Prop, 126, 0.05)

#Calculating Right tail test using 0.5 as p0
#----------------------------------------------------------------------------------------------------
propTestRight(0.5, mean_Prop, 126, 0.01)
propTestRight(0.5, mean_Prop, 126, 0.1)
propTestRight(0.5, mean_Prop, 126, 0.05)
#Calculating Right tail test using 0.5 as p0
#----------------------------------------------------------------------------------------------------
propTestTwoTail(0.5, mean_Prop, 126, 0.01)
propTestTwoTail(0.5, mean_Prop, 126, 0.1)
propTestTwoTail(0.5, mean_Prop, 126, 0.05)



#---------------------------------------------------------------------------------------------------------------------------
#Calculating for the Power of Test

#Calculating Power for when mu = 20 and muA = 20.77778
n = 126
diff_Mu_20 = (mean_Total_Points - 20) / std_Total_Points
diff_Mu_20
#When significance level = 0.01 when alpha = 99%
pwr.norm.test(d = diff_Mu_20, sig.level = 0.01, n = n, alternative = "two.sided")

#When significance level = 0.05 when alpha = 95%
pwr.norm.test(d = diff_Mu_20, sig.level = 0.05, n = n, alternative = "two.sided")

#When significance level = 0.10 when alpha = 90%
pwr.norm.test(d = diff_Mu_20, sig.level = 0.1, n = n, alternative = "two.sided")


diff_Mu_22 = (mean_Total_Points - 22) / std_Total_Points
diff_Mu_22
#When significance level = 0.01 when alpha = 99%
pwr.norm.test(d = diff_Mu_22, sig.level = 0.01, n = n, alternative = "less")

#When significance level = 0.05 when alpha = 95%
pwr.norm.test(d = diff_Mu_22, sig.level = 0.05, n = n, alternative = "less")

#When significance level = 0.10 when alpha = 90%
pwr.norm.test(d = diff_Mu_22, sig.level = 0.1, n = n, alternative = "less")

diff_Mu_25 = (mean_Total_Points - 25) / std_Total_Points
diff_Mu_25
#When significance level = 0.01 when alpha = 99%
pwr.norm.test(d = diff_Mu_25, sig.level = 0.01, n = n, alternative = "less")

#When significance level = 0.05 when alpha = 95%
pwr.norm.test(d = diff_Mu_25, sig.level = 0.05, n = n, alternative = "less")

#When significance level = 0.10 when alpha = 90%
pwr.norm.test(d = diff_Mu_25, sig.level = 0.1, n = n, alternative = "less")