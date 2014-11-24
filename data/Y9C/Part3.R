#install.packages("plm")
library(plm)

# In this part, we create table with city, year, and Y9C_masterregate lending

Y9C_example <- read.table(".//Y9C//bhcf0109.txt", sep="^", nrows=-1, comment.char="", header=TRUE, quote="", na.strings="--------", as.is=TRUE)
QCEW_example <- read.table(".//QCEW//2010.q1.by_size.csv", sep=",",header=T)
bhcfnames <- read.table(".//Y9C//bhcfnames.csv", sep=",", fill=T, header=T)
bhcfnames <- data.frame(lapply(bhcfnames, as.character), stringsAsFactors=FALSE)

# basic codes
Y9C_example[,c("BHCP2200")]   # total deposits
Y9C_example[,c("BHCK2122")]   # total loans
Y9C_example[,c("RSSD9001")]   # company code
Y9C_example[,c("BHCK2170")]   # total assets


# Y9C_masterregate total assets (deprecated)
# Just a basic test to find a ticker for total assets
Y9C_example$ATOT <- rep(NA, length(Y9C_example[,c("RSSD9001")]))
v <- c(NA)
count <- 1
temp <- NA
for (i in colnames(Y9C_example)) {
  if (substr(i,5,8)=="2170") {
    v[count] <- i
    count = count + 1
    temp <- cbind(temp, Y9C_example[,i])
  }
}
temp
v


######################## Y9C ###################################

# Initialize the master matrix. (co, name, time, city, loan, atot)
master = matrix(data=NA, nrow=1,ncol=6)
colnames(master) = c('co', 'name', 'time','city','loan','atot')
master <- as.data.frame(master)

#first quarters of multiple years
rowcount = 1
for (yr in 2001:2011) {
  yr_idx = paste0(yr) #make it into string
  yr_idx = substr(yr_idx, start=3, stop=4) # 1994 -> "94"
  filename = paste0(".//Y9C//bhcf", yr_idx, "12.txt")  #get the first quarter
  Y9C_data <- read.table(filename, sep="^", nrows=-1, comment.char="", header=TRUE, quote="", na.strings="--------", as.is=TRUE)
  # some outdated data have column names in smallcap
  colnames(Y9C_data)[colnames(Y9C_data)=="rssd9001"] <- "RSSD9001"
  
  # for each company in year YYYY
  for (i in 1:length(Y9C_data$RSSD9001)) {
    
    # total assets
    total.assets = Y9C_data$BHCK2170[i]
    
    # company code
    co = Y9C_data$RSSD9001[i]
    if (is.na(co))
      next
    
    # loan amount
    loan = Y9C_data$BHCK2122[i] 
    if(is.na(loan))
      next
    
    logical_array = (bhcfnames$code == co)
    
    # company name
    co_name = bhcfnames$name[ logical_array  ][1]
    if (is.na(co_name))
      next
    
    # city
    city_str = bhcfnames$state[ logical_array  ][1]
    if (is.na(city_str))
      next
    
    
    master[rowcount,] = c(co,co_name,yr,city_str,loan,total.assets)
    rowcount <- rowcount + 1
  }
}
master$loan = as.numeric(master$loan) # since everything is stored as character..
# Output : Table of (Company id, Company name, Year, City, Loan, Total Assets)

  
#check the output
write.csv(master, file = "Y9C(co_id, co_name, time, city, loan, atot).csv", append = FALSE, quote = TRUE, sep = ",",
          eol = "\n", na = "NA", row.names = TRUE,
          col.names = TRUE, qmethod = c("double"))



############ Y9C Step Two ########################

# Todo: sum over different companies with same city (control variable: Total Assets)
# Desired output : (Year, City, Loan)


# get bottom 10 percentile of ATOT
# FIX THIS LATER TO GET THE 10% PERCENTILE FOR EACH YEAR, NOT OVER THE ENTIRE 10-YEAR PERIOD
master$atot = as.numeric(master$atot)
atot_cutoff = rep(NA, length(2001:2011))
yr_idx = 2000
for (i in 2001:2011) {
  atot_cutoff[i-yr_idx] = quantile(master$atot[master$time==i], c(0.1,0.5,0.9))[1] # the 10% percentile cutoff
}


#initialize the table for the Desired Output (Year, City, Loan)
Y9C_master = as.data.frame(matrix(NA, ncol=3, nrow=1))
Y9C_master[1,]= c(2001,"NEW YORK", 0)
colnames(Y9C_master) = c("time", "city", "loan")
Y9C_master$loan = as.numeric(Y9C_master$loan)

rowcount = 2
for (i in 1:nrow(master)) {
  yr = as.numeric(master$time[i])
  
  # ATOT control
  if (master$atot[i] > atot_cutoff[yr-yr_idx]) 
    next
  
  city_str = master$city[i]
  loan_int = as.numeric(master$loan[i])
  
  logic_array = (Y9C_master$city == city_str & Y9C_master$time == yr)
  
  if ( nrow( Y9C_master[logic_array,]  ) > 1  ) {
    print("something is wrong")
  }
  if ( nrow( Y9C_master[logic_array,]  ) > 0  ) {
    Y9C_master[logic_array, "loan"] = Y9C_master[logic_array, "loan"] + loan_int
  }
  else {
    # we do this so that the column doesn't get converted from int to char
    Y9C_master[rowcount,"time"] = yr
    Y9C_master[rowcount,"city"] = city_str
    Y9C_master[rowcount,"loan"] = loan_int
    rowcount = rowcount+1
  }
}
#check the output
write.csv(Y9C_master, file = "Y9C(year, city, loan).csv", append = FALSE, quote = TRUE, sep = ",",
          eol = "\n", na = "NA", row.names = TRUE,
          col.names = TRUE, qmethod = c("double"))

# At this point, we have a giant table of (Year, City, Loan)




########################### QCEW #########################

# Final goal: Create a table with Y(ear, MSA, Employment)

# Part 1
# Goal: Iterate through the MSA files and create a giant table
# Input: Individual MSA files. Sum over the first few rows to get the Y9C_masterregate employment for the corresponding MSA

#create a master table (Year, MSA, Employment)

#for each yearly directory
#  read MSA file into a temp table
#  sum the first few rows (i.e. sum over the industry-wide rows)
#  create a row 

QCEW_master = as.data.frame(matrix(NA,nrow=1,ncol=3))
colnames(QCEW_master) = c("time", "MSA", "employment")
rowcount = 1


# We have multiple directory assigned to their year
for (yr in 2002:2012) {
 
  dir_path = paste0(".//QCEW//by_area//",yr,".q1-q4.by_area//")
  
  # list of individual MSA files in the yearly directory
  file_list = list.files(path=dir_path, pattern="MSA).csv", full.names=F, recursive=F)
  
  for (f in file_list) {
    # There are several broken files. skip those
    MSA_table <- try(read.table(file=paste0(dir_path,f), sep=",",header=T))
    if(class(MSA_table)=="try-error") {
      print("Error caught")
      next
    }
    
    # Sum the 1q employment value for each MSA. 
    #1Q01 MSA data will be regressed with 4Q00 lending data, so just subtract 1 from the year when writing in the master
    employment = MSA_table$month3_emplvl[1]
    
    QCEW_master[rowcount,"time"] <- yr-1
    QCEW_master[rowcount,"MSA"] <- toupper(f)  # Since the city name in Y9C data is all caps
    QCEW_master[rowcount,"employment"] <- as.numeric(employment)
    rowcount = rowcount+1
    
  }
}

# Output
write.csv(QCEW_master, file = "QCEW(time-MSA-employment).csv", append = FALSE, quote = TRUE, sep = ",",
          eol = "\n", na = "NA", row.names = TRUE,
          col.names = TRUE, qmethod = c("double"))




################ Regression ###############################
# Input : Y9C_master (time, city, loan)
# Input2 : QCEW_master (time, MSA, employment)
# build a regression table of (time, MSA-city, loan, employment)

regression_master = as.data.frame(matrix(NA, nrow=1, ncol=4))
colnames(regression_master) = c("time", "city", "loan", "employment")
rowcount = 1

for (i in 1:nrow(Y9C_master)) {
  yr = Y9C_master$time[i]
  city_str = Y9C_master$city[i]
  loan = as.numeric(Y9C_master$loan[i])
  
  logic_array = ( QCEW_master$time == yr & grepl(city_str, QCEW_master$MSA) ) # check is MSA string contains city string
  employment = QCEW_master[logic_array,"employment"][1]
  
  if (is.na(employment))
    next
  
  regression_master[rowcount, "time"] = yr
  regression_master[rowcount, "city"] = city_str
  regression_master[rowcount, "loan"] = loan
  regression_master[rowcount, "employment"] = employment
  
  rowcount = rowcount + 1
}

# output
write.csv(regression_master, file = "Panel(time-city-loan-employment).csv", append = FALSE, quote = TRUE, sep = ",",
          eol = "\n", na = "NA", row.names = TRUE,
          col.names = TRUE, qmethod = c("double"))



######### Panel data for js files #####################
#### formats the data into js-readable file format ####
js_loan_master <- data.frame()




###### Regressino Step Two ######################

JOB <- cbind(regression_master$employment)  # independent variable
LOAN <- cbind(regression_master$loan) # dependent variable

pdata <- plm.data(regression_master, index=c("city", "time"))

summary(JOB)
summary(LOAN)

fixed <- plm(JOB~LOAN, data=pdata, model="within") # also try within, between, pooling, fd, 
summary(fixed)

between <- plm(JOB~LOAN, data=pdata, model="between")
summary(between)

pooling <- plm(JOB~LOAN, data=pdata, model="pooling")
summary(pooling)


# Presentation:
# Draw an example table figure to illustrate how I derived the whole thing (outline of master, Y9C_master, QCEW_maser, etc.)
# 10 years of data
# We used naive method of checking the city name string with the MSA string
# time lag of one quarter (partly because ATOT is only available in Q4), since that was what the paper was doing
# Botton 10% of ATOT to get rid of JPMorgan and Citigroup
# With MSA level, we have about 10x more data points than last meeting
# Difference with lending data : Alexey's paper uses Call Reports. Not use them because it is no longer in service, and Y9C was cleaner
