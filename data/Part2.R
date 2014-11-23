Y9C_example <- read.table(".//Y9C//bhcf0712.txt", sep="^", nrows=-1, comment.char="", header=TRUE, quote="", na.strings="--------", as.is=TRUE)
QCEW_example <- read.table(".//QCEW//2007.q1.by_size.csv", sep=",",header=T)
bhcfnames <- read.table(".//Y9C//bhcstates.csv", sep=",", fill=T, header=T)
bhcfnames <- data.frame(lapply(bhcfnames, as.character), stringsAsFactors=FALSE)

Y9C_example[,c("BHCP2200")]   # total deposits
Y9C_example[,c("BHCK2122")]   # total loans
Y9C_example[,c("RSSD9001")]   # company code
  
#initialize year-state matrix
mat = matrix(c(0))
colnames(mat) <- c("NY")
for (state_tk in bhcfnames$state_code) {
  if (!(state_tk %in% colnames(mat))) {
    mat <- cbind(mat,c(0))
    colnames(mat)[length(mat[1,])] <- state_tk
  }
}
  
  
#test for one year
yridx = 1
for (i in 1:length(Y9C_example$RSSD9001)) {
  co = Y9C_example$RSSD9001[i]
  if (is.na(co))
    next
    
  logical_array = (bhcfnames$code == co)
state_str = bhcfnames$state_code[ logical_array  ][1]
  if (is.na(state_str))
    next
  
  loan = Y9C_example$BHCK2122[i] 
  if(is.na(loan))
    next
  mat[yridx,state_str] = mat[yridx,state_str] + loan
  
}






#initialize year-state matrix
mat = matrix(c(0))
colnames(mat) <- c("NY")
for (state_tk in bhcfnames$state_code) {
  if (!(state_tk %in% colnames(mat))) {
    mat <- cbind(mat,c(0))
    colnames(mat)[length(mat[1,])] <- state_tk
  }
}




idx_convert = 1990

for (yr in 1991:2011) {
  yr_idx = paste0(yr)
  yr_idx = substr(yr_idx, start=3, stop=4)
  filename = paste0(".//Y9C//bhcf", yr_idx, "03.txt")
  Y9C_data <- read.table(filename, sep="^", nrows=-1, comment.char="", header=TRUE, quote="", na.strings="--------", as.is=TRUE)
  colnames(Y9C_data)[colnames(Y9C_data)=="rssd9001"] <- "RSSD9001"
  
  for (i in 1:length(Y9C_data$RSSD9001)) {
    
    co = Y9C_data$RSSD9001[i]
    if (is.na(co))
      next
    
    logical_array = (bhcfnames$code == co)
    state_str = bhcfnames$state_code[ logical_array  ][1]
    if (is.na(state_str))
      next
    
    loan = Y9C_data$BHCK2122[i] 
    if(is.na(loan))
      next
    
    mat[yr-idx_convert,state_str] = mat[yr-idx_convert,state_str] + loan
    
  }
  
  mat <- rbind(mat, rep(0, times=length(mat[1,])) )
}
mat.df <- as.data.frame(mat)



#initialize year-state matrix for QCEW
QCEW_data <- read.table(".//QCEW//2007.q1.by_size.csv", sep=",",header=T)
no_levels = length(levels(QCEW_data$area_title[1]))
mat2 = matrix( rep.int(0, times= no_levels  ))
dim(mat2) = c(1,no_levels)
colnames(mat2) <- levels(QCEW_data$area_title[1])


#create a table for qcew
for (yr in 1991:2011) {
  filename = paste0(".//QCEW//" , paste0(yr), ".q1.by_size.csv" )
  QCEW_data <- read.table(filename, sep=",", header=T)
  QCEW_data <- data.frame(lapply(QCEW_data, as.character), stringsAsFactors=FALSE)
  
  
  for (i in 1:length(QCEW_data$area_title)) {
    curr_str = QCEW_data$area_title[i]
    if(curr_str == "U.S. TOTAL"){
      next
    }
    
    if(QCEW_data$industry_code[i] == "10") {
      mat2[yr - idx_convert, curr_str] = mat2[yr - idx_convert, curr_str] + as.numeric(QCEW_data$month1_emplvl[i])
    }
    
  }
  mat2 <- rbind(mat2, rep(0, times=length(mat2[1,])) )
}
mat2.df <-as.data.frame(mat2)

state_abbrev <- matrix(rep(NA, length(colnames(mat2.df))))
rownames(state_abbrev) <- colnames(mat2.df)
state_abbrev[,1] <- c("AL", "AK", "AZ", "AR",
                      "CA","CO","CT","DE","DC","FL",
                      "GA","HI","ID", "IL","IN","IA",
                      "KS","KY","LA","ME","MD","MA",
                      "MI","MN", "MS","MO","MT",
                      "NE","NV","NH","NJ","NM","NY",
                      "NC","ND","OH","OK","OR", "PA","PR",
                      "RI","SC","SD","TN","TX","US",
                      "UT","VT","VI","VA","WA",
                      "WV","WI","WY")
colnames(mat2.df) <- state_abbrev[colnames(mat2.df),1]

write.csv(mat.df, file = "lending_by_state.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", row.names = TRUE,
            col.names = TRUE, qmethod = c("double"))

write.csv(mat2.df, file = "employment_by_state.csv", append = FALSE, quote = TRUE, sep = ",",
          eol = "\n", na = "NA", row.names = TRUE,
          col.names = TRUE, qmethod = c("double"))


print(1)
