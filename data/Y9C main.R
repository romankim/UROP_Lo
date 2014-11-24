# Unit: Dollar amounts in thousands

# BHCK1287: Loans to depository institutions
# BHCK2081, BHDM2081 : Total loans and leases, net of unearned income
# BHCK2122 : Total loans and leases, net of unearned income <- this is it !!
# BHCK2123, BHDM2123 : Total loand and leases
# BHCK3353 : Quarterly average of total loans

library("psych")
library("quantmod")
library("ggplot2")


MDRM = c("BHCK2122")
Y9C = c()
rnames = c()


for (yr in 1991:2011) {
  print(yr)
  yr_idx = paste0(yr)
  yr_idx = substr(yr_idx, start=3, stop=4)
  for (qtr in 1:4) {
    filename = ".//Y9C//bhcf"
    qtr_idx = qtr*3
    qtr_idx = paste0(qtr_idx)
    if(nchar(qtr_idx)==1) {
      qtr_idx = paste0("0",qtr_idx)
    }
    filename = paste0(filename,yr_idx,qtr_idx,".txt")
    
    data <- read.table(filename, sep="^", nrows=-1, comment.char="", header=TRUE, quote="", na.strings="--------", as.is=TRUE)
    total.loan = sum( as.numeric( data[,MDRM]   ), na.rm = TRUE)
    
    Y9C[length(Y9C)+1] = total.loan
    rnames[length(rnames)+1] = paste0(yr_idx,'Q',qtr)
    
  }
}


Y9C.df = data.frame(rnames, Y9C, stringsAsFactors=FALSE)
colnames(Y9C.df) <- c("qtr","loan")

g <- ggplot(data=Y9C.df, aes(x=1:length(loan),y=loan))
g <- g + geom_line() + ggtitle("Total loans 1990-2011") + ylab("Loan in thousands USD")
g <- g + scale_x_discrete(breaks=seq(from=20,to=84,by=20), 
                          labels=Y9C.df$qtr[seq(from=20,to=84,by=20)]) +xlab(NULL)
g


describe(Y9C.df$loan)

pdf("Y9C.pdf", onefile=T)
dev.off()

