library("psych")
library("quantmod")
library("ggplot2")
library("reshape2")


MDRM = c("month1_emplvl")
QCEW = c()
rnames = c()


for (yr in 1991:2011) {
  print(yr)
  yr_idx = paste0(yr)
  yr_idx = substr(yr_idx, start=3, stop=4)
  for (qtr in 1:1) {
    filename = ".//QCEW//"
    qtr_idx <- "q1"
    filename = paste0(filename, paste0(yr),'.',qtr_idx,".by_size.csv")
    data <- read.table(filename, sep=",",header=T)
    
    logic.array <- data$area_fips == "US000" & data$industry_code=="10"
    emp.lvl = sum(data$month1_emplvl[logic.array], na.rm=TRUE)
    
    QCEW[length(QCEW)+1] = emp.lvl
    rnames[length(rnames)+1] = paste0(yr_idx,'Q1')
    
  }
}


QCEW.df = data.frame(rnames, QCEW, stringsAsFactors=FALSE)
colnames(QCEW.df) <- c("qtr","emp_lvl")

g <- ggplot(data=QCEW.df, aes(x=1:length(emp_lvl),y=emp_lvl))
g <- g + geom_line() + ggtitle("Employment level 1990-2011") + ylab("Employment level")
g <- g + scale_x_discrete(breaks=seq(from=5,to=length(QCEW.df$qtr),by=5), 
                          labels=QCEW.df$qtr[seq(from=5,to=length(QCEW.df$qtr),by=5)]) +xlab(NULL)
g

describe(QCEW.df$emp_lvl)


combined.df <- data.frame(rnames,QCEW$loan,Y9C.df$emp_lvl)
colnames(combined.df) <- c("qtr", "loan", "emp_lvl")

g <- ggplot(data=combined.df, aes(x=))


#overlay QCEW with Y9C


