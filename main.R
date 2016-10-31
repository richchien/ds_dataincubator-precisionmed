### Proposal: Predicting the price for precision medicine

### Data source: MEPS
### https://meps.ahrq.gov/mepsweb/data_stats/download_data_files.jsp
library(RCurl)
library(foreign)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

datafolder <- "C:/Users/RC3258/Documents/ds_data-incubator-precisionmed/data"
setwd(datafolder)

# Download files
# setup MEPS data files for download, reference https://gist.github.com/ajdamico/669666
year <-       c(2008:2012)
consolid <-   c(79,89,97,105,113,121,129,138,127,155)
conditions <- c(78,87,96,104,112,120,128,137,126,154)
prp <-        c(76,88,95,103,111,119,127,136,125,153)
events <-     c(77,85,94,102,110,118,126,135,124,152)
downloadsetup <- data.frame(year, consolid, conditions, prp, events)

# additional events files
downloadsetup$medi <- paste0(downloadsetup$events,"a")
downloadsetup$dental <- paste0(downloadsetup$events,"b")
downloadsetup$otherxp <- paste0(downloadsetup$events,"c")
downloadsetup$inpatient <- paste0(downloadsetup$events,"d")
downloadsetup$er <- paste0(downloadsetup$events,"e")
downloadsetup$outpatient <- paste0(downloadsetup$events,"f")
downloadsetup$office <- paste0(downloadsetup$events,"g")
downloadsetup$home <- paste0(downloadsetup$events,"h")
downloadsetup$events <- NULL

# download sas transport ssp files
for (i in 1:nrow(downloadsetup) ) {
	for (j in 2:ncol(downloadsetup) ) {
		if (!is.na(downloadsetup[i,j])){
			filename <- paste0("h",downloadsetup[i,j],"ssp.zip")
			zipfile <- paste0("http://www.meps.ahrq.gov/mepsweb/data_files/pufs/h" , downloadsetup[i,j] , "ssp.zip")
			download.file(zipfile, filename)
			unzipped <- unzip(filename, list=T)
			unzip(filename)
			finalname <- paste0(downloadsetup[i,1] , "_" , names(downloadsetup)[j] , ".ssp")
			file.rename(as.character(unzipped[1,1]), finalname)
			file.remove(filename)					
		}	
	}
}


# Task 1
# explore healthcare expenditures for cancer patients vs non-cancer patients

# read consolidated file overall summary for 2012
consolid <- read.xport("2012_consolid.ssp")
consolid %>% 
select(DUPERSID, CANCERDX, OBVEXP12, OPTEXP12, ERTEXP12, IPTEXP12, RXEXP12, HHAEXP12, HHNEXP12, OTHEXP12) -> consolid_set
names(consolid_set) <- c("DUPERSID", "CANCERDX", "Office_visit", "Outpatient", "ER_visit", "Inpatient", "Medication", "Home_health_agency", "Home_health_ind", "Other")

consolid_set %>% filter(CANCERDX==1) -> consolid_cancer
consolid_set %>% filter(CANCERDX==2) -> consolid_noncancer

write.csv(consolid_cancer, "consolid_cancer.csv")
write.csv(consolid_noncancer, "consolid_noncancer.csv")
  
mods <- theme_bw() +
theme(axis.title = element_text(size = "16"),
	panel.grid.minor = element_blank(), 
	axis.text = element_text(size = "14"), 
	axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.3), 
	axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
	axis.text.y = element_text(size = 14),
	legend.text = element_text(size = 14), 
	legend.title = element_text(size = 14), 
	legend.position="none", 
	panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
	plot.title = element_text(color="#666666", face="bold", size=16, hjust=0)
)

# label count for boxplot
give.n <- function(x){
   return(c(y = mean(x), label = length(x)))
}

# boxplots
consolid_cancer %>%
gather(insurance, value, -DUPERSID, -CANCERDX) -> temp
p1 <- ggplot(temp, aes(x=insurance, y=value, fill = insurance))
p1 <- p1 + geom_boxplot() + ylim(1, 40000) +
stat_summary(fun.data = give.n, geom = "text") + 
xlab("") + ylab("Expenditure $") + 
ggtitle("Cancer patient Expenditures 2012") +
mods

consolid_noncancer %>%
gather(insurance, value, -DUPERSID, -CANCERDX) -> temp
p2 <- ggplot(temp, aes(x=insurance, y=value, fill = insurance))
p2 <- p2 + geom_boxplot() + ylim(1, 40000) +
stat_summary(fun.data = give.n, geom = "text") + 
xlab("") + ylab("Expenditure $") + 
ggtitle("Non-cancer patient Expenditures 2012") +
mods

# plot 1
grid.arrange(p1, p2)


# Task 2 
# explore cost trends 2003 - 2012 of chemotherapy expenditures for cancer patients by insurance coverage
# 1455683 observations

# CPI table to adjust price http://www.usinflationcalculator.com/inflation/consumer-price-index-and-annual-percent-changes-from-1913-to-2008/
cpifile <- "CPI-U_table.csv"
cpitab <- read.csv(cpifile)

# function to read and format office visit files
compile.office <- function(datafile) {
	df <- read.xport(datafile)
	filename <- as.character(datafile)
	year <- gsub("_.*", "", filename)
	obsf<-grep("OBSF.*", names(df))
	obmr<-grep("OBMR.*", names(df))
	obmd<-grep("OBMD.*", names(df))
	obpv<-grep("OBPV.*", names(df))
	obot<-grep("OBOT.*", names(df))
	cpitab %>%
	filter(Year %in% year) -> cpitabtemp
	df %>% 
	select(EVNTIDX, CHEMOTH, RADIATTH, obsf, obmr, obmd, obpv, obot) %>%
	mutate(yr=year) %>%
	mutate(cpi=as.numeric(cpitabtemp$Avg))-> df
	names(df)<-c("eventid", "chemo", "radiation", "self_paid", "medicare", "medicade", "private", "other", "yr", "cpi")
	return(df)
}

# combine 2003 - 2012
filelist = list.files(pattern=".*_office") 
df <- ldply(filelist, compile.office)

df %>% 
	filter(chemo==1) %>%
	gather(insurance, value, self_paid:other) %>%
	mutate(adj_exp = value * 229.594/cpi) %>%
	group_by(yr, insurance) %>% dplyr::summarize(mean_expenditure=mean(value), mean_adj_expenditure=mean(adj_exp), count=n()) -> dfchemo
	write.csv(dfchemo, "chemo-trend.csv")

mods2 <- theme_bw() +
theme(axis.title = element_text(size = "16"),
	panel.grid.minor = element_blank(), 
	axis.text = element_text(size = "16"), 
	axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.3), 
	axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
	axis.text.y = element_text(size = 14),
	legend.text = element_text(size = 14), 
	legend.title = element_text(size = 14), 
	legend.position="right", 
	panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
	plot.title = element_text(color="#666666", face="bold", size=16, hjust=0)
)
	
pchemo <- ggplot(dfchemo, aes(x=yr, y=mean_adj_expenditure, color = insurance, group=insurance))
pchemo <- pchemo + geom_point(size = 5) + geom_line() + 
xlab("") + ylab("Mean Expenditure $") + 
ggtitle("Mean Expenditures for Chemotherapy Visits from 2003-2012 (Adjusted with 2012 CPI)") +
mods2

# plot 2
pchemo


