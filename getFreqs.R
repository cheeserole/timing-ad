library(tidyverse)
source("code/getPData.R")

# This script is meant to be run manually.


# # # # # # # # # # # # # # # # # # # # # # # # # # # #
#    Step 1 - Enter directory paths for each group    #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # 
paths <- list(
  aPath = "D:/OneDrive/Nottingham/Documents - Chasity's data/Revisions/DATA/AD",
  iPath = "D:/OneDrive/Nottingham/Documents - Chasity's data/Revisions/DATA/MCI",
  oPath = "D:/OneDrive/Nottingham/Documents - Chasity's data/Revisions/DATA/Mixed",
  cPath = "D:/OneDrive/Nottingham/Documents - Chasity's data/Revisions/DATA/Control_ACE-III"
)


# # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   Step 2 - Get participant info for every folder    #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # 
pInfo <- list_rbind(lapply(paths, getpInfo))

# copies pInfo to clipboard so you can paste to excel of your choice.
writeClipboard(capture.output(write.csv(pInfo)), format = 1) 
writeLines("Participant info has been copied to clipboard.")


# # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   Step 3 - Get each group's response data           #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Gets all sheet names with the name `block_` in them,# 
#  and grabs columns D and L and rows 1 to 7. Then it # 
#  rearranges the data frame into two groups of the   #
#  five durations based on the value from `feedback`. #
#  It also numbers the data based on row order 0-5    #
#  and discards the first 2 responses from each set.  #
#######################################################
# Obviously it takes a very long time to achieve this,#
# so each group should be run separately to maintain  #
# control if something goes wrong.                    #
#######################################################

# get the AD responses
ALLAD <-  with_dir(paths[['aPath']],
                   map(set_names(list.files(pattern = "\\.xlsx$")),~ as.list(getResps(.x)),.progress=TRUE))

### get the MCI responses
ALLMCI <- with_dir(paths[['iPath']],
                   map(set_names(list.files(pattern = "\\.xlsx$")),~ as.list(getResps(.x)),.progress=TRUE))

### get the OD responses
ALLOD <-  with_dir(paths[['oPath']],
                   map(set_names(list.files(pattern = "\\.xlsx$")),~ as.list(getResps(.x)),.progress=TRUE))

### get the Control responses
ALLCon <- with_dir(paths[['cPath']],
                   map(set_names(list.files(pattern = "\\.xlsx$")),~ as.list(getResps(.x)),.progress=TRUE))


# # # # # # # # # # # # # # # # # # # # # # # # # # # #
#  Step 4 - Apply frequencies to each group           #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #

aFreq <- applyFreq(ALLAD)
iFreq <- applyFreq(ALLMCI)
oFreq <- applyFreq(ALLOD)
cFreq <- applyFreq(ALLCon)

# # # # # # # # # # # # # # # # # # # # # # # # # # # #
#  Step 5 - Combine them into one big ALL list        #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #

All <- c(aFreq,iFreq,oFreq,cFreq)
names(All) <- str_match(names(All),"^[^\\_]+")

# Aggregates the duration frequencies over FNF
#All <- lapply(set_names(names(All)), \(x) aggFNF(All[[x]]))


# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Step 6 Option A - Make new, separate duration lists #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #

# AllCon[0:23] removes all the controls with incomplete data
Resps <- c(ALLAD,ALLMCI,ALLOD,ALLCon[0:23])
b <- unnest_wider(list_cbind(lapply(Resps,as_tibble)),col=everything(),names_sep="_")
d07 <- select(b,matches("0\\.7")) %>% relocate(matches("_F"))
d13 <- select(b,matches("1\\.3")) %>% relocate(matches("_F"))
d19 <- select(b,matches("1\\.9")) %>% relocate(matches("_F"))
d25 <- select(b,matches("2\\.5")) %>% relocate(matches("_F"))
d31 <- select(b,matches("3\\.1")) %>% relocate(matches("_F"))

# writes tables if you like
write.table(d07,"d07.csv", sep = ",")
write.table(d13,"d13.csv", sep = ",")
write.table(d19,"d19.csv", sep = ",")
write.table(d25,"d25.csv", sep = ",")
write.table(d31,"d31.csv", sep = ",")

# from ss_gauss - fits every new duration list into gaussians
# source("code/SSGaus.R")
# sec=seq(0.05,9.9,by=0.05)
# # calculations by duration
#   g07 <- lapply(slice(d07,2:(n()-1)),initxb)
#   g13 <- lapply(slice(d13,2:(n()-1)),initxb)
#   g19 <- lapply(slice(d19,2:(n()-1)),initxb)
#   g25 <- lapply(slice(d25,2:(n()-1)),initxb)
#   g31 <- lapply(slice(d31,2:(n()-1)),initxb)


  
# # # # # # # # # # # # # # # # # # # # # # # # # # # #
#    Step 6 Option A - Calculate EVERYTHING           #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #

#######################################
# Calculate gaussians and write table #
#######################################
All <- c(aFreq,iFreq,oFreq,cFreq)
names(All) <- str_match(names(All),"^[^\\_]+")
b <- unnest_wider(list_cbind(lapply(All,as_tibble)),col=everything(),names_sep="_")

# Creates sec, and then fits gaussians with trim
source("code/SSGaus.R")
sec <- seq(0.05,9.9,by=0.05)
g <- sapply(slice(b,2:(n()-1)),initxb)

rGauss <- as_tibble(as.data.frame(g),rownames="var")
rGauss <- rGauss %>% pivot_longer(!(var),cols_vary="slowest",names_to=c("code",".value"),names_pattern="^([^_]+)_(.*)$")
rGauss <- rGauss %>% 
          pivot_wider(names_from=`var`,values_from = matches("^\\d"),names_sep = ".",names_sort = TRUE) %>% 
          relocate(code,matches("\\.A"),matches("\\.B"),matches("\\.C"),
                        matches("\\.r\\.S"),matches("\\.mse"),matches("\\.SSq"),matches("\\.SSr"))
write.table(rGauss,"allGauss.csv", sep = ",")


#######################################
# Calculate means and StDevs          #
#######################################
# Remake All but with responses instead of frequencies
All <- c(ALLAD,ALLMCI,ALLOD,ALLCon)
names(All) <- str_match(names(All),"^[^\\_]+")
All <- modify_tree(All,leaf=~na.omit(.x))

# Means
means <- modify_tree(All,leaf=~mean(.x,na.rm=TRUE))
means <- unnest_wider(list_cbind(lapply(means,as_tibble)),col=everything(),names_sep="_")
means <- pivot_longer(means,cols=everything(),names_to=c("code",".value"),names_pattern="^([^_]+)_(.*)$")
write.table(means,"means.csv", sep = ",")

# Standard Deviations
SDs <- modify_tree(All,leaf=~sd(.x,na.rm=TRUE))
SDs <- unnest_wider(list_cbind(lapply(SDs,as_tibble)),col=everything(),names_sep="_")
SDs <- pivot_longer(SDs,cols=everything(),names_to=c("code",".value"),names_pattern="^([^_]+)_(.*)$")
write.table(SDs,"stdevs.csv", sep = ",")


########################################
# Aggregated gaussian, for weird stuff #
########################################
aggre <- function(data) {
  names <- unique(str_extract(names(b),"(?<=_)(.*)$"))
  g <- unique(tolower(str_extract(names(b),"^\\w")))
  sht <- lapply(set_names(names),\(x) select(data,ends_with(x)))
  s <- lapply(set_names(names(sht)),\(x) unnest(as_tibble(lapply(set_names(g),\(n) transmute(rowwise(select(sht[[x]],starts_with({{n}}))),{{n}}:=mean(c_across(starts_with({{n}})))))),cols=everything()))
  s <- unnest(as_tibble(s),cols=everything(),names_sep = "")
  names(s) <- str_replace(names(s),"([^\\_]*?)_([^\\_]*?)(?=(\\p{Ll})$).", "\\3\\1\\2")
}
sht <- sapply(slice(s,2:(n()-1)),initxb)
write.table(sht,"aggGauss.csv", sep = ",")
