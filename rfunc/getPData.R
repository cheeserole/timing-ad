library(openxlsx)
library(tidyverse)

# Enter directory paths for each group
# setwd(choose.dir())
paths <- list(
              aPath = "A:/Users/Chasity/Notes/OneDrive/DATA/AD",
              iPath = "A:/Users/Chasity/Notes/OneDrive/DATA/MCI",
              oPath = "A:/Users/Chasity/Notes/OneDrive/DATA/Mixed",
              cPath = "A:/Users/Chasity/Notes/OneDrive/DATA/Control_ACE-III"
          )



################################
#         Functions            #
################################

# dumb wrapper for quick directory jumping
with_dir <- function(dir, expr) {
  if (!getwd() == dir){
    old_wd <- getwd()
    on.exit(setwd(old_wd))
    setwd(dir)}
  evalq(expr)
}

### Step 1 - Get Participant Info of every folder
# goes to the path (if not set already), grabs all xlsx files, and reads participant info from each.
getpInfo <- function(path) {
  if (!getwd() == path){
    old_wd <- getwd()
    on.exit(setwd(old_wd))
    setwd(path)}
  files <- list.files(path = path,pattern = "^[^~].*\\.xlsx$")
  pinfo <- list_rbind(lapply(set_names(files),\(x) 
                             read.xlsx(x,sheet = "block_F1_",rows=9:15,cols=1:2)),names_to="file")
  plist <- pivot_wider(pinfo, names_from = extraInfo,values_from = X2) %>% 
    mutate(file = str_remove(file,"\\.xlsx$")) 
}

## Step 2 - Get responses from each sheet
# Collects the provided range using the provided sheet,
# Then appends a column with either `F` or `NF` from the sheet's name
getBlock <- function (path,sht=1,rows = NULL,cols = NULL) {
  book <- read.xlsx(path,sheet = sht,cols = cols, rows = rows)
  #read_xlsx(path,sheet = sht,range = rng,col_names = TRUE,col_types = NULL,n_max = 20)
  book <- book %>% mutate(feedback = as.vector(str_match(sht,"(?<=\\_)\\p{Lu}+")))
}

# Notes all sheet names with the name `block_` in them, and feeds each sheet
# into getBlock(), getting columns D and L and rows 1 to 7.
# Then rearranges the data frame into two groups of the five durations based
# on the value from `feedback`. The responses retain their row orders.
getResps <- function (file,order=TRUE) {
  sheets <- getSheetNames(file) %>% .[!sapply(.,function(x) all(grepl("block_",x) == 0))]

  fData <- list_rbind(lapply(sheets,function(x) getBlock(file[[1]],sht = x,rows = 1:7,cols =  c(4,12)))) %>%
            pivot_wider(names_from = correctseconds, values_from = resp.rt_mean,
                        names_vary = "slowest",values_fn = list) %>% unnest(cols = everything()) %>%
            mutate(order = rep(seq(0,5),length.out=n()))
  if (isTRUE(order)) {fData <- filter(fData,{feedback == "F" & order > 1}|feedback == "NF") } #%>% arrange(order)}
  fData <- pivot_wider(fData,names_from = feedback, values_from = c(`0.7`,`1.3`,`1.9`,`2.5`,`3.1`),
                       names_vary = "slowest",values_fn = list) %>% unnest(cols = everything()) %>% select(!(order))
  # writeLines(paste("File",file,"completed."))
  return(fData)
}

ordAD <-  with_dir(paths[['aPath']],
                   map(set_names(list.files(pattern = "\\.xlsx$")),~ as.list(getResps(.x,order=TRUE)),.progress=TRUE))


###############################
#    Frequency Functions      #
###############################

getFreq <- function(x, i) {
  s <- as_tibble(x[!x>10],.name_repair="unique")
  if (str_detect(i, "_F$")){
    s <- s %>% filter(!row_number() %% 6 %in% 1:2)
  }
  h <- hist(unlist(s),breaks = seq(0,10,by=0.05),include.lowest=TRUE)
  return(h$counts)
}


applyFreq <- function(g) {lapply(set_names(names(g)),\(n) imap(g[[n]],~ getFreq(.x,.y)))}

aggFNF <- function(x) {as_tibble(x) %>% rowwise() %>% mutate(`0.7` = mean(c_across(matches("0.7"))),
                                                             `1.3` = mean(c_across(matches("1.3"))),
                                                             `1.9` = mean(c_across(matches("1.9"))),
                                                             `2.5` = mean(c_across(matches("2.5"))),
                                                             `3.1` = mean(c_across(matches("3.1"))),.keep="none")}

#################################
# Workbook (OpenXLSX) Functions #
#################################
# Creates an empty workbook
makeWb <- function (name=NULL){
  wb <- createWorkbook()
  nom = 1
  if (!is.null(name)) {
    nom = name}
  addWorksheet(wb,1,zoom = 50)
  setColWidths(wb,1,1:200,widths = 6)
  return(wb)
}

# Edits the workbook, because I hate tedium. Inefficient but idc anymore
# For each name in the data list, it finds the matching sheet to the participant code.
# Then it unnests the data frame in order to map the columns to their respective excels.
# If it's F, it filters out the first 2 rows for each group of six.
editWb <- function(wb,sht=1,data,file=NULL) {
          namelist <- names(data)
          for (n in namelist){
            sheetname = paste(sht,str_match(n,"[1-3]?\\d(?=\\_)"),sep="")
            tbl <- unnest(as_tibble(list_transpose(data[n])),cols = everything())
            map(names(tbl),\(x) 
                writeData(wb,sheet=sheetname,
                          if(str_detect(x,"N")) select(tbl,all_of(x)) else filter(select(tbl,x),!row_number() %% 6 %in% 1:2),
                          startCol={grep(x, colnames(tbl)) * 2 + if(str_detect(x,"N")) 2 else 0},
                          startRow=1
                ))
            writeLines(paste("Sheet",n,"completed."))
          }}


# runs editWb for every file in the folder.
# If you provide a file, it'll load that one,
# and if you provide a save path, it'll save there,
# otherwise it'll overwrite the loaded workbook.
doWbs <- function(path,sheet,loadfrom=NULL,saveto=NULL,data=NULL){
  case_when(
    is.null(loadfrom)&is.null(saveto) ~ abort("need to provide at least one path."),
   !is.null(loadfrom)&is.null(saveto) ~ saveto <- loadfrom,
    is.null(loadfrom)&!is.null(saveto) ~ thisWb <- makeWb(),
   !is.null(loadfrom)&!is.null(saveto) ~ thisWb <- loadWorkbook(loadfrom),
  )
  if (is.null(data)) {
    data <- with_dir(path,
                     map(set_names(list.files(pattern = "\\.xlsx$")),
                     ~ as.list(getResps(.x)),.progress=TRUE))}
  editWb(thisWb,sht=sheet,data=data)
  saveWorkbook(thisWb,saveto,overwrite=TRUE)
  writeLines(paste("Data was saved to",saveto,"."))
}
