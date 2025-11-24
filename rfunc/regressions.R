library(dplyr)
library(tidyr)
library(broom)
library(openxlsx)

LMs <- function(y) {
  df <- data.frame(as.vector(y, mode='numeric'),sec)
  fit <- lm(y ~ sec, na.action = na.exclude)
  tidy <- rename_with(tidy(fit)[2,-1],~c("Slope","StdError","tStat","pVal"))
  list <- as.list(cbind(intercept = coef(fit)[1],tidy,
                        RMSE = sqrt(mean(fit$resid^2)),
                        rename_with(select(glance(fit),matches("^(adj)")),~c("R") )))
}


# Fits the linear regressions.
# Specify the filepath and sheet (can be name or index number)
# gets the vector `sec` by either grabbing it as a column or by detecting the unique numbers in the column names
# Then it separates the FNF variable and the value type from the duration columns before fitting regressions
# to the durations of each row, and unnests each result.
# if aggregate is TRUE, then it will fit to the average of each factor (group, FNF, and value type)
getRegs <- function(file,sheet=1,aggregate=FALSE) {
  sht <-  read.xlsx(file,sheet = sheet)
  if ("sec" %in% names(sht)){
    sec <- unlist(select(sht,sec))
    sht <- select(sht,!("sec"))
  } else sec <- as.vector(unique(na.omit(str_extract(names(sht),"\\d\\.\\d"))),"numeric")
  fnf <- sht %>% pivot_longer(ends_with("f"),
                 cols_vary = "slowest",names_to=c("var",".value","FNF"),
                 names_pattern = "^([^\\d]+)(\\d\\.\\d)(?=(?i)(nf|f)+$)") %>%
                 rowwise() %>% mutate(FNF = str_to_lower(FNF))
  if (isTRUE(aggregate)) {
    fnf <- ungroup(select(fnf,!(code))) %>% reframe(.by=c("group","FNF","var"),across(matches("\\d$"),mean))
  }
  try <- fnf %>% rowwise() %>% mutate(m = list(LMs(t(pick(as.vector(matches("\\d$")))))),.keep="unused")
  try <- try %>% pivot_wider(names_from="FNF",values_from=m) %>% unnest_wider(!matches("group|code"),names_sep="")
}

