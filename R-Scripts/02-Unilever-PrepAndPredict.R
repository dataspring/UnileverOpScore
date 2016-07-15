
set.seed(17)


load("D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\20141211130801-Training.RData")
uni.raw <- Training_Data[,colSums(is.na(Training_Data)) != nrow(Training_Data)]



require('fBasics')
basicStats(uni.raw$X5)[2]
colnames(uni.raw)[276]


## ---- feature engineering ----------------------------------------------------------------
## ---- get the highly correlated ones to be removed first
## load the library
library(mlbench)
library(caret)
library(stringr)

# load only numerical data
nums <- sapply(uni.raw, is.numeric)
uni.raw.num <- uni.raw[, nums]

# calculate correlation matrix
correlationMatrix <- cor(uni.raw.num,use = "pairwise.complete.obs")
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
print(highlyCorrelated)
## ----- have issues determining the correlated ones as findCorrelation gives error for NA values--------------------



## ----- an Learning Vector Quantization (LVQ) model. The varImp is then used to estimate the variable importance---------------
## -- http://machinelearningmastery.com/feature-selection-with-the-caret-r-package/ 

### --------------------not done ------------------------------
## ---- remove 1st set of duplicate columns "Smell..While.in.the.pack" to "Smell..When.wearing.clothes.at.the.end.of.the.day" 
## --- with "Strength.of.Smell..While.in.the.pack" to "Strength.of.Smell..When.wearing.clothes.at.the.end.of.the.day"  
## --- remove columns 170 to 179   ---- retain this though 180 to 189
### --------------------not done ------------------------------



for(i in 204:228) {
  uni.raw[,i] = sapply(uni.raw[,i], function(v) {
    if (!is.na(str_locate(v, ignore.case("no "))[1])) 
      return ("No")
    else return (v)
  })
}

for(i in 204:228) {
  
  uni.raw[,i] = sapply(uni.raw[,i], function(v) {
    vtrim <- toupper(str_trim(v))
    ifelse (vtrim != "YES" && vtrim != "NO" && vtrim != "NA", "YES", v)
  })
  
}

#-- Removing Whitespace From a Whole Data Frame in R
uni.raw <- data.frame(lapply(uni.raw, function(v) {
  if (is.character(v)) return(gsub('\\s+', '',v))
  else return(v)
}), stringsAsFactors = FALSE)

#-- convert all chr to lowercase 
uni.raw <- data.frame(lapply(uni.raw, function(v) {
  if (is.character(v)) return(tolower(v))
  else return(v)
}), stringsAsFactors = FALSE)

#-- assign all numbers intact removing explantory prefixes
uni.raw <- data.frame(lapply(uni.raw, function(v) {
  if (is.character(v) & !is.na(str_locate(v, "\\(\\d\\)")[1])) 
  {
    strloc <- str_locate(v,"\\(\\d\\)")
    return(str_sub(v, start=strloc[,1], end=strloc[,2]))
  }
  else return(v)
}), stringsAsFactors = FALSE)

## -- for some columns that don't get worked from the previous logic -----------------

for(i in 253:275) {
    uni.raw[,i] = sapply(uni.raw[,i], function(v) {
      if (is.character(v) & !is.na(str_locate(v, "\\(\\d\\)")[1])) 
      {
        strloc <- str_locate(v,"\\(\\d\\)")
        return(str_sub(v, start=strloc[,1], end=strloc[,2]))
      }
      else return(v)
    })
}

## -------------------------------------------------------------------------------------------------
## - check point to save the interim result so that can be replayed

save(uni.raw, file="D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unilever-Prep1.RData" )
#write.table(x=uni.raw, file="D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unilever-prep.csv", sep="\t" )

load("D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unilever-Prep1.RData")
#write.table(x=uni.raw, file="D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unilever-prep-csv.csv", sep="," )
## -------------------------------------------------------------------------------------------------


## ----- column level data munging to reduce the factor levels of some features ------------------------

## -------------------------------------------------------------------------------------------------
recoderFunc <- function(data, oldvalue, newvalue) {
  # convert any factors to characters
  # create the return vector
  newvec <- data
  # put recoded values into the correct position in the return vector
  for (i in unique(oldvalue)) newvec[data == i] <- newvalue[oldvalue == i]
  newvec
}


## ------------------- Column Education.level ----------------------------------------------------------
oldvalue <- as.character(na.omit(unique(uni.raw$Education.level))) 
newvalue <- c("primary","graduate","secondary","graduate","secondary","primary","diploma","diploma","graduate","graduate","secondary","primary",                                 
"secondary","graduate","graduate","secondary","secondary","primary","primary","illiterate","secondary","graduate","primary",                                    
"graduate","secondary","secondary","secondary","secondary","secondary","secondary","secondary","secondary","secondary","graduate",                     
"secondary","illiterate")  

as.character(na.omit(unique(uni.raw$Education.level)))
uni.raw$Education.level <- recoderFunc(uni.raw$Education.level, oldvalue, newvalue)
as.character(na.omit(unique(uni.raw$Education.level)))
## ------------------- Column Education.level ****ends***------------------------------------------------


## ------------------- Column Amount.used  ----------------------------------------------------------
oldvalue <- as.character(na.omit(unique(uni.raw$Amount.used ))) 
# "aboutthree-fourth(+)","fullyused","abouthalf(+)","onewholepack","aboutthree-quartersofapack"   
# "abouthalfofapack","lessthanaquarterofapack","afourth(+)","aboutaquarterofapack","abouthalfofpack"              
# "aboutthreequartersofapack","abouthalfapack","onewholepack...","aboutthree-quartersofapack...","abouthalfofapack..."  

newvalue <- c("3by4","all","1by2","all","3by4",                             
              "1by2","1by4","1by4","1by4","1by2",                                   
              "3by4","1by2","all","3by4","1by2")  

as.character(na.omit(unique(uni.raw$Amount.used )))
uni.raw$Amount.used  <- recoderFunc(uni.raw$Amount.used , oldvalue, newvalue)
as.character(na.omit(unique(uni.raw$Amount.used )))
## ------------------- Column Education.level ****ends***------------------------------------------------


## ------------------- Column Times.used  ----------------------------------------------------------
oldvalue <- as.character(na.omit(unique(uni.raw$Times.used ))) 
# [1] "11+"                       "6to10"                     "7-10"                      "6-10"                     
# [5] "10-14"                     "15+"                       "8-11"                      "12+"                      
# [9] "nooftimes24-27(25.5)"      "nooftimes12-15(13.5)"      "nooftimes20-23(21.5)"      "nooftimes16-19(17.5)"     
# [13] "nooftimes28(28)"           "morethan12times"           "12times"                   "10times"                  
# [17] "13ormoretimes"             "7-10timesinthelast14days"  "10-12timesinthelast14days" "6timesinthelast14days"    
# [21] "10-14(12)"                 "15+(15)"                   "everyday"                  "10+timesinthelast14days"  
# [25] "15plus"

newvalue <- c("11to13","lt8","lt8", "lt8", 
              "11to13", "14to16", "9to10", "11to13",
              "gt16", "14to16","gt16","gt16",
              "gt16","14to16", "11to13","9to10",
              "14to16","lt8","9to10","lt8",
              "11to13", "14to16", "14to16","11to13",
              "14to16")  

as.character(na.omit(unique(uni.raw$Times.used )))
uni.raw$Times.used  <- recoderFunc(uni.raw$Times.used , oldvalue, newvalue)
as.character(na.omit(unique(uni.raw$Times.used )))
## ------------------- Column Times.used  ****ends***------------------------------------------------


## ------------------- Column Times.used  ----------------------------------------------------------
oldvalue <- as.character(na.omit(unique(uni.raw$Dry.method ))) 
# "drymostclothesoutsideinthesun"             "dryallclothesoutsideinthesun"             
# [3] "*dryallclothesoutsideintheshade"           "*drymostclothesoutsideintheshade"         
# [5] "oudoorsinsun"                              "outdoorsinsunandshade"                    
#    "outdoorsinshade"                          
# [9] "tumbledried"                               "indoors,withoutheat(e.g.onaclothesmaiden)"
# [11] "***drymostclothesinadryer"                 "**dryallclothesinsidethehouse"            
# [13] "driedmostclothesoutsideinthesun"           "driedallclothesoutsideinthesun"           
# [15] "*driedallclothesinsidethehouse"            "*driedmostclothesinsidethehouse"          
# [17] "driedallclothesoutsideintheshade"          "driedmostclothesoutsideintheshade"        
# [19] "indoors,withheat(e.g.overaradiator)"       "**drymostclothesinsidethehouse"           
# [21] "indoors,withoutheat"

newvalue <- c("outdoors","outdoors",
              "outdoors", "outdoors", 
              "outdoors", "outdoors", 
              "outdoors",
              "indoors", "indoors",
              "indoors","indoors",
              "outdoors", "outdoors",
              "indoors","indoors",
              "indoors","outdoors", 
              "indoors","indoors",
              "indoors")  

as.character(na.omit(unique(uni.raw$Dry.method )))
uni.raw$Dry.method  <- recoderFunc(uni.raw$Dry.method , oldvalue, newvalue)
as.character(na.omit(unique(uni.raw$Dry.method )))
## ------------------- Column Times.used  ****ends***------------------------------------------------



## ------------------- Column Annual.household.income  ----------------------------------------------------------
oldvalue <- as.character(na.omit(unique(uni.raw$Annual.household.income ))) 

# [1] "25001-99999"         "0-0"                 "15001-20000"         "8001-10000"          "20001-25000"        
# [6] "10001-15000"         "7001-8000"           NA                    "rs.8001-rs.10,000"   "rs.25,001+"         
# [11] "rs.10,001-rs.15,000" "rs.20,001-rs.25,000" "rs.5001-rs.7000"     "rs.2001-rs.3000"     "iprefernottosay"    
# [16] "rs.4001-rs.5000"     "rs.15,001-rs.20,000" "rs.7001-rs.8000"     "rs.3001-rs.4000"     "uptors.2000"        
# [21] "rs5000-10,000"       "rs20,000to30,000"    "rs10,001to20,000"    "30,001-40,000"       "40,001-50,000"      
# [26] "50,001-60,000"       "lessthanrs.5000"     "above80,000"         "4001-5000"           "1-2000"  

newvalue <- c("gt25k", "lt25k", "lt25k", "lt25k","lt25k",
              "lt25k", "lt25k",  "lt25k","gt25k",
              "lt25k", "lt25k", "lt25k", "lt25k","lt25k",
              "lt25k", "lt25k", "lt25k", "lt25k","lt25k",
              "lt25k", "lt25k", "lt25k", "gt25k","gt25k",
              "gt25k", "lt25k", "gt25k", "lt25k","lt25k")  

as.character(na.omit(unique(uni.raw$Annual.household.income)))
uni.raw$Annual.household.income  <- recoderFunc(uni.raw$Annual.household.income , oldvalue, newvalue)
as.character(na.omit(unique(uni.raw$Annual.household.income )))

## ------------------- Column Annual.household.income ends ----------------------------------------------------------

## --- consolidate handle foam's 7 columns into 1 -------------------------------------------------------------------

# remove these duplicate columns 
# 
# Additional.Others..Difficult.to.remove.odours
# Additional.Others..Difficult.to.remove.stains
# Additional.Others..Powder.left.in.bucket
# Additional.Others..Powder.left.on.clothes
# Additional.Others..Yellowing.of.clothes 
# ### ---------------------------------------------------------------------------------------------
# 
# ### ----- consolidate foam column from these columns
# 
# Additional.Others..Foam.is.too.much  
# Additional.Others..Foam.is.too.less  
# Additional.Others..Foam.is.less.long.lasting  
# Additional.Others..More.time.to.lather

process <- function(x) {
  
  if (is.na(x["Amount.of.form"])) 
  {
    if (!is.na(x["Amount.of.form..while.mixing.with.water"]) ||
          !is.na(x["Amount.of.form..while.soaking.clothes"]) ||
          !is.na(x["Amount.of.form..while.washing.clothes"]) ||
          !is.na(x["Amount.of.form..while.rising.clothes"]))
    {
      dtl <- data.table(Aa = c(x["Amount.of.form..while.mixing.with.water"],
                               x["Amount.of.form..while.soaking.clothes"],
                               x["Amount.of.form..while.washing.clothes"],
                               x["Amount.of.form..while.rising.clothes"] ), 
                        Cc = c(1,1,1,1))
      dtlsort <- dtl[, length(Cc), by=Aa]
      
      
      return (dtlsort[order(-dtlsort$V1)][1]$Aa)
    }
    else
    {
      if (!is.na(x["Additional.Others..Foam.is.too.much"]))
      {
        vtrim <- toupper(x["Additional.Others..Foam.is.too.much"])
        ifelse (vtrim == "NO", "(3)", (ifelse (vtrim == "YES", "(1)", NA)))
      }
      else
      {
        if (!is.na(x["Additional.Others..Foam.is.too.less"]))
        {
          vtrim <- toupper(x["Additional.Others..Foam.is.too.less"])
          ifelse (vtrim == "NO", "(3)", (ifelse (vtrim == "YES", "(1)", NA)))
        }
        else
          return(x["Amount.of.form"])
      }
        
    }
  }
  else
    return(x["Amount.of.form"])
  
}           

uni.raw$Amount.of.form <- cbind(apply(uni.raw, 1, process))


## ------------------------------------------------------------------------------------------------------------------

## ----------------- combine 4 columns that is used to handle powder - hand/cup/scoop/plastic --------
uni.raw$Cup = sapply(uni.raw$Cup, function(v) {ifelse (v != "no", "Cup", v)})
uni.raw$Hands = sapply(uni.raw$Hands, function(v) {ifelse (v != "no" && v != "none", "Hands", v)})
uni.raw$Teaspoon = sapply(uni.raw$Teaspoon, function(v) {ifelse (v != "no", "Teaspoon", v)})
uni.raw$Plastic.Scoop = sapply(uni.raw$Plastic.Scoop, function(v) {ifelse (v != "no", "Scoop", v)})
uni.raw$Table.Spoon = sapply(uni.raw$Table.Spoon, function(v) {ifelse (v != "no", "Table", v)})

uni.raw$Cup.Hands.Teaspoon.Plastic.Scoop <- uni.raw$Cup
uni.raw$Cup.Hands.Teaspoon.Plastic.Scoop[is.na(uni.raw$Cup.Hands.Teaspoon.Plastic.Scoop)] <- uni.raw$Hands[is.na(uni.raw$Cup.Hands.Teaspoon.Plastic.Scoop)]
uni.raw$Cup.Hands.Teaspoon.Plastic.Scoop[is.na(uni.raw$Cup.Hands.Teaspoon.Plastic.Scoop)] <- uni.raw$Teaspoon[is.na(uni.raw$Cup.Hands.Teaspoon.Plastic.Scoop)]
uni.raw$Cup.Hands.Teaspoon.Plastic.Scoop[is.na(uni.raw$Cup.Hands.Teaspoon.Plastic.Scoop)] <- uni.raw$Plastic.Scoop[is.na(uni.raw$Cup.Hands.Teaspoon.Plastic.Scoop)]
uni.raw$Cup.Hands.Teaspoon.Plastic.Scoop[is.na(uni.raw$Cup.Hands.Teaspoon.Plastic.Scoop)] <- uni.raw$Table.Spoon[is.na(uni.raw$Cup.Hands.Teaspoon.Plastic.Scoop)]


## ----------------- combine 4 columns that is used to handle powder - hand/cup/scoop/plastic --------


# uni.raw <- as.data.frame(sapply(uni.raw, tolower))

# uni.raw1 <- uni.raw
uni.raw[c(3:129)][is.na(uni.raw[c(3:129)])] <- 0

uni.raw[c(3:129)] <- uni.raw[c(3:129)] /100

## -------------------------------------------------------------------------------------------------
## - check point to save the interim result so that can be replayed

save(uni.raw, file="D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unilever-Prep2.RData" )
#write.table(x=uni.raw, file="D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unilever-prep.csv", sep="\t" )

load("D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unilever-Prep2.RData")
#write.table(x=uni.raw, file="D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unilever-prep-csv.csv", sep="," )
## -------------------------------------------------------------------------------------------------


#---------- column "Smell..when.you.wear.them" to be prepared from teh following columns ---------------
# Smell..once.they.are.dry 42
# Smell..Whilst.ironing 72
# Smell..When.wearing.clothes.for.first.time.after.washing 68
# Smell..When.wearing.clothes.at.the.start.of.the.day 84.30
# Smell..When.wearing.clothes.at.the.end.of.the.day 68.86
#-----------------------------------------------------------
process <- function(x) {
  
  if (is.na(x["Smell..when.you.wear.them"])) 
  {
    if (!is.na(x["Smell..once.they.are.dry"]) ||
          !is.na(x["Smell..Whilst.ironing"]) ||
          !is.na(x["Smell..When.wearing.clothes.for.first.time.after.washing"]) ||
          !is.na(x["Smell..When.wearing.clothes.at.the.start.of.the.day"]) ||
          !is.na(x["Smell..When.wearing.clothes.at.the.end.of.the.day"]))
    {
      dtl <- data.table(Aa = c(x["Smell..once.they.are.dry"],
                               x["Smell..Whilst.ironing"],
                               x["Smell..When.wearing.clothes.for.first.time.after.washing"],
                               x["Smell..When.wearing.clothes.at.the.start.of.the.day"],
                               x["Smell..When.wearing.clothes.at.the.end.of.the.day"] ), 
                        Cc = c(1,1,1,1,1))
      dtlsort <- dtl[, length(Cc), by=Aa]
      
      
      return (dtlsort[order(-dtlsort$V1)][1]$Aa)
    }
    else
    {
      return(x["Smell..when.you.wear.them"])
    }
  }
  else
    return(x["Smell..when.you.wear.them"])
  
}           
uni.raw$Smell..when.you.wear.them <- cbind(apply(uni.raw, 1, process))
#------------------------------------------------------------------------------------------------------------


#---------- column "Smell..While.washing.the.clothes" to be prepared from teh following columns ---------------
# Smell..While.mixing.with.water.and.soaking 76.27
# Smell..Puttng.the.wet.clothes.on.the.line.for.drying 58.38
# Smell..When.drying.clothes.outdoors.on.the.line 75.60
#-----------------------------------------------------------
process <- function(x) {
  
  if (is.na(x["Smell..While.washing.the.clothes"])) 
  {
    if (!is.na(x["Smell..While.mixing.with.water.and.soaking"]) ||
          !is.na(x["Smell..Puttng.the.wet.clothes.on.the.line.for.drying"]) ||
          !is.na(x["Smell..When.drying.clothes.outdoors.on.the.line"]))
    {
      dtl <- data.table(Aa = c(x["Smell..While.mixing.with.water.and.soaking"],
                               x["Smell..Puttng.the.wet.clothes.on.the.line.for.drying"],
                               x["Smell..When.drying.clothes.outdoors.on.the.line"] ), 
                        Cc = c(1,1,1))
      dtlsort <- dtl[, length(Cc), by=Aa]
      
      
      return (dtlsort[order(-dtlsort$V1)][1]$Aa)
    }
    else
    {
      return(x["Smell..While.washing.the.clothes"])
    }
  }
  else
    return(x["Smell..While.washing.the.clothes"])
  
}           
uni.raw$Smell..While.washing.the.clothes <- cbind(apply(uni.raw, 1, process))
#------------------------------------------------------------------------------------------------------------


#-------------------- original contribution of data ------------------------------------
contrib.raw <- data.frame(cbind(colnames(uni.raw), NAs(uni.raw)), stringsAsFactors=FALSE)
colnames(contrib.raw) <- c("ColName", "NAs")

contrib.raw$NAs <- as.numeric(contrib.raw$NAs)
uni.rowcount <- nrow(uni.raw)
contrib.raw$NonNAs <-  uni.rowcount - contrib.raw$NAs
# contrib.raw$PercentNAs  <- round(contrib.raw$NAs/uni.rowcount * 100, digits=2)
contrib.raw$PercentData  <- round(contrib.raw$NonNAs/uni.rowcount * 100, digits=2)
#---------------------------------------------------------------------------------------------------------------------


#-------------------------------------------------- Update data for additional fields from their counterparts ------
for(i in 205:216) {
  uni.raw[,i] <- ifelse(is.na(uni.raw[,i]), uni.raw[,i+12], uni.raw[,i])
}
##------------------------------------------------------------------------------------------------------------------



## ---- "Powder.left.on.clothes" to "Skin.Sensitive.Issues" to be skiped for their Additional counterparts (204 to 216) and retain 217 229
uni.raw1 <- uni.raw[, -c(205:216)]

## ----- remove all "strength.of.smell" columns for thier "smell" columns which are well calibrated
uni.raw1 <- uni.raw1[,-grep(toupper("Strength.of.smell"),cbind(toupper(colnames(uni.raw1))))]



## remove these columns as they are consolidted
uni.raw1$Cup <- NULL
uni.raw1$Hands <- NULL
uni.raw1$Teaspoon <- NULL
uni.raw1$Plastic.Scoop <- NULL
uni.raw1$Table.Spoon <- NULL

save(uni.raw1, file="D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unilever-final-raw.RData" )


##Feature Selection based on the contribution of data points vs NAs in that particluar column
NAs <- function(x) {
  as.vector(apply(x, 2, function(x) length(which(is.na(x)))))
}
contrib <- data.frame(cbind(colnames(uni.raw1), NAs(uni.raw1)), stringsAsFactors=FALSE)
colnames(contrib) <- c("ColName", "NAs")

contrib$NAs <- as.numeric(contrib$NAs)
uni.rowcount <- nrow(uni.raw1)
contrib$NonNAs <-  uni.rowcount - contrib$NAs
contrib$PercentNAs  <- round(contrib$NAs/uni.rowcount * 100, digits=2)
contrib$PercentData  <- round(contrib$NonNAs/uni.rowcount * 100, digits=2)
summary(contrib$PercentNAs)

## ----- choose features with atleast 48% values in them or 52% NAs

confinal <- contrib[contrib$PercentData >= 48, c('ColName', 'PercentData')]
uni.raw2 <- uni.raw1[,c(confinal$ColName)]

dim(uni.raw2)

## Need to change column names with an X in front, hence substitute for Y (append a suffix on all column names hereafter--not done)
# colnames(uni.raw2) <- gsub("X","Y", cbind(colnames(uni.raw2)))

## -------------------------------------------------------------------------------------------------
save(uni.raw2, file="D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unilever-Data48.RData" )

#---85%
load("D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unilever-NA85.RData")
#-----70%
load("D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unilever-NA70.RData")
#-----30%
load("D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unilever-NA30.RData")

options(scipen=10)
write.table(x=uni.raw2, file="D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unilever-NA30.csv", sep=",", quote=FALSE)
options(scipen=0)
## -------------------------------------------------------------------------------------------------



uni.raw2[sapply(uni.raw2, is.character)] <- as.data.frame(lapply(uni.raw2[sapply(uni.raw2, is.character)], 
                                       as.factor))
uni.raw2[,1] <- as.character(uni.raw2[,1])
uni.raw2[,2] <- as.character(uni.raw2[,2])
str(uni.raw2, list.len=400)




## --- further investigation of data ---------------------

contrib2 <- data.frame(cbind(colnames(uni.raw2), NAs(uni.raw2)), stringsAsFactors=FALSE)
colnames(contrib2) <- c("ColName", "NAs")

contrib2$NAs <- as.numeric(contrib2$NAs)
uni.rowcount <- nrow(uni.raw2)
contrib2$NonNAs <-  uni.rowcount - contrib2$NAs
contrib2$PercentNAs  <- round(contrib2$NAs/uni.rowcount * 100, digits=2)


library(plyr)
cb1 <- contrib2[contrib2$PercentNAs < 10 & contrib2$PercentNAs != 0 ,]
colnames(cb1)[2] <- "10NAs"; colnames(cb1)[3] <- "10Data"; colnames(cb1)[4] <- "10NAs%"
cb2 <- contrib2[contrib2$PercentNAs < 20 & contrib2$PercentNAs != 0 ,]
colnames(cb2)[2] <- "20NAs"; colnames(cb2)[3] <- "20Data"; colnames(cb2)[4] <- "20NAs%"
cb3 <- contrib2[contrib2$PercentNAs < 30 & contrib2$PercentNAs != 0 ,]
colnames(cb3)[2] <- "30NAs"; colnames(cb3)[3] <- "30Data"; colnames(cb3)[4] <- "30NAs%"
cb4 <- contrib2[contrib2$PercentNAs < 40 & contrib2$PercentNAs != 0 ,]
colnames(cb4)[2] <- "40NAs"; colnames(cb4)[3] <- "40Data"; colnames(cb4)[4] <- "40NAs%"
cb5 <- contrib2[contrib2$PercentNAs < 50 & contrib2$PercentNAs != 0 ,]
colnames(cb5)[2] <- "50NAs"; colnames(cb5)[3] <- "50Data"; colnames(cb5)[4] <- "50NAs%"

cbb <- merge(cb1, cb2, all=TRUE, by="ColName")
cbb <- merge(cbb, cb3, all=TRUE, by="ColName")
cbb <- merge(cbb, cb4, all=TRUE, by="ColName")
cbb <- merge(cbb, cb5, all=TRUE, by="ColName")

data.frame(cbb[c("ColName")], 100-cbb[c("10NAs%", "20NAs%", "30NAs%", "40NAs%","50NAs%")])
### ---------------------------------------------------------------------------------------------


## -- let us start imputing this final data set and see how it goes - use missForest as KNN impute is only linux
## -- http://stackoverflow.com/questions/23288668/can-i-use-package-imputation-in-r-version-3-0-3

library("doParallel")
library("missForest")
library(randomForest)
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl, cores = detectCores() - 1)

##---cannot impute chr data types and we ignore all numeric data types too
uni.raw1.imp <- missForest(uni.raw2[,c(130:186)], verbose=TRUE, parallelize = "forests", maxiter = 8, ntree = 100)

df.train <- uni.raw2
df.train[,130:186] <- uni.raw1.imp$ximp

sum(is.na(df.train))
save(df.train, file="D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unilever-train1.RData" )


### ------------------------ Prediction part starts -------------------------------------------

require(caret)  # for modelling

## split training data into train batch and test batch
set.seed(786)
training.rows <- createDataPartition(df.train$Overall.Opinion, 
                                     p = 0.8, list = FALSE)
train.batch.orig <- df.train[training.rows, ]
test.batch.orig <- df.train[-training.rows, ]


train.batch <- train.batch.orig[,-c(1,2)]
test.batch <- test.batch.orig[,-c(1,2)]
train.bat <- train.batch.orig[,c(128:170)]


##---------------------------------------------------------------------------------------------------
#Multi-Class Summary Function
#Based on caret:::twoClassSummary
require(compiler)
multiClassSummary <- cmpfun(function (data, lev = NULL, model = NULL){
  #Load Libraries
  require(Metrics)
  require(caret)
  #Check data
  if (!all(levels(data[, "pred"]) == levels(data[, "obs"])))
    stop("levels of observed and predicted data do not match")
  #Calculate custom one-vs-all stats for each class
  prob_stats <- lapply(levels(data[, "pred"]), function(class){
    #Grab one-vs-all data for the class
    pred <- ifelse(data[, "pred"] == class, 1, 0)
    obs <- ifelse(data[, "obs"] == class, 1, 0)
    prob <- data[,class]
    
    #Calculate one-vs-all AUC and logLoss and return
    cap_prob <- pmin(pmax(prob, .000001), .999999)
    prob_stats <- c(auc(obs, prob), logLoss(obs, cap_prob))
    names(prob_stats) <- c('ROC', 'logLoss')
    return(prob_stats)
  })
  prob_stats <- do.call(rbind, prob_stats)
  rownames(prob_stats) <- paste('Class:', levels(data[, "pred"]))
  #Calculate confusion matrix-based statistics
  CM <- confusionMatrix(data[, "pred"], data[, "obs"])
  #Aggregate and average class-wise stats
  #Todo: add weights
  class_stats <- cbind(CM$byClass, prob_stats)
  class_stats <- colMeans(class_stats)
  
  #Aggregate overall stats
  overall_stats <- c(CM$overall)
  #Combine overall with class-wise stats and remove some stats we don't want
  stats <- c(overall_stats, class_stats)
  stats <- stats[! names(stats) %in% c('AccuracyNull',
                                       'Prevalence', 'Detection Prevalence')]
  #Clean names and return
  names(stats) <- gsub('[[:blank:]]+', '_', names(stats))
  return(stats)
}) 
#-----------------------------------------------------------------------------------------------------------------


# Using classification --------------------------------------

uni.logit.1 <- glm(Overall.Opinion ~ ., 
                       data = train.batch, family=binomial("logit"))
varImp(uni.logit.1, scale = TRUE)

uni.rf.1 <- train( Overall.Opinion ~ ., 
                   data = train.batch, method="rf" ,importance = TRUE, tuneGrid =  )

mtryGrid <- expand.grid(mtry = 150) # you can put different values for mtry
uni.rf.1 <- train(x = train.batch[,-c(170)],
               y = train.batch$Overall.Opinion,
               method = "rf",
               metric = "Kappa",
               ntree = 1500,
               tuneGrid = mtryGrid,
               importance = TRUE)

Sys.time()
uni.rf.pr <- predict(uni.rf.1, test.batch)
Sys.time()
confusionMatrix(uni.rf.pr, test.batch$Overall.Opinion)


####-------------------trainig propoer models begin-------------------


## Define control function to handle optional arguments for train function
## Models to be assessed based on largest absolute area under ROC curve
cv.ctrl <- trainControl(method = "repeatedcv", repeats = 2, number = 3,
                        summaryFunction = multiClassSummary,
                        classProbs = TRUE)

#####------------------------------KNN-----------------------------------------------

require('randomForest')
require('caret')
library("doParallel")
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl, cores = detectCores() - 1)

start.time <- Sys.time()
###...Relevent codes..........
set.seed(35)
knn.tune <- train(
  Overall.Opinion ~ .,
  data=train.batch,
  method='knn',
  tuneGrid=expand.grid(.k=1:30),
  metric='ROC',
  trControl=cv.ctrl)
#-------- time taken code 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken




#####------------------------------randomForest-----------------------------------------------

require('randomForest')
require('caret')
library("doParallel")
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl, cores = detectCores() - 1)

start.time <- Sys.time()
###...Relevent codes..........
rf.grid  <- expand.grid(mtry = 150) # you can put different values for mtry
rf.tune.nocomp <- train(x = train.batch[,-c(1:127,170)],
                  y = train.batch$Overall.Opinion,
                  method = "rf",
                  metric = "ROC",
                  ntree = 1100,
                 tuneGrid = rf.grid,
                 trControl = cv.ctrl)
#-------- time taken code 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

uni.rf.pr.no <- predict(rf.tune.nocomp, test.batch[,-c(1:127)])
Sys.time()
confusionMatrix(uni.rf.pr.no, test.batch$Overall.Opinion)
for(stat in c('Accuracy', 'Kappa', 'AccuracyLower', 'AccuracyUpper', 'AccuracyPValue',
              'Sensitivity', 'Specificity', 'Pos_Pred_Value',
              'Neg_Pred_Value', 'Detection_Rate', 'ROC', 'logLoss')) {
  plot(uni.rf.pr.no, metric=stat)
}

plot(uni.rf.pr.no, metric='ROC')



#####------------------------------Bayesian-----------------------------------------------

require('arm')
require('caret')
library("doParallel")
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl, cores = detectCores() - 1)
start.time <- Sys.time()
###...Relevent codes..........
bay.tune <- train(x = train.batch[,-c(170)],
                 y = train.batch$Overall.Opinion,
                 method = "bayesglm",
                 metric = "ROC",
                 trControl = cv.ctrl)
#-------- time taken code 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


#####------------------------------svm-----------------------------------------------
require('kernlab')
require('caret')
library("doParallel")
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl, cores = detectCores() - 1)

start.time <- Sys.time()
###...Relevent codes..........

set.seed(35)
svm.tune <- train(x = train.batch[,-c(170)],
                  y = train.batch$Overall.Opinion,
                  method = "svmRadial",
                  tuneLength = 9,
                  metric = "ROC",
                  trControl = cv.ctrl)
#-------- time taken code 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
                  


## Logistic regression model
glm.pred <- predict(glm.tune.5, test.batch)
confusionMatrix(glm.pred, test.batch$Fate)
## Boosted model
ada.pred <- predict(ada.tune, test.batch)
confusionMatrix(ada.pred, test.batch$Fate)
## Random Forest model
rf.pred <- predict(rf.tune, test.batch)
confusionMatrix(rf.pred, test.batch$Fate)
## SVM model 
svm.pred <- predict(svm.tune, test.batch)
confusionMatrix(svm.pred, test.batch$Fate)

## Logistic regression model (BLACK curve)
glm.probs <- predict(glm.tune.5, test.batch, type = "prob")
glm.ROC <- roc(response = test.batch$Fate,
               predictor = glm.probs$Survived,
               levels = levels(test.batch$Fate))
plot(glm.ROC, type="S")   
## Area under the curve: 0.8609 

## Boosted model (GREEN curve)
ada.probs <- predict(ada.tune, test.batch, type = "prob")
ada.ROC <- roc(response = test.batch$Fate,
               predictor = ada.probs$Survived,
               levels = levels(test.batch$Fate))
plot(ada.ROC, add=TRUE, col="green")    
## Area under the curve: 0.8759

## Random Forest model (RED curve)
rf.probs <- predict(rf.tune, test.batch, type = "prob")
rf.ROC <- roc(response = test.batch$Fate,
              predictor = rf.probs$Survived,
              levels = levels(test.batch$Fate))
plot(rf.ROC, add=TRUE, col="red") 
## Area under the curve: 0.8713

## SVM model (BLUE curve)
svm.probs <- predict(svm.tune, test.batch, type = "prob")
svm.ROC <- roc(response = test.batch$Fate,
               predictor = svm.probs$Survived,
               levels = levels(test.batch$Fate))
plot(svm.ROC, add=TRUE, col="blue")


cv.values <- resamples(list(Logit = glm.tune.5, Ada = ada.tune, 
                            RF = rf.tune, SVM = svm.tune))
dotplot(cv.values, metric = "ROC")










