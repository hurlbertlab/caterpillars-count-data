# 
library(dplyr)
library(stringr)
library(taxize)


files = list.files()
expert = read.csv(files[grepl("ExpertIdentification.csv", files)], header = TRUE, stringsAsFactors = FALSE)

uniqueNames = unique(expert[, c('Rank', 'TaxonName')])
classify = data.frame(uniqueNames, Order = NA, Family = NA)

problemNames = c()
i = 1
for (n in uniqueNames$TaxonName) {
  
  print(paste("Checking", i, "of", nrow(uniqueNames), "names"))
  info = classification(n, db = 'itis')[[1]]
  
  if (!is.na(info)) {
    classify$Order[classify$TaxonName == n] = ifelse('order' %in% info$rank, info$name[info$rank == 'order'], NA)
    classify$Family[classify$TaxonName == n] = ifelse('family' %in% info$rank, info$name[info$rank == 'family'], NA)
  } else {
    classify$Order[classify$TaxonName == n] = NA
    classify$Family[classify$TaxonName == n] = NA
    problemNames = c(problemNames, n)
    
  }
  i = i + 1
}


write.csv(classify, 'classified_expert_identifications.csv', row.names = F)
