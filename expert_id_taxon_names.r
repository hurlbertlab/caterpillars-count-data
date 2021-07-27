# Function for updating list of expertly identified taxa and their Family and Order

updateExpertClassification = function() {

  require(dplyr)
  require(stringr)
  require(taxize)
  
  expert = read.csv(list.files()[grepl("ExpertIdentification.csv", list.files())], header = TRUE, stringsAsFactors = FALSE)
  
  ranksOfInterest = c('species', 'complex', 'subgenus', 'genus', 'subtribe', 'tribe', 'family')
  
  uniqueNames = unique(expert[expert$Rank %in% ranksOfInterest, c('Rank', 'TaxonName')])

  classifiedNames = read.csv('classified_expert_identifications.csv', header = T)
  
  # Get list of names that has not already been classified previously
  newNamesToClassify = uniqueNames[!uniqueNames$TaxonName %in% classifiedNames$TaxonName & 
                                     !uniqueNames$Rank %in% c('kingdom', 'phylum', 'subphylum', 'class', 'subclass',
                                                              'infraorder', 'order', 'suborder', 'stateofmatter'), ]
  
  if (nrow(newNamesToClassify) > 0) {
  
    classify = data.frame(newNamesToClassify, Order = NA, Family = NA)
    
    problemNames = c()
    i = 1
    for (n in newNamesToClassify$TaxonName) {
      
      print(paste("Checking", i, "of", nrow(newNamesToClassify), "names"))
      info = classification(n, db = 'ncbi')[[1]]
      
      if (is.data.frame(info)) { # if the name returns a result from NCBI
        
        classify$Order[classify$TaxonName == n] = ifelse('order' %in% info$rank, info$name[info$rank == 'order'], NA)
        classify$Family[classify$TaxonName == n] = ifelse('family' %in% info$rank, info$name[info$rank == 'family'], NA)
        
      } else {  # if no NCBI match
        
        info2 = classification(n, db = 'itis')[[1]]
        
        if (is.data.frame(info2)) { # if the name returns a result from ITIS 
          
          classify$Order[classify$TaxonName == n] = ifelse('order' %in% info2$rank, info2$name[info2$rank == 'order'], NA)
          classify$Family[classify$TaxonName == n] = ifelse('family' %in% info2$rank, info2$name[info2$rank == 'family'], NA)
          
        } else { # if no ITIS match
          
          info3 = classification(n, db = 'eol')[[1]]
          
          if (is.data.frame(info3)) { # if the name returns a result from EOL
            
            classify$Order[classify$TaxonName == n] = ifelse('order' %in% info3$rank, info3$name[info3$rank == 'order'], NA)
            classify$Family[classify$TaxonName == n] = ifelse('family' %in% info3$rank, info3$name[info3$rank == 'family'], NA)
            
          } else { # if still no match after trying ITIS, NCBI, EOL, then assign NA's
            
            classify$Order[classify$TaxonName == n] = NA
            classify$Family[classify$TaxonName == n] = NA
            problemNames = c(problemNames, n)
            
          }
        }
      } # end if no ITIS match
      
      i = i + 1
      
    } # end for loop 
    
    updatedClassifiedNames = rbind(classifiedNames, classify)
    write.csv(updatedClassifiedNames, 'classified_expert_identifications.csv', row.names = F)
    
    return(list(problemNames = problemNames))
    # A large number of names are not matching with ITIs. Possibly use rinat package to query the name and get the info that way instead.
    
  } else {
    
    problemNames = classifiedNames$TaxonName[is.na(classifiedNames$Order) & is.na(classifiedNames$Family) &
                                               classifiedNames$Rank %in% ranksOfInterest]
    
    warning("No new names to classify. Here are the existing names without Order or Family matches.")
    
    return(list(problemNames = problemNames))
    
  }

}

