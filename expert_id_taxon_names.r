# Function for updating list of expertly identified taxa and their Family and Order

# --this function appends new names to 'classified_expert_identifications.csv' as it goes
#   so that if a connection cuts out, one can simply re-run the function and it will pick up
#   from where it left off.

# --names for which Family and Order are both NA should be looked into manually,
#   e.g. via Global Names Resolver 

# --(don't use "United States Species List" or "iNaturalist" as taxon source, as taxon info above genus not returned)

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
    
    i = 1
    for (n in newNamesToClassify$TaxonName) {
      
      print(paste("Checking", i, "of", nrow(newNamesToClassify), "names"))
      info = classification(n, db = 'ncbi')[[1]]
      
      if (is.data.frame(info)) { # if the name returns a result from NCBI
        
        Order = ifelse('order' %in% info$rank, info$name[info$rank == 'order'], NA)
        Family = ifelse('family' %in% info$rank, info$name[info$rank == 'family'], NA)
        
      } else {  # if no NCBI match
        
        info = classification(n, db = 'itis')[[1]]
        
        if (is.data.frame(info)) { # if the name returns a result from ITIS 
          
          Order = ifelse('order' %in% info$rank, info$name[info$rank == 'order'], NA)
          Family = ifelse('family' %in% info$rank, info$name[info$rank == 'family'], NA)
          
          
        } else { # if no ITIS match
          
          info = classification(n, db = 'eol')[[1]]
          
          if (is.data.frame(info)) { # if the name returns a result from EOL
            
            Order = ifelse('order' %in% info$rank, info$name[info$rank == 'order'], NA)
            Family = ifelse('family' %in% info$rank, info$name[info$rank == 'family'], NA)
            
          } else { 
            
            info = classification(n, db = 'gbif')[[1]]
            
            if (is.data.frame(info)) { # if the name returns a result from GBIF
              
              Order = ifelse('order' %in% info$rank, info$name[info$rank == 'order'], NA)
              Family = ifelse('family' %in% info$rank, info$name[info$rank == 'family'], NA)
              
            } else { 
              
              info = classification(n, db = 'wiki')[[1]]
              
              if (is.data.frame(info)) { # if the name returns a result from Wiki
                
                Order = ifelse('order' %in% info$rank, info$name[info$rank == 'order'], NA)
                Family = ifelse('family' %in% info$rank, info$name[info$rank == 'family'], NA)
                
              } else { # if still no match after trying ITIS, NCBI, EOL, GBIF, and Wiki then assign NA's
                
                Order = NA
                Family = NA
                
              }
            }
          }
        }
      } # end if no ITIS match
      
      i = i + 1
      
      write(paste(newNamesToClassify$Rank[newNamesToClassify$TaxonName == n],  # Rank
                  n,                                                           # TaxonName
                  Order,                                                       # Order
                  Family, sep = ','),                                          # Family
            file = "classified_expert_identifications.csv", append = T)
      
    } # end for loop 
  }
}

