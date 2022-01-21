# caterpillars-count-data

This is the repository of both raw and cleaned data for the (Caterpillars Count! project)[https://caterpillarscount.unc.edu].

As of 2022, the intended workflow for integrating newly submitted data is as follows:  

* *Update the raw data files*  
-- Source `update_catcount_data.R` and then run `updateCatCountData()`.  
-- This will grab the most recent table versions from https://caterpillarscount.unc.edu/backups and replace the older versions.  
* *Prepare any newly submitted data since the last update for cleaning*
-- Run `dataCleaning/reading_and_cleaning_new_data.r`
-- This will create a file called `flagged_dataset_YYYY-MM-DD.csv`.
* *Manually clean records*
-- Manually check any records in this file for which `status` is not "ok". The `flags` field indicates which information was identified as requiring checking. For example, a value of "ants numLeaves rareArthDiv" indicates that 1) either the number or length of ants, 2) the number of leaves, and 3) the diversity of rare arthropod groups were all unusual.
-- If the error and its appropriate fix can be inferred (e.g. 'daddylonglegs' was given a length of 30 mm where the user was clearly including leg length in the estimate, change length to 5 [the median length of a daddylonglegs in the dataset]), then 1) modify the necessary value(s), 2) describe what was done in the `actionTaken` column, and 3) change the `status` to "ok".
-- If there is a clear error in either the _number or length of leaves_, but no obvious solution, then change the `status` for every record pertaining to this Survey ID to "bad leaves".
-- If there is a clear error in the _arthropod quantity_ for a *single arthropod group* but no obvious solution, then change the `status` to "bad quantity".
-- If there is a clear error in the _arthropod length_ for a *single arthropod group* but no obvious solution, then change the `status` to "bad length".
-- If there is a clear error that pertains to the *entire survey*, e.g. the _total abundance or diversity of arthropods_, then change the `status` for every record pertaining to this Survey ID to "remove".
-- Finally, if upon examination, it is decided that these flagged values are still plausible and can be included in an analysis, change the `status` to "ok" and in the `actionTaken` column put "none".
-- This cleaning step is done when there are no longer any records with a `status` of "check".
* *Add new records to cleaned data file*
-- Append the file of newly cleaned records to the bottom of the most recent file named `cleaned_dataset_YYYY-MM-DD.csv`.
-- This is the file that should be used for any analyses.
-- Depending on the analysis, the user should `filter` out any undesirable records based on the `status` column.


...STILL IN PROGRESS...
