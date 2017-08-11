# Federal

The assignment code takes in a latitude/longtiude of a location and adds a city variable and a census block variable. These are used to create more geographical linkages between datasets.


Following R Packages are Needed:
``` {r}
install.packages(dplyr)
install.packages(tidyr)
install.packages(stringr)
install.packages(data.table)
```

In the *scripts* folder, attached are scripts of R code that need to be included in some of the code. Here is which code needs which scripts:
``` {r}
# For database_ccd_financial.R
# This script renames and reorders the columns of the dataset. Should be included last after all the cleaning.
# Function(s) Included: rename_col(df)
#				    reorder_col(df)
scripts(clean_finance_col.R) 

# For master-school-list.R (change to actual name later)
# This script cleans the data to make it more compatible for the database.
# Function(s) Included: clean_master(df)
scripts(clean_master_schools_list.R) 
```