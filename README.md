FAOSTAT Pre-Standardization Methodology
=======

This is the **github** repository for the development of the FAOSTAT
Pre-Standardization Methodology.


==============================================================================

**Documentation**


**Example**

Currently only the wheat example is available in the repository, but
the scripts can be changed for all other FBS groups.

There are three scripts for the wheat example:

1. *wheatDatamanipulation.R* - This scripts performs the data
manipulation from the SUA format to the desired structure for R.

2. *wheatPreStandard.R* - This is the script that performs the
pre-standardization.

3. *wheatCheck.R* - This script downloads the FBS data from FAOSTAT to
perform cross-validation checks.