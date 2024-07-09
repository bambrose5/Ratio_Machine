# Ratio Machine

Ratio Machine finds the ratio of analytes for 2 specific groups within a set of data ex. WT and KO. It then compares whether there is significance in the difference between the ratios of each group and automatically plots the result as a bar plot.
You can calculate as many ratios as you want and a p-adjusted value is calculated based on the number of comparisons. In the first part of the code, a table with the significant ratios is provided as output. Then the user can choose which
ratio they want plotted, corresponding to the row number of the output. The original data file must simply contain columns with the name "Group" with the group names for each row and then the analyte names with their intensities listed for each
sample. Ratio Machine does the heavy lifting to make as many comparisons as one can dream.
