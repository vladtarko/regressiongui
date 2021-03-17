You can define new variables using operations on existing variables. For example, if `X` is a preexisting variable you can define `Y` as the base-10 logarithm of `X` in the following way:
  
- Variable name: `Y`
 
- Variable definition: `log10(X)`

Or you can define a category variable `GROUP` based on the values of `X` using one of these options:

- Variable name: `GROUP`

- Variable definition: `ifelse(X < 1000, "low", "high")`

- Variable definition: `case_when(X < 100 ~ "low", X > 100 & X < 1000 ~ "medium", X > 1000 ~ "high")`

Or you can define a new variable based on several other variables. E.g. centering a variable:

- Step 1:

  - Variable name: `mean_X`
  - Variable definition: `mean(X, na.rm = TRUE)`

- Step 2:
  - Variable name: `X_centered`
  - Variable definition: `X - mean_X`

**If the variable name already exists, the variable will be over-ridden / updated.**

**You can save the data with the new variables using the 'CSV' or 'Excel' buttons in the Data tab.**

[*Technical note*: You can use any valid R/tidyverse code to define the new/updated variable. Behind the scenes, the code in the variable definition goes inside a "mutate" command.]