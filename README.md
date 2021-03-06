## Precious Metals  

> A naive review of the precious metals markets, key metrics and a template for a quick pulse on financial reporting  

### Dependencies  

Here are the libraries that this report relies on:   

```{r}  
require(Quandl,quietly = T)
require(quantmod,quietly = T)
require(dplyr,quietly = T)
require(ggplot2,quietly = T)
require(apexcharter,quietly = T)
require(DT,quietly = T)
require(rvest,quietly = T)
require(htmltools,quietly = T)
require(jsonlite,quietly = T)

```  

Additionally, you will need a quandl/data.nasdaq.com API to be able to download the data.  In production, this repository relies on `quandl_api.json` to hold the API token for accessing. Here is what my API looks like:  

```{json}  
{"QUANDL":"[QUANDL API KEY]",
 "AV": "[AV API KEY]"
}
```   

**NOTE**: The second API is not absolutely necessary.  All that is required is a replicated structure of the JSON as only the first element is used in the report.  


#### Future Releases  

Here are some nice to haves should time permit:  
1. Convert this workflow into a workable template for other asset classes   
2. Dynamic components of the report based on the parameter settings  
3. TBD  




