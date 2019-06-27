# UKBiodiversity

The functions in UKBiodiversity can be used to recreate the analyses carried out in the associated paper "Complexity of biodiversity change revealed through long-term trends of invertebrates, bryophytes and lichens" by Outhwaite *et al*.

To carry out the analyses, you will first need to download the data used.  This can be downloaded from an entry in the EIDC repository which can be found  [here](https://catalogue.ceh.ac.uk/documents/0ec7e549-57d4-4e2d-b2d3-2199e1578d84).

To download the data you will need to register with the website, the data can then be downloaded directly.

The following R code can be used to download the UKBiodiversity R package from GitHub:

```{r, echo = TRUE}

library(devtools)

# Install the R package from GitHUb
install_github("CharlieOuthwaite/UKBiodiversity")

# Load the package
library(UKBiodiversity)

```

For more information on using the functions to recreate the analyses, please read the vignette.

```{r, echo = TRUE}

vignette('UKBiodiversity')


```
