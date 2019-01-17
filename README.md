# R package for Ipeadata API database 

## Instalation

The package is only available in **GitLab**. 

To install and load manually:

```{r eval=FALSE}
install.packages("devtools")
install.packages("getPass")
devtools::install_git("https://gitlab.com/ipeadata-pkg/ipeadata", 
                      credentials = git2r::cred_user_pass("YOUR GITLAB USER HERE", 
                                                          getPass::getPass()))
library(ipeadata)
````

## Help

For more details and examples, see [R Documentation](https://drive.google.com/open?id=1UPR2FZrslwlmQ448OOTNWNju5vkGBfFk).
