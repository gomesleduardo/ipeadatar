# ipeadata

install.packages("devtools")
install.packages("getPass")
devtools::install_git("https://gitlab.com/ipeadata-pkg/ipeadata", 
                      credentials = git2r::cred_user_pass("xxxxx USER xxxxxx", 
                                                          getPass::getPass()))