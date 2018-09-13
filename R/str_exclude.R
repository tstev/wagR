# Helper function
# Basically opposite of stringr::str_detect
str_exclude <- function(string, pattern) {
    string[!stringr::str_detect(string, pattern)]
}
