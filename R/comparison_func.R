#' String comparison functions
#'
#' @param threshold threshold to use for the `jaro_winkler`, `lcs` or `jaccard`
#'   method when calculating distances using [stringdist()] from `stringdist`
#'   package.
#' @param ignore_case logical whether case should be ignored when comparing
#'   strings. Default is `FALSE`.
#' @param ignore_punct logical whether punct should be ignored when comparing
#'   strings. Default is `FALSE`.
#' @param ... further arguments to be passed to [stringdist()] function.
#'   Specifically, `p` for `jaro_winkler` or `q` for `jaccard` method.
#'
#' @return A comparison function.
#'
#' @rdname comparisons
#' @export cmp_identical
#'
#' @examples
#' x <- c("brb", "Awesome", "TOM")
#' y <- c("b.r.b", "Awesome", "tom")
#' cmp <- cmp_identical()
#' cmp(x, y) # FALSE TRUE FALSE
#' cmp <- cmp_identical(ignore_case = TRUE, ignore_punct = TRUE)
#' cmp(x, y) # TRUE FALSE TRUE
#'
#' @seealso
#' [stringdist::stringdist()] function for more details of the underlying string
#' distance methods. []
cmp_identical <- function(ignore_case = FALSE, ignore_punct = FALSE) {
  str_cmp("id", ignore_case = ignore_case, ignore_punct = ignore_punct)
}

#' @rdname comparisons
#' @export cmp_jaro_wrinkler
cmp_jaro_wrinkler <- function(threshold = 0.95, ignore_case = FALSE,
                              ignore_punct = FALSE, ...) {
  str_cmp("jw", ignore_case = ignore_case, ignore_punct = ignore_punct, ...)
}

#' @rdname comparisons
#' @export cmp_lcs
cmp_lcs <- function(threshold = 0.8, ignore_case = FALSE,
                    ignore_punct = FALSE) {
  str_cmp("lcs", ignore_case = ignore_case, ignore_punct = ignore_punct)
}

#' @rdname comparisons
#' @export cmp_jaccard
cmp_jaccard <- function(threshold = 0.8, ignore_case = FALSE,
                        ignore_punct = FALSE, ...){
  str_cmp("jaccard", ignore_case = ignore_case,
          ignore_punct = ignore_punct, ...)
}


str_cmp <- function(method = c("id", "jw", "lcs", "jaccard"), threshold = 0.9,
                    ignore_case = FALSE, ignore_punct = FALSE, ...) {
  method <- match.arg(method)
  function(x, y) {
    if (!missing(y)) {
      # Ensure input is a character
      x <- as_bare_character(x)
      y <- as_bare_character(y)

      if (ignore_case) {
        x <- str_to_lower(x)
        y <- str_to_lower(y)
      }
      if (ignore_punct) {
        x <- str_replace_all(x, "[[:punct:]]", "")
        y <- str_replace_all(y, "[[:punct:]]", "")
      }
      switch (method,
        id = x == y,
        lcs = {
          d <- stringdist(x, y, method = "lcs")
          maxd <- str_count(x) + str_count(y)
          1 - d/maxd
        },
        1 - stringdist(x, y, method = method, ...)
      )
    } else {
      if (method == "id") {
        x & !is.na(x)
      } else {
        (x > threshold) & !is.na(x)
      }
    }
  }
}

# Adapted from stringr package to ensure input is converted to a character
as_bare_character <- function(x) {
  if (is.character(x) && !is.object(x)) return(x)
  as.character(x)
}
