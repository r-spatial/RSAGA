#' Extended Argument Matching
#'
#' `match.arg.ext` matches `arg` against a set of candidate values as specified by `choices`; it extends [match.arg()] by allowing `arg` to be a numeric identifier of the `choices`.
#' @name match.arg.ext
#' @param arg a character string or numeric value
#' @param choices a character vector of candidate values
#' @param base numeric value, specifying the numeric index assigned to the first element of `choices`
#' @param several.ok logical specifying if `arg` should be allowed to have more than one element
#' @param numeric logical specifying if the function should return the numerical index (counting from `base`) of the matched `arg`ument, or, by default, its name
#' @param ignore.case logical specifying if the matching should be case sensitive
#' @details When `choices` are missing, they are obtained from a default setting for the formal argument `arg` of the function from which `match.arg.ext` was called.
#'
#' Matching is done using [pmatch()] (indirectly through a call to [match.arg()], so `arg` may be abbreviated.
#'
#' If `arg` is numeric, it may take values between `base` and `length(choices)+base-1`.  `base=1` will give standard 1-based R indices, `base=0` will give indices counted from zero as used to identify SAGA modules in library RSAGA.
#' @return If `numeric` is false and `arg` is a character string, the function returns the unabbreviated version of the unique partial match of `arg` if there is one; otherwise, an error is produced if `several.ok` is false, as per default. When `several.ok` is true and there is more than one match, all unabbreviated versions of matches are returned.
#'
#' If `numeric` is false but `arg` is numeric, `match.arg.ext` returns name of the match corresponding to this index, counting from `base`; i.e. `arg=base` corresponds to `choices[1]`.
#'
#' If `numeric` is true, the function returns the numeric index(es) of the partial match of `arg`, counted from `base` to `length(choices)+base-1`. If `arg` is already numeric, the function only checks whether it falls into the valid range from `arg` to `length(choices)+base-1` and returns `arg`.
#' @author Alexander Brenning
#' @seealso [match.arg()], [pmatch()]
#' @examples
#' # Based on example from 'match.arg':
#' require(stats)
#' center <- function(x, type = c("mean", "median", "trimmed")) {
#'   type <- match.arg.ext(type,base=0)
#'   switch(type,
#'          mean = mean(x),
#'          median = median(x),
#'          trimmed = mean(x, trim = .1))
#' }
#' x <- rcauchy(10)
#' center(x, "t")       # Works
#' center(x, 2)         # Same, for base=0
#' center(x, "med")     # Works
#' center(x, 1)         # Same, for base=0
#' try(center(x, "m"))  # Error
#' @keywords utilities
#' @export
match.arg.ext = function(arg, choices, base = 1, several.ok = FALSE,
    numeric = FALSE, ignore.case = FALSE)
{
    if (missing(choices)) {
        formal.args <- formals(sys.function(sys.parent()))
        choices <- eval(formal.args[[deparse(substitute(arg))]])
    }
    if (is.character(arg)) {
        if (ignore.case) {
            choices = tolower(choices)
            arg = tolower(arg)
        }
        res = match.arg(arg=arg,choices=choices,several.ok=several.ok)
        if (numeric)  res = which(choices %in% res) + base - 1
    } else if (is.numeric(arg)) {
        if ( (arg<base) | (arg>(length(choices)+base-1)) )
            stop("'arg' should be between ",base," and ",length(choices)+base-1)
        if (numeric) {
            res = arg
        } else {
            res = choices[arg - base + 1]
        }
    } else stop("'arg' should be numeric or character")
    return(res)
}
