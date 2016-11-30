#' @name knit_print.summary.lme
#' @title Pretty print contrast table for lme
#' @description
#'   Custom method for knitr/printr
#'
#` @examples
#`  d = as_tibble(expand.grid(pgroup = letters[1:2], app = LETTERS[1:3],
#`   dayc = seq(0,100,by = 10), Short = as.character(1:5), stringsAsFactors = TRUE ))
#`  d$Hours = rnorm(nrow(d), 5,2)
#` object = summary(lme(Hours~pgroup*dayc*app, random = ~1|Short, data = d,
#`                     weights = nlme::varIdent(form = ~1|app)))
#`  cat(knit_print.summary.lme(object, moredec = 1)
#' @param object of class \code{summary.lme}
#' @param ... \code{caption, moredec, bold_p, intercept_p}
#' @import knitr
#' @importFrom stats formula
#' @export
knit_print.summary.lme = function(object, ...) {

  get_opt = function(par, default){
    op = list(...)[[par]]
    if (is.null(op)) return(default)
    op
  }
  caption = get_opt("caption",NULL)
  moredec = get_opt("moredec",0)
  parameter = get_opt("parameter", as.character(formula(object))[2])
  bold_p = get_opt("bold_p", 0.05)
  intercept_p = get_opt("intercept.p", FALSE)

  xtTab = as.data.frame(object$tTable)
  sigp = xtTab[,"p-value"] < bold_p # cells that will be boldfaced
  if (!intercept_p) {
    sigp[1] = FALSE # intercept will never be shaded
    # Replace small significances, discarding p-value for (Intercept)
    xtTab[1,"p-value"] = 1 # we do not show it anyway, easier formatting
  }
  pval = format(zapsmall(xtTab[, "p-value"],4))
  pval[as.double(pval) < 0.0001] = "< .0001"
  xtTab[, "p-value"] = pval
  xtTab[,"t-value"] = round(xtTab[,"t-value"],1)
  if (ncol(xtTab) == 5)
    # not for gls
    xtTab[,"DF"] = as.integer(xtTab[,"DF"])
  if (any(wchLv <-
          (as.double(levels(xtTab[, "p-value"])) == 0))) {
    levels(xtTab[, "p-value"])[wchLv] = "< .0001"
  }

  # All I( in factors are replaced with "(" **This could be improved
  row.names(xtTab) =
    gsub("I\\(","(",dimnames(object$tTable)[[1]])

  # extract formula
  form = formula(object)
  fvars = sapply(all.vars(formula(object))[-1],
                 function(x) {is.numeric(object$data[[x]])})
  # categorical variables (might be strings)
  catvars = names(fvars[!fvars])
  levnames = ""
  if (length(catvars) != 0) {
    catlevels = sapply(catvars, function(x){
       lv = levels(as.factor(object$data[[x]]))[1]
       paste(x, lv, sep=" = ")
     })
    levnames = paste0(catlevels, collapse = ", ")
  }
  # numeric variables
  numvars = names(fvars[fvars])
  numnames = ""
  if (length(numvars) != 0) {
    numnames = paste0(numvars," = 0",collapse = ", ")
    levnames = paste(levnames, numnames, sep=", ")
  }
  if (is.null(caption)) {
    # TODO: Allow %s substitution
    if (inherits(object,"lme"))
      md = "Mixed model (lme)"   else
        if (inherits(object,"gls"))
          md = "Extended linear model (gls)"  else
            md = "Linear model"
          caption = paste(
            md," contrast table for `",
            parameter, "`  (model `",deparse(form),
            "`). The value in row `(Intercept)`  gives the reference value for `",
            levnames,"`.",sep = '')
          if (any(sigp))
            caption = paste0(caption, " Significant differences at p = ", bold_p,
                             " are printed **bold**.")
  }
  ndec = pmax(round(1 - log10(xtTab[,2] + 0.000001) + moredec),0)
  xtTab[,1] = formatC(round(xtTab[,1],ndec))
  xtTab[,2] = formatC(round(xtTab[,2],ndec))
  if (ncol(xtTab) == 5) {
    names(xtTab) = c("Value","StdErr","DF","t","p")
    pcol = 5
  } else {
    # gls misuse
    names(xtTab) = c("Value","StdErr","t","p")
    pcol = 4
  }
  # Only show intercept p/t when explicitely required
  if (!intercept_p) {
    xtTab[1,pcol - 1] = NA
    xtTab[1,pcol] = ''
  }
  xtTab[sigp, pcol] = paste0("**",xtTab[sigp, pcol],"**")
  # Do not show t and DF
  res = kable(xtTab[,-(3:4)], caption = caption, align = "r")
  res = paste(c("","",res), collapse = "\n")
  asis_output(res)
}

if (FALSE) {
  library(nlme)
  library(tibble)
  library(knitr)
  library(printr)
  set.seed(1)
  d = as_tibble(expand.grid(pgroup = letters[1:2], app = LETTERS[1:3], dayc = seq(0,100,by = 10), Short = as.character(1:5), stringsAsFactors = TRUE ))
  d$Hours = rnorm(nrow(d), 5,2)
  object = summary(lme(Hours~pgroup*dayc*app, random = ~1|Short, data = d,
                       weights = nlme::varIdent(form = ~1|app)))

  object = summary(lme(Hours~pgroup, random = ~1|Short, data = df))

  caption = NULL
  intercept_p = FALSE
  moredec = 1
  bold_p = 0.05

  cat(knit_print.summary.lme(object))
  cat(knit_print(object, moredec = 2, caption="m"))
}

