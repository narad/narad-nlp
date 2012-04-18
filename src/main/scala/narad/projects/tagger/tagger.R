

bp.train(fidx.file="/Users/narad/Desktop/narad-nlp/test.fidx.gz", pv.size=474, model.fun="tag.model")


tag.model <- function(pots, tagset) {
  nrwords  <- attr(pots, "slen")
  nrtags   <- attr(pots, "nrtags")
	m <- bpparser(exp(pots))
	vars <- sprintf("PVAR(%d)", 0:(nrwords-1))
	add.variables(m, vars, arity=nrtags)
	for ( idx in 1:(nrwords-1 ) ) {
	  fac <- paste(sprintf("pos(%d,", idx-1), "%d)")
		add.named1.factors(m, fac, vars[idx], default=1)
	}
	m
}

#  	fac <- sprintf("PFAC(%d,%d)", idx-1, idx)
#   add.named2.factors(m, fac, vars[idx], vars[idx+1], default=1)

for ( idx in 1:(nrwords-2 ) ) {
  fac <- paste(sprintf("pos(%d,", idx), "%d)")
  add.named2.factors(m, fac, vars)
}

	fac <- sprintf("pos(%d,%d)", idx-1, idx)
	add.named2.factors(m, fac, vars[idx], vars[idx+1], default=1)
}


The minimal pair to look at are the add.table[1234].factors and
add.named[1234].factors functions. The former takes the potential
table as an argument. The latter takes a name "pattern" and then looks
up names in the potential table (passed in, as you observed) in the
constructor. If the name passed to add.named1.factors is "foo(%d)" and
there are four values to its neighboring variable, the Named2Factor
constructor will look up the following potentials in the potential
table:
foo(0)
foo(1)
foo(2)
foo(3)

To pop up to the meta level, one reason I'd suggested trying a
linear-chain CRF is that you could replicate it in Mallet, CRF++, or
any of the better-documented CRF packages without worrying about the R
code.

facidx.loop(scan(gzfile("/Users/narad/Desktop/narad-nlp/tag.model.pv.gz")), "gzip -dc /Users/narad/Desktop/narad-nlp/train.fidx.gz", parse.fun=tag.decode, bp.iterations=40)


tag.decode <- function(pots, bp.iterations=40, damp.rate=0.99,
                        model.fun=tag.model, example=1, print=TRUE, ...) {
  model.fun <- match.fun(model.fun)
  m <- model.fun(pots, ...)
  conv <- run.bp(m, bp.iterations, damp.rate=damp.rate)
  beliefs <- potential.beliefs(m)
  nada <- 0
}


  res <- beliefs[grep("^unary\\(", names(beliefs))] >= 0.5
  res <- structure(as.integer(res), names=names(res))
  attr(res, "convergence") <- conv
  constits <- res[res == 1]
  labs <- beliefs[grep("^unaryLabel", names(beliefs))]
  lefts <- as.integer(sub("^unaryLabel[^\\(]+\\(([0-9]+),([0-9]+)\\).*", "\\1",  names(labs)))
  rights <- as.integer(sub("^unaryLabel[^\\(]+\\(([0-9]+),([0-9]+)\\).*", "\\2", names(labs)))
  offsets <- 1000 * lefts + rights
  if (print) {
      meta <- setdiff(names(attributes(pots)), c("names", "correct.score", "correct"))
      cat(sapply(meta, function(x) sprintf("@%s\t%s", x, attr(pots, x))), sep="\n")
  }
  tapply(labs, offsets, function(vars) {
    label <- sub("^unaryLabel([^\\(]+)\\(.*$", "\\1", names(labs)[which.max(vars)])
    left <- sub("^unaryLabel[^\\(]+\\(([0-9]+),([0-9]+)\\).*", "\\1",  names(vars))[1]
    right <- sub("^unaryLabel[^\\(]+\\(([0-9]+),([0-9]+)\\).*", "\\2",  names(vars))[1]
    pattern <- sprintf("unary\\(%1$s,%2$s\\)", left, right)
    if (length(grep(pattern, names(constits))) > 0 && print) {
      cat(sprintf("%d\t%s\t%s\t%s\n", example, left, right, label))
    }
  })
  cat("\n")
  res
}



  for ( idx in 1:length(morph.attr) ) {
    slot <- morph.attr[idx]
# Sets up the morphology variables and unary factors
    vars <- sprintf("var%s(%d)", names(slot), 1:slen)
    facs <- sprintf("%s(%d,%%d)", names(slot), 1:slen)
    add.variables(parser, vars, arity=slot)
    add.named1.factors(parser, facs, vars)

# Ternary links between link variables and pairs of pos/case variables
    mlinks <- sprintf("dep%s(%d,%d,%%d,%%d,%%d)", names(slot), nonroot.links$mom, nonroot.links$kid)
    add.named3.factors(parser, mlinks, vars[nonroot.links$mom], vars[nonroot.links$kid], nonroot.dep.var, default=1)
