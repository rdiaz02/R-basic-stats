I teach an introductory course on statistics with R for biomedical
graduate students (PRSTR). The notes are in
/home/ramon/charlas-cursos/R-basic-stats/R-basic-stats.Rnw (knitr/LaTeX,
about 8260 lines). I start teaching in a few days.

TASK: check PART {N} of the notes (see the table below) for statistical
blunders: statements that are wrong, or seriously misleading for a
student. Read every line of that part, prose and code.

WHAT TO REPORT
- Wrong statistical claims (definitions, interpretations of p-values,
  CIs, tests, models, multiple testing, AIC, etc.).
- Statements about R output that do not match what the code actually
  produces (e.g., "these intervals are wider", "the p-value is about
  0.03", "the coefficient changes sign").
- Code that does not do what the surrounding text says it does.
- Internal contradictions between two places in the notes.
- Misleading advice, i.e., something a student would reasonably take
  away and that would lead them to do bad analysis.

WHAT NOT TO REPORT
- Grammar, spelling, punctuation, style, wording, LaTeX formatting.
  None of it, even if obvious.
- Deliberate simplifications. This is an intro course; a statement that
  omits nuance is fine unless a student would come away believing
  something false. If in doubt, report it but mark it as "minor".
- Suggestions for extra material or reorganization.
- Anything inside LaTeX comments (lines starting with %), FIXMEs, or
  commented-out code.
- Three chunks have eval=FALSE (lines 1702, 3679, 6833). Their code is
  not run and their output is deliberately not shown; do not report
  "output missing" around them.

VERIFY, DON'T REASON
When a claim is about R output or a numerical result, check it by
running the code, not by reasoning about what it should print. R is
installed with multcomp, HH, car, effects, nlme and ISwR. Chunks depend
on earlier ones, and the whole code runs in under 10 seconds, so the
simplest is to run everything once and then inspect the objects:

  1. Copy the data files (*.txt) and knitr-setup.R from the repository
     into your scratchpad directory. The code reads them with relative
     paths, so it must run from a directory that contains them.
  2. In the scratchpad, extract the code with
       knitr::purl("/home/ramon/charlas-cursos/R-basic-stats/R-basic-stats.Rnw",
                   output = "all.R")
     (the three eval=FALSE chunks come out commented).
  3. From the scratchpad, run
       pdf(NULL); source("all.R")
     and then inspect whatever objects you need.

Inline \Sexpr{} values in the text (e.g., lines 2273-2278) are computed
from R objects; evaluate them if the surrounding text depends on them.

Do not run R from the repository directory (it would write Rplots.pdf
there), do not knit the whole document, and do not write anything in
the repository.

Do NOT edit R-basic-stats.Rnw. Only report.

OUTPUT
A list ranked by severity (serious / moderate / minor). For each
finding:
- the line number(s) in R-basic-stats.Rnw (check with grep -n; do not
  cite from memory) and the section name,
- a short quote of the offending text,
- why it is wrong or misleading (and, if you ran code, what it gave),
- a suggested fix, in as few words as possible, keeping my wording
  where it can be kept.
If you find nothing serious in this part, say so plainly; do not pad
the list. Also say which claims you verified by running code.

Write the report to
/home/ramon/charlas-cursos/R-basic-stats/blunder-reports/blunder-report-part-{N}.md
(this is the one file you may write in the repository), and show it to
me too.

PARTS (line numbers are approximate; use the section headings)
- PART 1: lines 1-1092. Preamble, setup chunks (the p53 data are
  simulated at lines 187-196), Introduction through "Confidence
  intervals and p-values: a longer discussion" (t-test, one- vs
  two-tailed, power, bioequivalence, Bayesian inference, CIs and
  p-values).
- PART 2: lines 1093-2641. "Paired tests" through "Introduction to
  ANOVAs, regression, and linear models" (paired and one-sample
  t-tests, non-parametric procedures, non-independent data, symmetry).
- PART 3: lines 2642-3448. "Comparing more than two groups" and
  "Multiple comparisons: FWER and FDR" (one-way ANOVA, multiple
  comparisons with multcomp, FWER, FDR).
- PART 4: lines 3449-5190. "Two-way ANOVA" (interactions, order of
  factors, type of sums of squares, one observation per cell, more
  than two factors, multiple comparisons in two-way ANOVA,
  nonparametric alternatives).
- PART 5: lines 5191-6947. "Simple linear regression", "Multiple
  regression", ANCOVA, "Interactions, summary", "Diagnostics".
- PART 6: lines 6948-8264. "Variable and model selection" through the
  end (experimental design, covariate adjustment and causal comments,
  ratios, and the appendices on Anova tables and the two-way ANOVA by
  hand).
