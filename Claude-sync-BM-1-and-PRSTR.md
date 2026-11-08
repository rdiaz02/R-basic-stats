I teach two classes, using R for statistics, that are very similar in content. One is in this directory and the main file is R-basic-stats.Rnw. This is taught starting in November. Call this PRSTR. This is more advanced, to students of computational biology and bioinformatics.

The other is in /home/ramon/clases/bm-1/2026-2027/Stats, and the main files are Lesson-2.Rnw, Lesson-3.Rnw (also Lesson-1 and Lesson-4, but as you'll see below these do not concern us here.) This is taught in September and October, and uses R-commander too. Call this BM-1.


What I often do is, when I am done with BM-1 and I am going to start PRSTR, I introduce, in the files for PRSTR, the improvements and fixes I did in BM-1. Likewise the other way around: when I am done with PRSTR, I introduce the changes in BM-1, so that the following year the improvements are there.

But I don't always manage to do that. Sometimes I just make notes like "FIXME: do this as it was done in PRSTR" and the like.

Moreover, I can't always copy things verbatim, since in PRSTR I do not use R Commander.

I am about to teach PRSTR. I've been incorporating the changes I made in BM-1 (going through the commit history of the repo and seeing what new things I introduced, and where). I think I am fully done. As well, I've added material in PRSTR that I will introduce into BM-1 next year (but that will be next year, and I already made a note in BM-1).

I want to double check all improvements and changes in BM-1 that could be relevant for PRSTR are already in PRSTR.

When doing so, please, don't be nitpicky (i.e., minor grammar, punctuation, etc, issues can be left out). Only focus on:

a) are the changes in BM-1 that could matter for PRSTR in PRSTR already?

b) if there is any major blunder, or a seriously misleading thing, let me know too.

Going from oldest to newest commit naively might not always work; why? because in a commit at time t I might add something, that I later expand at commit t+1. So this needs to be done smartly (that is what I tried). Alternatively, one could compare the actual, final products as they stand now, in so far as all the commits in the BM-1 are, well, what eventually brings us to the final, finished product as of now. What I'd do if I could is compare just the current final text. That is what matters. But this requires wading through what is and what is not Rcmdr, etc. So I went via the commits, and asked myself if that had to be in the PRSTR notes. But, actually, I started from the most recent, not oldest commit, and then looked at the final version of the notes, copied the text and then if other worked had been started before for the same material I could ignore as that section or paragraph had already been introduced.

Is the task clear? And should I use Fable as the model, or is Opus 5.5. enough for this?
