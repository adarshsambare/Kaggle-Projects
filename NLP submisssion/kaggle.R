# Nlp copy paste code

library(irlba)

complete.term.matrix <- as.matrix(t(corpus_tdm))

a2 = NULL
for (i1 in 1:ncol(complete.term.matrix))
{ if (sum(complete.term.matrix[, i1]) == 0) {a2 = c(a2, i1)} }


complete.term.matrix[1:10, 1:20]
dim(complete.term.matrix)


# Fixing incomplete cases
incomplete.cases <- which(!complete.cases(complete.term.matrix))
complete.term.matrix[incomplete.cases,] <- rep(0.0, ncol(complete.term.matrix))

complete_irlba <- irlba(t(complete.term.matrix), nv = 50, maxit = 6)

