library(optmatch)


set.seed(20120111) # set this to get the exact same answers as I do
n <- 26 # chosen so we can divide the alphabet in half
W <- data.frame(w1 = rbeta(n, 4, 2), w2 = rbinom(n, 1, p = .33))

# nature assigns to treatment
tmp <- numeric(n)
tmp[sample(1:n, prob = W$w1^(1 + W$w2), size = n/2)] <- 1
W$z <- tmp

# for convenience, let's give the treated units capital letter names
tmp <- character(n)
tmp[W$z == 1] <- LETTERS[1:(n/2)]
tmp[W$z == 0] <- letters[(26 - n/2 + 1):26]
rownames(W) <- tmp

table(W$w2, W$z)
library(lattice) ; densityplot(W$w1, groups = W$z)

distances <- list()
distances$euclid <- match_on(z ~ w1 + w2, data = W, method = "euclidean")
distances$mahal <- match_on(z ~ w1 + w2, data = W)
propensity.model <- glm(z ~ w1 + w2, data = W, family =
                          binomial())
distances$propensity <- match_on(propensity.model)

distances$euclid@.Data
distances$mahal@.Data

distances$all <- with(distances, euclid + mahal + propensity) # add up distances
distances$median.caliper <- caliper(distances$all, median(distances$all))
distances$all.trimmed <- with(distances, all + median.caliper)


matches <- lapply(distances, function(x) {fullmatch(x, data = W)})


mahal.match <- pairmatch(distances$mahal, data = W)

print(mahal.match, grouped = T)

W.matched <- cbind(W, matches = mahal.match)
