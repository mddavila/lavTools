context("test-modind")

HS.model <- "visual  =~ x1 + x2 + x3
             textual =~ x4 + x5 + x6
             speed   =~ x7 + x8 + x9"
fit <- cfa(HS.model, data = HolzingerSwineford1939)
random <- sample(1:10, 1)
lavout <- head(lavaan::modificationindices(fit, power = T, sort = T),
               n = random)
myout <- modind(fit, n = random)

test_that("digits works", {
    expect_equal(lavout, myout)
})
