library(pwr)

# all power analyses were two-sample, one-way t-tests (type and alternative arguments)

# alpha = 0.05, effect size = 30%
power1 <- pwr.t.test(n=NULL, power = 0.8, d = 0.3, sig.level = 0.05,
                     type = "two.sample", alternative = "greater")
plot(power1)


# alpha = 0.01, effect size = 30%
power2 <- pwr.t.test(n=NULL, power = 0.8, d = 0.3, sig.level = 0.01,
                     type = "two.sample", alternative = "greater")
plot(power2)


# alpha = 0.05, effect size = 50%
power3 <- pwr.t.test(n=NULL, power = 0.8, d = 0.5, sig.level = 0.05,
                     type = "two.sample", alternative = "greater")
plot(power3)


# alpha = 0.01, effect size = 50%
power4 <- pwr.t.test(n=NULL, power = 0.8, d = 0.5, sig.level = 0.01,
                     type = "two.sample", alternative = "greater")
plot(power4)

