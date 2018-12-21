-- stack --resolver lts-12.0 script

-- I didn't actually write any more code to solve this one. My solution for
-- part 1 had the ability to print out the registers when certain ones changed,
-- so I did that and looked for a pattern. Turns out it was computing the sum
-- of the divisors of the number in one of the registers. So I grabbed that
-- number and put it in Wolfram Alpha. :)

main = print 32188416
