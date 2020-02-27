library(testthat)
source('House/05_00_getSequence.R')
context('test sequence generation helper functions')

#---- Test Helper functions for getSequence ----
## variables
nona_sample_hn_seq <- readRDS("Data/100k_post_clean.RDS") %>%
  select(hn_1) %>% unlist() %>% as.numeric()
nona_sample_hn_seq <- nona_sample_hn_seq[!is.na(nona_sample_hn_seq)]
nona_sample_hn_seq <- nona_sample_hn_seq[1:50]

## getSequenceHead
print("TESTING getSequenceHead")
test_that('correct heads', {expect_equal(getSequenceHead(nona_sample_hn_seq, jump_size = 10), c(1, 22, 27, 43))})
test_that('correct heads', {expect_equal(getSequenceHead(nona_sample_hn_seq, check_parity = FALSE, jump_size = 10), c(1, 22, 43))})

## isHead
print("TESTING isHead")

## getDirectionalHeads
print("TESTING getDirectionalHeads")

dir_list = getDirectionalHeads(nona_sample_hn_seq)
test_that('returns correct dimensions', 
          {expect_equal(length(directions), length(nona_sample_hn_seq))})
test_that('correct heads - 15 is head', {expect_equal(dir_list$isHead[1], TRUE)})
test_that('correct heads - 18 is not head', {expect_equal(dir_list$isHead[27], FALSE)})
test_that('correct heads - 31 is head', {expect_equal(dir_list$isHead[22], TRUE)})
test_that('correct heads - 33 is not head', {expect_equal(directions[20], FALSE)})
test_that('correct heads - 9 is head', {expect_equal(directions[43], TRUE)})
test_that('correct heads - 7 is not head', {expect_equal(directions[45], FALSE)})
test_that('correct heads - 3 is not head', {expect_equal(directions[50], FALSE)})

## isDirectionalHead

## checkDistance

## sameParity
print("TESTING sameParity")

test_that('same parity', {expect_equal(sameParity(1, 2), FALSE)})
test_that('same parity', {expect_equal(sameParity(2, 2), TRUE)})

## withinJumpSize
print("TESTING sameParity")

test_that('jump size is working', {expect_equal(withinJumpSize(48, 20, 10), FALSE)})
test_that('jump size is working', {expect_equal(withinJumpSize(20, 48, 10), FALSE)})
test_that('jump size is working', {expect_equal(withinJumpSize(20, 48, 50), TRUE)})
test_that('jump size is working', {expect_equal(withinJumpSize(48, 20, 50), TRUE)})














