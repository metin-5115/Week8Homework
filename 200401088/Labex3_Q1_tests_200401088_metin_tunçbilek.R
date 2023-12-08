library(testthat)
rm(list = ls())



source("Labex3_Q1_200401088_metin_tunçbilek.R")

# Test 1

test_that("spotify_token fonksiyonu geçerli bir çıktı döndürmeli", {
  token <- spotify_token()
  expect_named(token, c("status_code", "token"))
  expect_true(is.numeric(token$status_code))
  expect_true(length(token$token) > 0)
})

# Test 2

test_that("spotify_artist_top_tracks fonksiyonu bir liste döndürmeli", {
  result <- spotify_artist_top_tracks("36QJpDe2go2KgaRleHCDTp")
  expect_is(result, "list")
})

# Test 3

test_that("spotify_artist_top_tracks fonksiyonu status_code ve resultdf içeren bir çıktı döndürmeli", {
  result <- spotify_artist_top_tracks("36QJpDe2go2KgaRleHCDTp")
  expect_named(result, c("status_code", "resultdf"))
  expect_true(is.numeric(result$status_code))
  expect_true(is.data.frame(result$resultdf))
})

# Test 4

test_that("spotify_artist_top_tracks fonksiyonu boş artist_id ile uygun hata kodu döndürmeli", {
  result <- spotify_artist_top_tracks("")
  expect_equal(result$status_code, 400)
})

# Test 5

test_that("spotify_artist_top_tracks fonksiyonu bir veri çerçevesi döndürmeli", {
  result <- spotify_artist_top_tracks("36QJpDe2go2KgaRleHCDTp")
  expect_is(result$resultdf, "data.frame")
})

# Test 6

test_that("spotify_artist_top_tracks fonksiyonu boş olmayan bir veri döndürmeli", {
  result <- spotify_artist_top_tracks("36QJpDe2go2KgaRleHCDTp")
  expect_true(nrow(result$resultdf) > 0)
})

# Test 7

test_that("spotify_artist_top_tracks fonksiyonu 'name' sütunu boş olmamalı", {
  result <- spotify_artist_top_tracks("36QJpDe2go2KgaRleHCDTp")
  expect_true(all(result$resultdf$name != ""))
})

# Test 8

test_that("spotify_artist_top_tracks fonksiyonu belirli bir sayıda şarkı döndürmeli", {
  artist_id <- "36QJpDe2go2KgaRleHCDTp"
  result <- spotify_artist_top_tracks(artist_id)
  expect_length(result$resultdf$id, 10)
})