test_that("SMED function runs with dummy data", {
  data(dummy_elution_matrix)
  data(dummy_train_complexes)
  
  # Minimal run to check infrastructure
  # Using n_fracs=1 and top_ppi=100 to keep it fast but more robust
  expect_error(
    suppressWarnings(
      results <- SMED(dummy_elution_matrix, dummy_train_complexes, 
                      fnMachine = "rf", n_fracs = 1, top_ppi = 100)
    ),
    NA # expect no error
  )
  
  expect_s3_class(results, "data.frame")
  expect_true(nrow(results) > 0)
  expect_named(results, c("InteractorA", "InteractorB", "Score"))
})
