test_that("app connections react to reconnects and release owned connections", {
  path <- tempfile(fileext = ".duckdb")
  con <- gnaf_connect(path)
  gnaf_init(con)
  gnaf_disconnect(con)
  on.exit(unlink(path), add = TRUE)
  app <- gnaf_app(run = FALSE)
  owned <- NULL
  shiny::testServer(app, {
    expect_null(current_con())
    session$setInputs(db_path = path)
    session$setInputs(connect = 1L)
    expect_true(connection_info()$connected)
    expect_true(DBI::dbIsValid(current_con()))
    owned <<- current_con()
  })
  expect_false(DBI::dbIsValid(owned))
})

test_that("app sessions leave caller-owned connections open", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  app <- gnaf_app(con = con, run = FALSE)
  for (i in 1:2) {
    shiny::testServer(app, {
      expect_true(DBI::dbIsValid(current_con()))
    })
    expect_true(DBI::dbIsValid(con))
  }
})
