context("get_kolada_data")

test_that("getMetadata() is working", {
  df = getMetadata("municipality")
  expect_true(is.data.frame(df))
  expect_equal(df[6, "values.id"], "2506")
  expect_equal(df[6, "values.title"], "Arjeplog")
  df = getMetadata("kpi")
  expect_true(is.data.frame(df))
  expect_equal(df[3, "values.id"], "N00005")
  expect_equal(df[3, "values.title"], "Utjämningssystemet enl resultaträkning, kr/inv")
  df = getMetadata("ou")
  expect_true(is.data.frame(df))
  expect_equal(df[5, "values.id"], "V15E011401301")
  expect_equal(df[5, "values.title"], "Sverigefinska sk, Uppl Väsby")
  df = getMetadata("kpi_groups")
  expect_true(is.data.frame(df))
  expect_equal(df[2, "member_id"], "U28129")
  expect_equal(df[2, "member_title"], "Andel personer med en aktuell genomförandeplan.")
  df = getMetadata("municipality_groups")
  expect_true(is.data.frame(df))
  expect_equal(df[1, "member_id"], "1263")
  expect_equal(df[1, "member_title"], "Svedala")
})

test_that("fetch_all_given_entity() works",{
  output=fetch_all_given_entity("N00945",1860,c(2009,2007,2008))
  expect_equal(output[[5]][1],"T")
  expect_equal(round(output[[7]][1]),39)
  expect_true(is.data.frame(output))
})

test_that("fetch_given_kpiandyear() works",{
  output=fetch_given_kpiandyear("N00905",2009)
  expect_equal(output[[5]][5],"M")
  expect_equal(round(output[[7]][1]),190730)
  expect_true(is.data.frame(output))
})

test_that("fetch_given_muncipalityandyear() works",{
  output=fetch_given_muncipalityandyear(1860,2009)
  expect_equal(output[[5]][2],"T")
  expect_equal(round(output[[7]][1]),55)
  expect_true(is.data.frame(output))
})

test_that("fetch_given_kpiandmuncipality_id() works",{
  output=fetch_given_kpiandmuncipality_id("N00945",1860)
  expect_equal(output[[5]][2],"T")
  expect_equal(output[[3]][4],1999)
  expect_true(is.data.frame(output))
})

test_that("fetch_given_kpiandmuncipality_id() rejects errounous input",{
  expect_error(fetch_given_kpiandmuncipality_id(45,1860))
  expect_error(fetch_given_kpiandmuncipality_id("N00945","Ale"))
  })

test_that("fetch_given_muncipalityandyear() rejects errounous input",{
  expect_error(fetch_given_muncipalityandyear("Ale",2009))
})

test_that("fetch_given_kpiandyear() rejects errounous input",{
  expect_error(fetch_given_kpiandyear(45,2009))
})

test_that("fetch_all_given_entity() rejects errounous input",{
  expect_error(fetch_all_given_entity(945,1860,2009))
  expect_error((fetch_all_given_entity("N00945","Ale",2009)))
})