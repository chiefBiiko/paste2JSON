# test paste2JSON

testthat::context('concatenation w/ partial conversion')

testthat::test_that('partial conversion', {
  
  # setup
  json <- jsonlite::toJSON(419L)
  
  # paste 2 JSON array
  testthat::expect_identical(paste2JSON(list('make money', json , 'way')),
                             structure('[["make money"],[419],["way"]]', 
                                       class='json'))
  
  # pass arg on to jsonlite::toJSON
  testthat::expect_identical(paste2JSON(list('make money', 419L, 'way'),
                                        auto_unbox=TRUE),  # gets passed on
                             structure('["make money",419,"way"]', 
                                       class='json'))
  
  # paste 2 JSON object
  testthat::expect_identical(paste2JSON(list('make money', json , 'way'), 
                                        keys=c('juju', 'fufu', 'zulu')),
                             structure(paste0('{"juju":["make money"],', 
                                               '"fufu":[419],', 
                                               '"zulu":["way"]}'), 
                                       class='json'))
  
})