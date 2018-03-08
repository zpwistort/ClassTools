


## test the quadcount function

test_that('x-vector present in df',{
  
  valNum = sample(c(0,1),replace = T, 150)
  pixel10 <- data.frame(
    
     x   = rep(seq(1,15,1),each = 10)
    ,y   = rep(seq(10,1,-1),15)
    ,valNum
    ,valFact = factor(valNum)
 )
  
  
  test<-quadcount(pixel10,val='valNum',xbreak=5,ybreak=5) 

  ## 1) test to make sure output is a list
  expect_that(test,is_a('list'))
  
  ## 2) test to make sure list is of length 2
  expect_that(length(test),equals(2))
  
  ## 3) test that pixelNumber values summed add up to the same sum from the original data.frame
  expect_that(sum(pixel10$valNum),equals(sum(test[[1]][2])))

  
  ## 4) test that number of boxes in output equals number of boxes in plot
  expect_that(length(test$boxNumber), equals(length(levels(test$id))))
  
  
  })























