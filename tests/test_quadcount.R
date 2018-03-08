


## generates a test plot titled 'pixels',   10 x 10 size
valNum = sample(c(0,1),replace = T, 150)
pixel10 <- data.frame(
  
  x   = rep(seq(1,15,1),each = 10)
  
  ,y   = rep(seq(10,1,-1),15)
  
  ,valNum
  
  ,valFact = factor(valNum)
  
)





## test the quadcount function

test_that('x-vector present in df',{
  
  
  
  
  test<-quadcount(pixel10,val='valNum',xbreak=5,ybreak=5) 

  expect_that(test,is_a('list'))
  
  expect_that(length(test),equals(2))
  
  
  
  
  })























