magnesium <- with(list(rt=c( 1,  9,  2, 1, 10, 1, 1,  90),
                            nt=c(40,135,200,48,150,59,25,1150),   
                            rc=c( 2, 23,  7, 1,  8, 9, 3, 118),      
                            nc=c(36,135,200,46,148,56,23,1150)
                            ),
                       data.frame(name = c("Morton","Rasmussen","Smith","Abraham","Feldstedt","Shechter","Ceremuzynski","LIMIT-2"),
                                  est = log((rt/nt)/(rc/nc)),
                                  se = sqrt(1/rt + 1/rc - 1/nt - 1/nc)
                                  ))

