cut_num <- recode(tmsalary$cut, "'Ideal'=1; 'Not Ideal'=2;", 
                  as.factor.result=FALSE)

cha_num <- recode(tmsalary$channel, "'Independent'=1; 'Internet'=2; 'Mall'=3;", 
                  as.factor.result=FALSE)

store_num <- recode(tmsalary$store, "'Ashford'=1; 'Ausmans'=2; 
                    'Blue Nile'=3; 'Chalmers'=4;
                    'Danford'=5; 'Fred Meyer'=6;
                    'Goodmans'=7; 'Kay'=8; 'R. Holland'=9;
                    'Riddles'=10; 'University'=11; 'Zales'=12;", 
                    as.factor.result=FALSE)

tmsalary.matrix2 <- cbind(tmsalary.matrix, cut_num, cha_num, store_num)