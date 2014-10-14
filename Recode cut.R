cut_num <- recode(tmsalary$cut, "'Ideal'=1; 'Not Ideal'=2;", 
                  as.factor.result=FALSE)