As <- paste0("A", 1:10)
Bs <- paste0("B", 1:10)
Cs <- paste0("C",1:10)
Ds <- paste0("D",1:10)

all_As <- NULL
all_Bs <- NULL
all_Cs <- NULL
all_Ds <- NULL

for(i in 1:50) {
  s_A <- sample(As, 5)
  s_B <- sample(Bs, 5)
  s_C <- sample(Cs, 5)
  s_D <- sample(Ds, 5)
  
  all_As <- c(all_As, s_A)
  all_Bs <- c(all_Bs, s_B)
  all_Cs <- c(all_Cs, s_C)
  all_Ds <- c(all_Ds, s_D)
}

min(table(all_As))
min(table(all_Bs))
min(table(all_Cs))
min(table(all_Ds))
