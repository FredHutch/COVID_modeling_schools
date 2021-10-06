#construct_contact_matrix
#Date: 8/1/2021
#Author: Mia Moore
# Purpose: Construct contact matrices based on population


load("../data/wa_counties_aggr_extend.Rdata")


balance.contact.matrix = function(contacts.per.capita, N){
  total.contacts = diag(N)%*%contacts.per.capita #ijth entry is all contacts that happen between class i and j in a given day (should balance)
  total.contracts.sym = total.contacts/2 + t(total.contacts)/2 #Ensure symmetry
  
  diag(1/N)%*%total.contracts.sym #Return contacts per capita which satisfies symmetry in our population
  
}

constuct_contact_matrix = function(county){

us.home = read.csv("../data/contact_matrices_152_countries/US_home.csv", header = F)
us.work = read.csv("../data/contact_matrices_152_countries/US_work.csv", header = F)
us.school = read.csv("../data/contact_matrices_152_countries/US_school.csv", header = F)
us.other = read.csv("../data/contact_matrices_152_countries/US_other.csv", header = F)

age.groupings = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4)
age.groupings2 = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4)

N.age = 4
age.groupings.mat = diag(rep(1, N.age))[,age.groupings]
age.groupings.mat2 = diag(rep(1, N.age))[,age.groupings2]

build.contact.matrix = function(M.full){
  #Increase contact with eldest age group to account for 80+ year-olds
  M.reduce = reduction.matrix%*%as.matrix(M.full)%*%t(age.groupings.mat2)
  balance.contact.matrix(M.reduce, aggr.age.distribution)
}



combined.age.distribution.raw=wa.counties.pop.extend[[county]]

#Account for the fact that contact matrices ignore 80+
combined.age.distribution = combined.age.distribution.raw[-17]
combined.age.distribution[16] = combined.age.distribution.raw[16] + combined.age.distribution.raw[17]

aggr.age.distribution = as.vector(age.groupings.mat2 %*% combined.age.distribution)
reduction.matrix = diag(1/aggr.age.distribution)%*%age.groupings.mat2%*%diag(combined.age.distribution)
list(C.home = build.contact.matrix(us.home),
C.work = build.contact.matrix(us.work),
C.school = build.contact.matrix(us.school),
C.other = build.contact.matrix(us.other)
)
}