#Deprecated 8/1/2021

print("Warning this function is deprecated")

#Code for constructing the contact matrix
require(MASS)

#Import Raw Contact matrices
#us.all = read_excel("../data/contact_matrices_152_countries/MUestimates_all_locations_2.xlsx", sheet = "United States of America", col_names = F, .name_repair = "minimal")
#us.home = read_excel("../data/contact_matrices_152_countries/MUestimates_home_2.xlsx", sheet = "United States of America", col_names = F, .name_repair = "minimal")
#us.work = read_excel("../data/contact_matrices_152_countries/MUestimates_work_2.xlsx", sheet = "United States of America", col_names = F, .name_repair = "minimal")
#us.school = read_excel("../data/contact_matrices_152_countries/MUestimates_school_2.xlsx", sheet = "United States of America", col_names = F, .name_repair = "minimal")
#us.other = read_excel("../data/contact_matrices_152_countries/MUestimates_other_locations_2.xlsx", sheet = "United States of America", col_names = F, .name_repair = "minimal")

us.all = read.csv("../data/contact_matrices_152_countries/US_all.csv", header = F)
us.home = read.csv("../data/contact_matrices_152_countries/US_home.csv", header = F)
us.work = read.csv("../data/contact_matrices_152_countries/US_work.csv", header = F)
us.school = read.csv("../data/contact_matrices_152_countries/US_school.csv", header = F)
us.other = read.csv("../data/contact_matrices_152_countries/US_other.csv", header = F)

######Balance contact matrices (so contacts are symmetric)

#Procedure for balancing contact matrix
balance.contact.matrix = function(contacts.per.capita, N){
  total.contacts = diag(N)%*%contacts.per.capita #ijth entry is all contacts that happen between class i and j in a given day (should balance)
  total.contracts.sym = total.contacts/2 + t(total.contacts)/2 #Ensure symmetry
  
  diag(1/N)%*%total.contracts.sym #Return contacts per capita which satisfies symmetry in our population
  
}

#Specify KC age distribution
kc.age.distribution.by.decade = c(12, 11, 15, 17, 14, 13, 10, 6, 3)/100
kc.age.distribution.by.semidecade = c(rep(kc.age.distribution.by.decade[-9], each = 2)/2, 
                                      kc.age.distribution.by.decade[9])
kc.age.distribution.by.semidecade2 = kc.age.distribution.by.semidecade[-17]
kc.age.distribution.by.semidecade2[16] = kc.age.distribution.by.semidecade2[16] + kc.age.distribution.by.semidecade[17] 

wa.age.distribution.by.decade = c(12, 12, 14, 15, 12, 13, 12, 7, 3)/100
wa.age.distribution.by.semidecade = c(rep(wa.age.distribution.by.decade[-9], each = 2)/2, 
                                      wa.age.distribution.by.decade[9])

#Specify Required Age groups
N.age = 4 #0-19, 20-49, 50-69, 70+ (King county)
N.age.wa = 5 #0-19, 20-39, 40-59, 60-79, 80+ (Washington State)

#Combine age groups appropriately
age.groupings = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4)
age.groupings2 = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4)
age.groupings.wa = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5)
age.groupings.kc = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 5)
age.groupings.mat = diag(rep(1, N.age))[,age.groupings]
age.groupings.mat.wa = diag(rep(1, N.age.wa))[,age.groupings.wa]
age.groupings.mat.kc = diag(rep(1, N.age.wa))[,age.groupings.kc]
age.groupings.mat2 = diag(rep(1, N.age))[,age.groupings2]


kc.combined.age.distribution = as.numeric(kc.age.distribution.by.semidecade%*%t(age.groupings.mat))


#Attempt to convert from statistics given in Washington state age categories to King County (technically should be washington state age distribution)

#Intermediate step necessary for well-defined problem
washington.kc.conversion = age.groupings.mat.wa%*%diag(wa.age.distribution.by.semidecade)%*%t(age.groupings.mat.kc)

#Convert back to age groups we want
kc.conversion = age.groupings.mat%*%diag(kc.age.distribution.by.semidecade)%*%t(age.groupings.mat.kc)


washington.kc.conversion.inv = ginv(washington.kc.conversion)
full.washington.kc.conversion = diag(1/rowSums(kc.conversion))%*%kc.conversion%*%washington.kc.conversion.inv

build.contact.matrix = function(M.full){
  M.reduce = diag(1/kc.combined.age.distribution)%*%age.groupings.mat2%*%diag(kc.age.distribution.by.semidecade2)%*%t(M.full)%*%t(age.groupings.mat2)
  balance.contact.matrix(M.reduce, kc.combined.age.distribution)
}

C.home = build.contact.matrix(us.home)
C.work = build.contact.matrix(us.work)
C.school = build.contact.matrix(us.school)
C.other = build.contact.matrix(us.other)

N.age.groups = N.age
N.risk.groups = 1


#Washington Data (5/11/2020)
Hospitalizations.by.age.wa = c(0.7, 10, 24.6, 40.9, 23.8)
Deaths.by.age.wa = c(0, .01, .09, .38, .52)

death.risk.by.age = full.washington.kc.conversion %*% Deaths.by.age.wa

rel.death.risk.by.age = death.risk.by.age/max(death.risk.by.age)

hospitalization.risk.by.age = full.washington.kc.conversion %*% Hospitalizations.by.age.wa

rel.hospitalization.risk.by.age = hospitalization.risk.by.age/max(hospitalization.risk.by.age)