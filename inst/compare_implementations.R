rm(list = ls())


# # =================================================================
# load("~/projects/fontys/state_1_gui.RData")
# 
# ghome = home
# ghome_nbh = home_nbh
# gpalms = palms
# gparticipant_basis = participant_basis
# gschool_nbh = school_nbh
# 
# load("~/projects/fontys/state_1_lynne.RData")
# 
# lhome = home
# lhome_nbh = home_nbh
# lpalms = palms
# lparticipant_basis = participant_basis
# lschool_nbh = school_nbh
# 
# print(table(ghome == lhome))
# print(table(ghome_nbh == lhome_nbh))
# print(table(gparticipant_basis == lparticipant_basis))
# print(table(gschool_nbh== lschool_nbh))
# print(table(gpalms == lpalms))

# At this point data is identical

# #===========================================================
load("~/projects/fontys/state_2_gui.RData")
# 
gpalmsplus = palmsplus
gpalmsplus_fields = config_test[,c("name", "formula", "domain_field")]
load("~/projects/fontys/state_2_lynne.RData")
# 
lpalmsplus = palmsplus
lpalmsplus_fields = palmsplus_fields

GP = colnames(gpalmsplus)
LP = colnames(lpalmsplus)
# print(GP[GP %in% LP == FALSE])
# print(LP[LP %in% GP == FALSE])
# 
# print(table(gpalmsplus[,GP[GP %in% LP == TRUE]] == lpalmsplus[,LP[LP %in% GP == TRUE]]))
print(table(gpalmsplus_fields == lpalmsplus_fields, ))

# config_test is used instead of palmsplus_field
# difference comes from palmsplus, which has added columns "home"       "school"     "transport"  "home_nbh"   "school_nbh" "other"  
# however, the other columns are identical
kkk
# 
# # #===========================================================
# load("~/projects/fontys/state_3_gui.RData")
# gdays = days
# print(dim(gdays))
# load("~/projects/fontys/state_3_lynne.RData")
# ldays = days
# print(dim(ldays))
# 
# GP = colnames(gdays)
# LP = colnames(ldays)
# print(GP[GP %in% LP == FALSE])
# print(LP[LP %in% GP == FALSE])
# 
# for (i in 1:nrow(gdays)) {
#   for (j in 1:ncol(gdays)) {
#     if (!is.na(gdays[i,j]) & !is.na(ldays[i,j])) {
#       if (!(gdays[i,j] == ldays[i,j])) {
#         print(paste0(i, " - ", j))
#         print(colnames(ldays[i,j]))
#         print(paste0(gdays[i,j], " ", ldays[i,j]))
#       }
#     }
#   }
# }
# print(table(gdays == ldays))
# Minor differences in 68 values (rounding problems??), the 3412 other values are identical


# # #===========================================================
load("~/projects/fontys/state_4_gui.RData")
# multimodal_fields is the same
# trajectory_locations is the same, only order of rows is different
gtra = trajectories
print(dim(gtra))
load("~/projects/fontys/state_4_lynne.RData")
ltra = trajectories
print(dim(ltra))

print(table(gtra == ltra))
print(table(gtl == ltl))

for (i in 1:nrow(gtra)) {
  for (j in 1:ncol(gtra)) {
    if (!all(is.na(gtra[i,j])) & !all(is.na(ltra[i,j]))) {
      if (any(gtra[i,j] != ltra[i,j])) {
        print(paste0(i, " - ", j))
        print(colnames(ltra[i,j]))
        print(paste0(gtra[i,j], " ", ltra[i,j]))
        kkkk
      }
    }
  }
}

# ldays = days
# print(dim(ldays))
# 
# GP = colnames(gdays)
# LP = colnames(ldays)
# print(GP[GP %in% LP == FALSE])
# print(LP[LP %in% GP == FALSE])
# 
# for (i in 1:nrow(gdays)) {
#   for (j in 1:ncol(gdays)) {
#     if (!is.na(gdays[i,j]) & !is.na(ldays[i,j])) {
#       if (!(gdays[i,j] == ldays[i,j])) {
#         print(paste0(i, " - ", j))
#         print(colnames(ldays[i,j]))
#         print(paste0(gdays[i,j], " ", ldays[i,j]))
#       }
#     }
#   }
# }
# print(table(gdays == ldays))
# Minor differences in 68 values (rounding problems??), the 3412 other values are identical


#  
# lpalmsplus = palmsplus
# lpalmsplus_fields = palmsplus_fields
