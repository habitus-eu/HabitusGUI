rm(list = ls())


# =================================================================
load("~/projects/fontys/state_1_gui.RData")

ghome = loca$home$home
ghome_nbh = loca$home$home_nbh
gpalms = palms
gparticipant_basis = participant_basis
gschool_nbh = loca$school$school_nbh

load("~/projects/fontys/state_1_lynne.RData")

lhome = home
lhome_nbh = home_nbh
lpalms = palms
lparticipant_basis = participant_basis
lschool_nbh = school_nbh

print(table(ghome == lhome))
print(table(ghome_nbh == lhome_nbh))
print(table(gparticipant_basis == lparticipant_basis))
print(table(gschool_nbh== lschool_nbh))
print(table(gpalms == lpalms))

# At this point data is identical

# # #===========================================================
# load("~/projects/fontys/state_2_gui.RData")
# #
# gpalmsplus = palmsplus
# gpalmsplus_fields = palmsplus_fields
# 
# load("~/projects/fontys/state_2_lynne.RData")
# 
# lpalmsplus = palmsplus
# lpalmsplus_fields = palmsplus_fields
# 
# GP = colnames(gpalmsplus)
# LP = colnames(lpalmsplus)
# 
# 
# print(table(gpalmsplus == lpalmsplus))
# 
# print(table(gpalmsplus_fields[1:16,] == lpalmsplus_fields[1:16,]))
# # Only difference is that we define datai in palms_in_polygon instead of .
# 
# 
# # 
# # # #===========================================================
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
# # Identical
# 

# # # #===========================================================
# load("~/projects/fontys/state_4_gui.RData")
# # multimodal_fields and trajectory_locations are identical
# # main difference is in the trajectories object
# 
# gm2 = multimodal_fields
# gpp = palmsplus
# gt2 = trajectory_locations
# gtra = trajectories
# 
# print(dim(gtra))
# load("~/projects/fontys/state_4_lynne.RData")
# 
# lm2 = multimodal_fields
# lt2 = trajectory_locations
# ltra = trajectories
# lpp = palmsplus
# 
# print(dim(ltra))
# 
# print(table(gtra == ltra))
# print(table(gm2 == lm2))
# print(table(gt2 == lt2))
# # print(table(gpp == lpp))
# 
# 
# for (i in 1:nrow(gtra)) {
#   for (j in 1:ncol(gtra)) {
#     if (!all(is.na(gtra[i,j])) & !all(is.na(ltra[i,j]))) {
#       if (any(gtra[i,j] != ltra[i,j])) {
#         print(paste0(i, " - ", j))
#         print(colnames(ltra[i,j]))
#         print(paste0(gtra[i,j], " ", ltra[i,j]))
#         kkkk
#       }
#     }
#   }
# }
# kkkk

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


# #===========================================================
# load("~/projects/fontys/state_5_gui.RData")
# gm = multimodal
# 
# load("~/projects/fontys/state_5_lynne.RData")
# 
# lm2 = multimodal
# lm2 = lm2[order(names(lm2))]
# gm = gm[order(names(gm))]
# print(names(lm2) == names(gm))
# for (i in 1:nrow(gm)) {
#   for (j in 1:ncol(gm)) {
#     if (length(gm[i,j]) == 1) {
#       if (!is.na(gm[i,j]) & !is.na(lm2[i,j])) {
#         if (!(gm[i,j] == lm2[i,j])) {
#           print(paste0(i, " - ", j))
#           print(colnames(lm2[i,j]))
#           print(paste0(gm[i,j], " ", lm2[i,j]))
#         }
#       }
#     } else {
#       if (any(gm[i,j] != lm2[i,j], na.rm = TRUE)) {
#         print(paste0(i, " - ", j))
#         print(colnames(lm2[i,j]))
#         print(paste0(gm[i,j], " ", lm2[i,j]))
#         kkkk
#       }
#     }
#   }
# }
