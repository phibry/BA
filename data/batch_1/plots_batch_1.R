source("add/libraries.r") 
source("add/Functions.r")
# 5N 5L 10R####
load("data/batch_1/optim_5_5_10.rda")
cor(optim_5_5_10[,1:2])
plot(optim_5_5_10[,1:2])

cor(optim_5_5_10[,3:4])
plot(optim_5_5_10[,3:4])

cor(optim_5_5_10[,5:6])
cor(optim_5_5_10[,7:8])
cor(optim_5_5_10[,9:10])

cor(optim_5_5_10[,11:12])
plot(optim_5_5_10[,11:12])


# rect
plot_all_rect(optim_5_5_10, 10, "5N 5L 10R")
plot_by_layer_rect(optim_5_5_10, 10, "5N 5L 10R")
# normal
plot_all(optim_5_5_10, 10, "5N 5L 10R")
plot_by_layer(optim_5_5_10, 10, "5N 5L 10R")



# 5N 5L 20R####
load("data/batch_1/optim_5_5_20.rda")
# rect
plot_all_rect(optim_5_5_20, 20, "5N 5L 20R")
plot_by_layer_rect(optim_5_5_20, 20, "5N 5L 20R")
# normal
plot_all(optim_5_5_20, 20, "5N 5L 20R")
plot_by_layer(optim_5_5_20, 20, "5N 5L 20R")



# 5N 3L 30R####
load("data/batch_1/optim_5_3_30.rda")
# rect
plot_all_rect(optim_5_3_30, 30, "5N 3L 30R")
plot_by_layer_rect(optim_5_3_30, 30, "5N 3L 30R")
# normal
plot_all(optim_5_3_30, 30, "5N 3L 30R")
plot_by_layer(optim_5_3_30, 30, "5N 3L 30R")



# 6N 2L 40R####
load("data/batch_1/optim_6_2_40.rda")
# rect
plot_all_rect(optim_6_2_40, 40, "6N 2L 40R")
plot_by_layer_rect(optim_6_2_40, 40, "6N 2L 40R")
# normal
plot_all(optim_6_2_40, 40, "6N 2L 40R")
plot_by_layer(optim_6_2_40, 40, "6N 2L 40R")



# 10N 2L 20R####
load("data/batch_1/optim_10_2_10.rda")
# rect
plot_all_rect(optim_10_2_10, 10, "10N 2L 10R")
plot_by_layer_rect(optim_10_2_10, 10, "10N 2L 10R")
# normal
plot_all(optim_10_2_10, 10, "10N 2L 10R")
plot_by_layer(optim_10_2_10, 10, "10N 2L 10R")