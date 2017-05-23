

load(file.path(getwd(),"data",
               "example-application-run-2017-05-16_18-35-13.Rdata"))

avgs <- ddply(output_storage, .(Var1, Var2, Var3, Var4, V5),
              summarize, ev = mean(total_value))
naive_avgs <- ddply(naive_output_storage, .(Var1, Var2, Var3, Var4, V5),
                    summarize, ev = mean(total_value))

naive_avgs[which.max(naive_avgs$ev),]

avgs[which.max(avgs$ev),]
avgs[which.max(naive_avgs$ev),]





## for the smart way of determining expected value of an allocation
## determine the value of the "best" allocation when the sales are
## simulated taking the mode of the posterior distribution to be
## the "true" parameter value.

## theta_1 ~ n(10,2) -> theta_1=10
## theta_2 ~ gamma(3*100/3,3) -> theta_2=100/3
##mu_kt = 10*exp(-3*t/100)

n_generations <- 100
storage <- list()
storage[[1]] <- data.frame(total_value = rep(NA, n_generations),
                            amt_sold = rep(NA, n_generations),
                            total_buy = rep(NA, n_generations),
                            total_demand = rep(NA, n_generations),
                            total_delivery_cost = rep(NA, n_generations),
                            #total_transfer_cost = rep(NA, n_generations),
                            full_price = rep(NA, n_generations),
                            marked_down = rep(NA, n_generations))
storage[[2]] <- data.frame(total_value = rep(NA, n_generations),
                           amt_sold = rep(NA, n_generations),
                           total_buy = rep(NA, n_generations),
                           total_demand = rep(NA, n_generations),
                           total_delivery_cost = rep(NA, n_generations),
                           #total_transfer_cost = rep(NA, n_generations),
                           full_price = rep(NA, n_generations),
                           marked_down = rep(NA, n_generations))
expected_buys <- matrix(NA, n_regions, n_time_periods)
for (j in 1:n_regions) {
  expected_buys[j, ] <- 10 * exp(-((1:n_time_periods)*3 )/ 100 )
}

#Go over time applying the fulfillment and transfer rules.  Also, calculate
#total costs as you go.


initial_allocation <- avgs[which.max(avgs$ev),1:5]

for (this_generation in 1:100) {
  print(paste0(this_generation," of ",n_generations))
  true_buys <- matrix(NA, n_regions, n_time_periods)
  for (j in 1:n_regions) {
    true_buys[j, ] <- rpois(n_time_periods, expected_buys[j, ])
  }
  
  current_inventory <- initial_allocation
  total_fulfilled <- 0
  total_delivery_cost <- 0
  
  for (time_point in 1:n_time_periods) {
    
    #If we're all out of inventory, we're done
    if (sum(current_inventory) == 0)
      next
    
    #Randomly select the set of houses that placed orders
    true_total_buy <- sum(true_buys[, time_point])
    order_hhs <- NULL
    for (j in 1:n_regions) {
      order_hhs <- c(order_hhs, sample(which(HH_samples$region == j),
                                       true_buys[j, time_point], replace = FALSE))
    }
    
    
    #Calculate distances from each ordering HH to each FC
    deliver_distances <- as.matrix(dist(rbind(fulfill_centers_locations[, c("lon", "lat")],
                                              HH_samples[order_hhs, c("lon", "lat")])))
    deliver_distances <- deliver_distances[1:n_fulfill_centers, 
                                           -(1:n_fulfill_centers)]
    deliver_distances <- matrix(as.numeric(deliver_distances), nrow = n_fulfill_centers)
    
    #Find the closest FC to each HH
    closest_delivery <- apply(deliver_distances, 2, which.min)
    
    #Count up how many need to be sent from each FC
    n_to_fulfill <- as.data.frame(table(closest_delivery))
    n_to_fulfill$closest_delivery <- as.character(n_to_fulfill$closest_delivery)
    n_to_fulfill <- merge(n_to_fulfill, 
                          data.frame(closest_delivery = as.character(1:n_fulfill_centers)),
                          all = TRUE)
    n_to_fulfill$Freq[is.na(n_to_fulfill$Freq)] <- 0
    n_to_fulfill <- n_to_fulfill[order(n_to_fulfill$closest_delivery),]
    
    #Make a table of current delivery strategy.
    current_delivery_strategy <- matrix(0, n_fulfill_centers, true_total_buy)
    current_delivery_strategy[matrix(c(closest_delivery, 1:true_total_buy),
                                     ncol = 2)] <- 1
    
    #Check if they all have enough.
    if (min(current_inventory - n_to_fulfill$Freq) < 0) { #Not enough to fulfill
      
      #Check if we're going to run completely out of inventory.
      if (sum(current_inventory) < sum(n_to_fulfill$Freq)) {  #We're going to run out of inventory at this time point
        
        #Randomly select which ones we'll actually fulfill
        random_delivery <- sample(1:true_total_buy, sum(current_inventory))
        
        #Update the objects
        true_total_buy <- sum(current_inventory)
        order_hhs <- order_hhs[random_delivery]
        deliver_distances <- deliver_distances[, random_delivery]
        deliver_distances <- matrix(as.numeric(deliver_distances), nrow = n_fulfill_centers)
        closest_delivery <- apply(deliver_distances, 2, which.min)
        n_to_fulfill <- as.data.frame(table(closest_delivery))
        n_to_fulfill$closest_delivery <- as.character(n_to_fulfill$closest_delivery)
        n_to_fulfill <- merge(n_to_fulfill, 
                              data.frame(closest_delivery = as.character(1:n_fulfill_centers)),
                              all = TRUE)
        n_to_fulfill$Freq[is.na(n_to_fulfill$Freq)] <- 0
        n_to_fulfill <- n_to_fulfill[order(n_to_fulfill$closest_delivery),]
        
        #Make a table of current delivery strategy.
        current_delivery_strategy <- matrix(0, n_fulfill_centers, true_total_buy)
        current_delivery_strategy[matrix(c(closest_delivery, 1:true_total_buy),
                                         ncol = 2)] <- 1
      }
      
      #Modify this to a viable delivery strategy
      fulfill_total <- apply(current_delivery_strategy, 1, sum)
      while(min(current_inventory - fulfill_total) < 0) {
        worst_fc <- which.min(current_inventory - fulfill_total)[1]
        move_to <- which(current_inventory > fulfill_total)
        to_move <- which.min(deliver_distances[move_to, current_delivery_strategy[worst_fc,] == 1])[1]
        move_to_index <- to_move %% length(move_to)
        if (move_to_index == 0) {
          move_to_index <- length(move_to)
        }
        fc <- move_to[move_to_index]
        hh <- which(current_delivery_strategy[worst_fc,] == 1)[ceiling(to_move / length(move_to))]
        current_delivery_strategy[fc, hh]  <- 1
        current_delivery_strategy[worst_fc, hh] <- 0
        fulfill_total <- apply(current_delivery_strategy, 1, sum)
      }
      
      #Optimize the fulfillment strategy
      #TODO!!!
      
    }
    
    #Calculate the cost of fulfilling under this strategy
    delivery_cost <- sum(alpha_1 + alpha_2 * 
                           as.numeric(deliver_distances)[as.numeric(current_delivery_strategy) == 1] +
                           alpha_3 *
                           floor(10 * as.numeric(deliver_distances)[as.numeric(current_delivery_strategy) == 1]))
    
    #Update inventory
    fulfill_total <- apply(current_delivery_strategy, 1, sum)
    current_inventory <- current_inventory - fulfill_total
    
    total_fulfilled <- total_fulfilled + fulfill_total
    total_delivery_cost <- total_delivery_cost + delivery_cost
    
  }
  
  amt_sold <- sum(total_fulfilled)
  total_demand <- sum(true_buys)
  total_value <- amt_sold * AUR * (total_buy < total_demand) +
    ((amt_sold * AUR + (total_buy - amt_sold) * AUR * exp(-delta * (total_buy - amt_sold))) * 
       (total_buy >= total_demand)) -
    total_delivery_cost 
  
  storage[[1]]$total_value[this_generation] <- total_value
  storage[[1]]$amt_sold[this_generation] <- amt_sold
  storage[[1]]$total_buy[this_generation] <- total_buy
  storage[[1]]$total_demand[this_generation] <- total_demand
  storage[[1]]$total_delivery_cost[this_generation] <- total_delivery_cost
  storage[[1]]$full_price[this_generation] <- amt_sold * AUR
  storage[[1]]$marked_down[this_generation] <- (total_buy - amt_sold) * AUR * exp(-delta * (total_buy - amt_sold)) * (total_buy >= total_demand)
  
}

####################################################################################
## do the same thing for the best allocation according to same posterior value
####################################################################################
initial_allocation <- naive_avgs[which.max(naive_avgs$ev),1:5]

for (this_generation in 1:n_generations) {
  
  true_buys <- matrix(NA, n_regions, n_time_periods)
  for (j in 1:n_regions) {
    true_buys[j, ] <- rpois(n_time_periods, expected_buys[j, ])
  }
  current_inventory <- initial_allocation
  total_fulfilled <- 0
  total_delivery_cost <- 0
  for (time_point in 1:n_time_periods) {
    
    #If we're all out of inventory, we're done
    if (sum(current_inventory) == 0)
      next
    
    #Randomly select the set of houses that placed orders
    true_total_buy <- sum(true_buys[, time_point])
    order_hhs <- NULL
    for (j in 1:n_regions) {
      order_hhs <- c(order_hhs, sample(which(HH_samples$region == j),
                                       true_buys[j, time_point], replace = FALSE))
    }
    
    
    #Calculate distances from each ordering HH to each FC
    deliver_distances <- as.matrix(dist(rbind(fulfill_centers_locations[, c("lon", "lat")],
                                              HH_samples[order_hhs, c("lon", "lat")])))
    deliver_distances <- deliver_distances[1:n_fulfill_centers, 
                                           -(1:n_fulfill_centers)]
    deliver_distances <- matrix(as.numeric(deliver_distances), nrow = n_fulfill_centers)
    
    #Find the closest FC to each HH
    closest_delivery <- apply(deliver_distances, 2, which.min)
    
    #Count up how many need to be sent from each FC
    n_to_fulfill <- as.data.frame(table(closest_delivery))
    n_to_fulfill$closest_delivery <- as.character(n_to_fulfill$closest_delivery)
    n_to_fulfill <- merge(n_to_fulfill, 
                          data.frame(closest_delivery = as.character(1:n_fulfill_centers)),
                          all = TRUE)
    n_to_fulfill$Freq[is.na(n_to_fulfill$Freq)] <- 0
    n_to_fulfill <- n_to_fulfill[order(n_to_fulfill$closest_delivery),]
    
    #Make a table of current delivery strategy.
    current_delivery_strategy <- matrix(0, n_fulfill_centers, true_total_buy)
    current_delivery_strategy[matrix(c(closest_delivery, 1:true_total_buy),
                                     ncol = 2)] <- 1
    
    #Check if they all have enough.
    if (min(current_inventory - n_to_fulfill$Freq) < 0) { #Not enough to fulfill
      
      #Check if we're going to run completely out of inventory.
      if (sum(current_inventory) < sum(n_to_fulfill$Freq)) {  #We're going to run out of inventory at this time point
        
        #Randomly select which ones we'll actually fulfill
        random_delivery <- sample(1:true_total_buy, sum(current_inventory))
        
        #Update the objects
        true_total_buy <- sum(current_inventory)
        order_hhs <- order_hhs[random_delivery]
        deliver_distances <- deliver_distances[, random_delivery]
        deliver_distances <- matrix(as.numeric(deliver_distances), nrow = n_fulfill_centers)
        closest_delivery <- apply(deliver_distances, 2, which.min)
        n_to_fulfill <- as.data.frame(table(closest_delivery))
        n_to_fulfill$closest_delivery <- as.character(n_to_fulfill$closest_delivery)
        n_to_fulfill <- merge(n_to_fulfill, 
                              data.frame(closest_delivery = as.character(1:n_fulfill_centers)),
                              all = TRUE)
        n_to_fulfill$Freq[is.na(n_to_fulfill$Freq)] <- 0
        n_to_fulfill <- n_to_fulfill[order(n_to_fulfill$closest_delivery),]
        
        #Make a table of current delivery strategy.
        current_delivery_strategy <- matrix(0, n_fulfill_centers, true_total_buy)
        current_delivery_strategy[matrix(c(closest_delivery, 1:true_total_buy),
                                         ncol = 2)] <- 1
      }
      
      #Modify this to a viable delivery strategy
      fulfill_total <- apply(current_delivery_strategy, 1, sum)
      while(min(current_inventory - fulfill_total) < 0) {
        worst_fc <- which.min(current_inventory - fulfill_total)[1]
        move_to <- which(current_inventory > fulfill_total)
        to_move <- which.min(deliver_distances[move_to, current_delivery_strategy[worst_fc,] == 1])[1]
        move_to_index <- to_move %% length(move_to)
        if (move_to_index == 0) {
          move_to_index <- length(move_to)
        }
        fc <- move_to[move_to_index]
        hh <- which(current_delivery_strategy[worst_fc,] == 1)[ceiling(to_move / length(move_to))]
        current_delivery_strategy[fc, hh]  <- 1
        current_delivery_strategy[worst_fc, hh] <- 0
        fulfill_total <- apply(current_delivery_strategy, 1, sum)
      }
      
      #Optimize the fulfillment strategy
      #TODO!!!
      
    }
    
    #Calculate the cost of fulfilling under this strategy
    delivery_cost <- sum(alpha_1 + alpha_2 * 
                           as.numeric(deliver_distances)[as.numeric(current_delivery_strategy) == 1] +
                           alpha_3 *
                           floor(10 * as.numeric(deliver_distances)[as.numeric(current_delivery_strategy) == 1]))
    
    #Update inventory
    fulfill_total <- apply(current_delivery_strategy, 1, sum)
    current_inventory <- current_inventory - fulfill_total
    
    total_fulfilled <- total_fulfilled + fulfill_total
    total_delivery_cost <- total_delivery_cost + delivery_cost
    
  }
  
  amt_sold <- sum(total_fulfilled)
  total_demand <- sum(true_buys)
  total_value <- amt_sold * AUR * (total_buy < total_demand) +
    ((amt_sold * AUR + (total_buy - amt_sold) * AUR * exp(-delta * (total_buy - amt_sold))) * 
       (total_buy >= total_demand)) -
    total_delivery_cost 
  
  storage[[2]]$total_value[this_generation] <- total_value
  storage[[2]]$amt_sold[this_generation] <- amt_sold
  storage[[2]]$total_buy[this_generation] <- total_buy
  storage[[2]]$total_demand[this_generation] <- total_demand
  storage[[2]]$total_delivery_cost[this_generation] <- total_delivery_cost
  #storage$total_transfer_cost[this_generation] <- total_transfer_cost
  storage[[2]]$full_price[this_generation] <- amt_sold * AUR
  storage[[2]]$marked_down[this_generation] <- (total_buy - amt_sold) * AUR * exp(-delta * (total_buy - amt_sold)) * (total_buy >= total_demand)

}

storage[[2]] <- data.frame(storage[[2]], naive_avgs[which.max(naive_avgs$ev),1:5])

mean(storage[[2]]$total_delivery_cost)
mean(storage[[1]]$total_delivery_cost)
