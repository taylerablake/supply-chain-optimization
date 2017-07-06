set.seed(1977)

#Set a time range
n_time_periods <- 100

# load HH locations generated from munge script
load(file.path(getwd(),"cache","HH_samples.RData"))
n_regions <- length(unique(HH_samples$region))


# load HH locations generated from munge script
load(file.path(getwd(),"cache","fulfill_centers_locations.RData"))
n_fulfill_centers <- length(unique(fulfill_centers_locations$fc))

#Calculate distances among fulfillment centers
transfer_distances <- as.matrix(dist(fulfill_centers_locations[, c("lon", "lat")]))



#Define the posterior distribution of actual orders in different regions.
#Data distribution
#Y_{kt} ~ Poi(\mu_{kt})
#\mu_{kt} = \theta_{k1} * exp(-t / \theta_{k2})

#Posterior distribution of \theta
theta_1_means <- rnorm(n_regions, 10, 2)
theta_1_variances <- rgamma(n_regions, 1, 1)
theta_2_means <- rgamma(n_regions, 3 * n_time_periods / 3, 3)
theta_2_variances <- rgamma(n_regions, 5 * n_time_periods / 6, 5)



#For a gamma distribution
#mu = alpha / beta
#sigmasq = alpha / beta^2
#--> alpha = mu * beta
#--> sigmasq = mu * beta / beta^2
#--> sigmasq = mu / beta --> beta = mu / sigmasq
#--> alpha = mu^2 / sigmasq




#Generate a large number of samples from the posterior distributions
n_samples <- 10000
posterior_samples <- NULL
for (j in 1:n_regions) {
  alpha_1 <- theta_1_means[j]^2 / theta_1_variances[j]
  beta_1 <- theta_1_means[j] / theta_1_variances[j]
  alpha_2 <- theta_2_means[j]^2 / theta_2_variances[j]
  beta_2 <- theta_2_means[j] / theta_2_variances[j]
  posterior_samples <- rbind(posterior_samples,
                             data.frame(region = rep(j, n_samples),
                                        theta_1 = rgamma(n_samples, alpha_1, beta_1),
                                        theta_2 = rgamma(n_samples, alpha_2, beta_2)))
}




#Determine the buy. Set it at 90% of the predicted sales
#For each of the sampled theta values, we need to generate the distribution of
#actual sales by Monte Carlo.
x <- 1:n_time_periods
n_replications <- 10
sales_matrix <- matrix(NA, nrow(posterior_samples), n_replications)
for (i in 1:nrow(posterior_samples)) {
  if (i %% 10000 == 0)
    cat(i, "of", nrow(posterior_samples), " ")
  for (j in 1:n_replications) {
    mu <- sum(posterior_samples$theta_1[i] * 
                exp(-x / posterior_samples$theta_2[i]))
    sales_matrix[i, j] <- rpois(1, mu)
  }
}

total_sales <- matrix(0, n_samples, n_replications)
for (j in 1:n_regions) {
  total_sales <- total_sales + sales_matrix[posterior_samples$region == j,]
}
total_buy <- quantile(as.numeric(total_sales), .9)


#Set up the rules for fulfillment and transfer between DCs
#For transfer:
# 1. Calculate the difference between expected sales and current stock in each FC
# 2. If one FC is more than x% below expected sales find the FC that is the
#     highest amount above expected sales.  If this highest amount is x% over
#     expected sales, transfer.  Except, no transfers of fewer than y units
#For fulfillment:
# If all FCs have enough to fulfill the closest buys, do that.
# If not, perform an optimization


####################################################################################################
####################################################################################################
####################################################################################################


#Calculate the expected sales in each region
expected_sales <- rep(0, n_regions)
for (j in 1:n_regions) {
  expected_sales[j] <- mean(sales_matrix[posterior_samples$region == j,])
}

#Calculate distances from each FC to each region centroid
fc_to_region_distances <- matrix(0, n_fulfill_centers, n_regions)
for (i in 1:n_fulfill_centers) {
  for (j in 1:n_regions) {
    fc_to_region_distances[i, j] <- dist(matrix(
      c(fulfill_centers_locations$lon[i],
        fulfill_centers_locations$lat[i],
        mean(HH_samples$lon[HH_samples$region == j]),
        mean(HH_samples$lat[HH_samples$region == j])),
      ncol = 2, byrow = TRUE))
  }
}

#Figure out which region is served by each FC
fc_region_assignments <- apply(fc_to_region_distances, 2, which.min)

#Sum up to get expected sales to fulfill from each FC
fc_expected_fulfillment <- ddply(data.frame(fc = fc_region_assignments,
                                            expected_sales = expected_sales),
                                 .(fc), summarize,
                                 expected_sales = sum(expected_sales))



####################################################################################################
####################################################################################################

cl <- makePSOCKcluster(detectCores()-2)
registerDoParallel(cl)

#Implement an optimization on this.
set.seed(1978)
#Set up cost parameters
alpha_1 <- 1 #fixed cost of delivering a unit
alpha_2 <- .0001 #variable cost of delivering a unit 1 distance unit
alpha_3 <- .0002 #variable cost of delivering a unit 1 time unit
AUR <- 39.99 #Item sale price
delta <- 0.00069 #Discount factor per additional unit over demand (markdown rate)
#0.00069 implies that AUR is cut in half for an over-order of 1000 units

fc_expected_fulfillment <- ddply(data.frame(fc = fc_region_assignments,
                                            expected_sales = expected_sales),
                                 .(fc), summarize,
                                 expected_sales = sum(expected_sales))
initial_allocation <- round(total_buy * fc_expected_fulfillment$expected_sales /
                              sum(fc_expected_fulfillment$expected_sales))

allocations <- matrix(initial_allocation, nrow = 1)
allocation_list <- list()
allocation_list[[1]] <- seq(400, 700, length.out=4)
allocation_list[[2]] <- seq(500, 800, length.out = 4)
allocation_list[[3]] <- seq(300, 600, length.out = 4)
allocation_list[[4]] <- seq(600, 800, length.out = 4)

other_allocations <- as.matrix(expand.grid(allocation_list))
other_allocations <- apply(other_allocations, MARGIN=c(1,2),FUN=floor)
other_allocations <- cbind(other_allocations,
                           total_buy - apply(other_allocations, 1, sum))
allocations <- rbind(allocations, other_allocations)


options(warn = 2)
clusterExport(cl,c("allocations",
              "alpha_1",
              "alpha_2",
              "alpha_3",
              "AUR",
              "delta",
              "HH_samples",
              "fulfill_centers_locations"))
t1 <- Sys.time()
print(t1)
output_storage_list <- foreach(a=iter(allocations, by='row'),
                               .errorhandling='remove') %dopar% {
  
  output_storage <- NULL
  initial_allocation <- as.numeric(a)
  allocation_number <- which(duplicated(data.frame(rbind(initial_allocation,allocations)))) -1
  n_generations <- 100 #This must be less than or equal to n_samples
  value_storage <- data.frame(total_value = rep(NA, n_generations),
                              amt_sold = rep(NA, n_generations),
                              total_buy = rep(NA, n_generations),
                              total_demand = rep(NA, n_generations),
                              total_delivery_cost = rep(NA, n_generations),
                              #total_transfer_cost = rep(NA, n_generations),
                              full_price = rep(NA, n_generations),
                              marked_down = rep(NA, n_generations))
  
  for (this_generation in 1:n_generations) {
    
    #Generate the number of buys in each region across all time points
    expected_buys <- matrix(NA, n_regions, n_time_periods)
    true_buys <- matrix(NA, n_regions, n_time_periods)
    for (j in 1:n_regions) {
      this_index <- n_samples * (j - 1) + this_generation
      expected_buys[j, ] <- posterior_samples$theta_1[this_index] * 
        exp(-(1:n_time_periods) / posterior_samples$theta_2[this_index])
      true_buys[j, ] <- rpois(n_time_periods, expected_buys[j, ])
    }
    
    
    
    #Go over time applying the fulfillment and transfer rules.  Also, calculate
    #total costs as you go.
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
    
    #Record the total value of the strategy
    amt_sold <- sum(total_fulfilled)
    total_demand <- sum(true_buys)
    total_value <- amt_sold * AUR * (total_buy < total_demand) +
      ((amt_sold * AUR + (total_buy - amt_sold) * AUR * exp(-delta * (total_buy - amt_sold))) * 
         (total_buy >= total_demand)) -
      total_delivery_cost #-
    #total_transfer_cost
    value_storage$total_value[this_generation] <- total_value
    value_storage$amt_sold[this_generation] <- amt_sold
    value_storage$total_buy[this_generation] <- total_buy
    value_storage$total_demand[this_generation] <- total_demand
    value_storage$total_delivery_cost[this_generation] <- total_delivery_cost
    #value_storage$total_transfer_cost[this_generation] <- total_transfer_cost
    value_storage$full_price[this_generation] <- amt_sold * AUR
    value_storage$marked_down[this_generation] <- (total_buy - amt_sold) * AUR * exp(-delta * (total_buy - amt_sold)) * (total_buy >= total_demand)
    
  }
  
  value_storage$allocation_number <- which(duplicated(data.frame(rbind(initial_allocation,allocations)))) 
  #value_storage$allocation_number <- this_allocation
  output_storage <- rbind(output_storage, value_storage)
  output_storage
}
t2 <- Sys.time()
print(t2-t1)
cache("output_storage_list")


t1 <- Sys.time()
naive_output_storage_list <- foreach(a=iter(allocations, by='row')) %dopar% {
  
  output_storage <- NULL
  initial_allocation <- as.numeric(a)
  allocation_number <- which(duplicated(data.frame(rbind(initial_allocation,allocations)))) 
  n_generations <- 100 #This must be less than or equal to n_samples
  value_storage <- data.frame(total_value = rep(NA, n_generations),
                              amt_sold = rep(NA, n_generations),
                              total_buy = rep(NA, n_generations),
                              total_demand = rep(NA, n_generations),
                              total_delivery_cost = rep(NA, n_generations),
                              #total_transfer_cost = rep(NA, n_generations),
                              full_price = rep(NA, n_generations),
                              marked_down = rep(NA, n_generations))
  
  for (this_generation in 1:n_generations) {
    
    #Generate the number of buys in each region across all time points
    expected_buys <- matrix(NA, n_regions, n_time_periods)
    true_buys <- matrix(NA, n_regions, n_time_periods)
    for (j in 1:n_regions) {
      this_index <- n_samples * (j - 1) + 1
      expected_buys[j, ] <- posterior_samples$theta_1[this_index] * 
        exp(-(1:n_time_periods) / posterior_samples$theta_2[this_index])
      true_buys[j, ] <- rpois(n_time_periods, expected_buys[j, ])
    }
    
    
    
    #Go over time applying the fulfillment and transfer rules.  Also, calculate
    #total costs as you go.
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
    
    #Record the total value of the strategy
    amt_sold <- sum(total_fulfilled)
    total_demand <- sum(true_buys)
    total_value <- amt_sold * AUR * (total_buy < total_demand) +
      ((amt_sold * AUR + (total_buy - amt_sold) * AUR * exp(-delta * (total_buy - amt_sold))) * 
         (total_buy >= total_demand)) -
      total_delivery_cost #-
    #total_transfer_cost
    value_storage$total_value[this_generation] <- total_value
    value_storage$amt_sold[this_generation] <- amt_sold
    value_storage$total_buy[this_generation] <- total_buy
    value_storage$total_demand[this_generation] <- total_demand
    value_storage$total_delivery_cost[this_generation] <- total_delivery_cost
    #value_storage$total_transfer_cost[this_generation] <- total_transfer_cost
    value_storage$full_price[this_generation] <- amt_sold * AUR
    value_storage$marked_down[this_generation] <- (total_buy - amt_sold) * AUR * exp(-delta * (total_buy - amt_sold)) * (total_buy >= total_demand)
    
  }
  
  value_storage$allocation_number <- which(duplicated(data.frame(rbind(initial_allocation,allocations)))) 
  #value_storage$allocation_number <- this_allocation
  output_storage <- rbind(output_storage, value_storage)
  output_storage
}
t2 <- Sys.time()
print(t2-t1)
cache("naive_output_storage_list")






allocation_frame <- as.data.frame(allocations)
allocation_frame$allocation_number <- 1:nrow(allocation_frame)
output_storage <- merge(ldply(output_storage_list,data.frame),
                        allocation_frame)
naive_output_storage <- merge(ldply(naive_output_storage_list,data.frame),
                               allocation_frame)


save.image(file=file.path(getwd(),
                               "data",
                               paste0("example-application-run-",
                                      str_replace_all(str_replace_all(Sys.time()," ","_"),":","-"),".Rdata")))



avgs <- ddply(output_storage, .(Var1, Var2, Var3, Var4, V5),
                     summarize, ev = mean(total_value))
naive_avgs <- ddply(naive_output_storage, .(Var1, Var2, Var3, Var4, V5),
                     summarize, ev = mean(total_value))

naive_avgs[which.max(naive_avgs$ev),]

avgs[which.max(avgs$ev),]
avgs[which.max(naive_avgs$ev),]


expected <- rep(NA, n_regions)
mean(posterior_samples$theta_1)
for (j in 1:n_regions) {
  this_index <- n_samples * (j - 1) + this_generation
  expected_buys[j, ] <- posterior_samples$theta_1[this_index] * 
    exp(-(1:n_time_periods) / posterior_samples$theta_2[this_index])
  true_buys[j, ] <- rpois(n_time_periods, expected_buys[j, ])
}
