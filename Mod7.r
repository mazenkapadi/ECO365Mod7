# Part 1


library(ggplot2)

# Function to visualize estimation of pi using Monte Carlo simulation
visualize_pi_estimation <- function(n) {
	# Generate n random points in a square
	x <- runif(n, min = -1, max = 1)
	y <- runif(n, min = -1, max = 1)

	# Calculate distance from each point to origin
	distance <- sqrt(x^2 + y^2)

	# Check if point is inside the circle (radius = 1)
	inside_circle <- distance <= 1

	# Create a data frame for ggplot
	df <- data.frame(x = x, y = y, inside_circle = inside_circle)

	# Plot points
	ggplot(df, aes(x, y, color = inside_circle)) +
		geom_point(alpha = 0.5, size = 1.5) +
		scale_color_manual(values = c("blue", "red"), labels = c("Outside Circle", "Inside Circle")) +
		theme_minimal() +
		ggtitle("Estimating Pi using Monte Carlo Simulation") +
		labs(x = NULL, y = NULL)
}

# Visualize estimation of pi with 1000 random points
visualize_pi_estimation(1000)




# Part 2

# Function to generate a list of group_size randomly chosen birthdays
birthday_sample <- function(group_size = 23) {
	# Generate group_size random birthdays
	birthdays <- sample(1:365, group_size, replace = TRUE)
	return(birthdays)
}

# Function to check if there are any duplicate birthdays
birthday_match <- function(birthdays) {
	# Check for duplicate birthdays
	any(duplicated(birthdays))
}

# Function to repeat birthday_match reps times and calculate proportion of matches
many_samples <- function(reps = 10, group_size = 23) {
	# Generate samples and check for matches
	samples <- replicate(reps, {
		birthdays <- birthday_sample(group_size)
		birthday_match(birthdays)
	})

	# Calculate proportion of matches
	estimated_prob <- mean(samples)

	return(estimated_prob)
}

library(ggplot2)

# Function to visualize relationship between sample size and probability of shared birthday
visualize_birthday_problem <- function() {
	# Vector of sample sizes
	sample_sizes <- seq(2, 50, by = 1)

	# Vector to store probabilities
	probabilities <- numeric(length(sample_sizes))

	# Calculate probability for each sample size
	for (i in seq_along(sample_sizes)) {
		probabilities[i] <- many_samples(1000, sample_sizes[i])
	}

	# Create a data frame for ggplot
	df <- data.frame(sample_size = sample_sizes, probability = probabilities)

	# Plot relationship between sample size and probability
	ggplot(df, aes(sample_size, probability)) +
		geom_line(color = "blue", size = 1) +
		geom_point(color = "blue", size = 2) +
		theme_minimal() +
		ggtitle("Relationship between Sample Size and Probability of Shared Birthday") +
		labs(x = "Sample Size", y = "Probability")
}

# Visualize relationship between sample size and probability of shared birthday
visualize_birthday_problem()
