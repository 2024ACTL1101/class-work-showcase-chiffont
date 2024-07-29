
## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```r
# Load data from CSV file
amd_df <- read.csv("AMD.csv")
# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)
amd_df <- amd_df[, c("date", "close")]
```

#### Plotting the Data
Plot the closing prices over time to visualize the price movement.
```r
plot(amd_df$date, amd_df$close,'l')
```

### Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```r
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
for (i in 1:nrow(amd_df)) { # Considering the case where the rows read as NA
curr_price <- amd_df[i,2]
if (i == nrow(amd_df)) { # Selling all shares on the last day
amd_df[i,3] <- 'sell'
amd_df[i,4] <- accumulated_shares * curr_price accumulated_shares <- 0
} else if (previous_price == 0 | curr_price < previous_price) { amd_df[i,3] <- 'buy'
amd_df[i,4] <- -(curr_price * share_size)
accumulated_shares <- accumulated_shares + share_size
  }
  amd_df[i,5] <- accumulated_shares
  previous_price <- curr_price
}

```


### Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```r
# Define start and end date
start_day <- as.Date("2023-01-01") end_day <- as.Date("2024-01-01")
# Filter the dataframe to only include the defined trading period
amd_df <- amd_df[amd_df$date >= start_day & amd_df$date <= end_day ,]
```


### Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```r
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA # Corrected column name amd_df$accumulated_shares <- 0 # Initialize if needed for tracking
# Initialize variables for trading logic
previous_price <- 0
share_size <- 100 # defining size of the shares such that code can easily be manipulated should share size change
accumulated_shares <- 0
for (i in 1:nrow(amd_df)) {
curr_price <- amd_df[i,2] # current price is defined by column 2
if (i == nrow(amd_df)) { # case where all shares are sold on the last day
amd_df[i,3] <- 'sell'
amd_df[i,4] <- accumulated_shares * curr_price
accumulated_shares <- 0 # accumulated shares is set to 0 to avoid
carry forward information from step 1 that is not applicable to this period
} else if (previous_price == 0 | curr_price < previous_price) { # logic included for the reasoning behind buying stock based on its previous price relative to today's
amd_df[i,3] <- 'buy'
amd_df[i,4] <- -(curr_price * share_size) accumulated_shares <- accumulated_shares + share_size
  }
  amd_df[i,5] <- accumulated_shares
  previous_price <- curr_price
}
# Define variables for trading logic
total_profit_or_loss <- 0
capital_invested <- 0
costs_proceeds <- 0
# Create loop
for (i in 1:nrow(amd_df)) { if (!is.na(amd_df[i,3])) {
if (i == nrow(amd_df) && amd_df[i,3] != 'sell') {
amd_df[i,3] <- 'sell'
amd_df[i,4] <- amd_df[i,2] * amd_df[i,5]
total_profit_or_loss <- total_profit_or_loss + (amd_df[i,2] *
amd_df[i,5])
          amd_df[i,5] <- 0
} else {
total_profit_or_loss <- total_profit_or_loss + amd_df[i,4] if (amd_df[i,3] == "buy") {
capital_invested <- capital_invested - amd_df[i,4] }
}
} }
print(total_profit_or_loss) ## [1] 532197
print(capital_invested) ## [1] 1177759
    # Calculate ROI
roi <- ((total_profit_or_loss) / (capital_invested)) * 100 print(roi)
## [1] 45.18726
```

### Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```r
# Choosing Option 1 (sell half of your holdings)
# Initialise columns for trade type, cost proceeds, accumulated shares and average purchasing price
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA
amd_df$accumulated_shares <- 0 amd_df$average_purchase_price <- 0
#Initialise variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
no_of_purchases <- 0 # To help with finding the average, +1 will be added each time trade_type is a buy
average_purchase_price <- 0
proportion_to_buy_at <- 0 [i,2]>=1.2 a capital_invested <- 0 total_profit_or_loss <- 0
# Forming Loop
for (i in 1:nrow(amd_df))
curr_price <- amd_df[i,2]
if (previous_price == 0) { # conditions for base case, where first day
indicates a purchase
amd_df[i,3] <- 'buy'
amd_df[i,4] <- curr_price * share_size * -1
no_of_purchases <- no_of_purchases + 1
capital_invested <- capital_invested - amd_df[i,4] average_purchase_price <- average_purchase_price + curr_price total_profit_or_loss <- total_profit_or_loss + amd_df[i,4] accumulated_shares <- accumulated_shares + share_size
} else if (i == nrow(amd_df)) { # this row accounts for the last day during the period, where all stocks will be sold
amd_df[i,3] <- 'sell'
amd_df[i,4] <- curr_price * accumulated_shares accumulated_shares <- 0
total_profit_or_loss <- total_profit_or_loss + amd_df[i,4]
}
else if (curr_price >= 1.2 * average_purchase_price) { # this row implements the profit taking strategy, where 1/2 of the stock is sold if the current price is 20% higher than the average purchase price
amd_df[i,3] <- 'sell'
accumulated_shares <- accumulated_shares / 2
amd_df[i,4] <- curr_price * accumulated_shares
total_profit_or_loss <- total_profit_or_loss + accumulated_shares *
curr_price
} else if (previous_price > curr_price) { # this row allows for purchases
of stock to occur if the current price is lower than the previous price
amd_df[i,3] <- 'buy'
amd_df[i,4] <- curr_price * share_size * -1
accumulated_shares <- accumulated_shares + share_size
capital_invested <- capital_invested - amd_df[i,4]
total_profit_or_loss <- total_profit_or_loss + amd_df[i,4] average_purchase_price <- ((average_purchase_price * no_of_purchases) + amd_df[i,2]) / (no_of_purchases + 1)
no_of_purchases <- no_of_purchases + 1
}
amd_df[i,6] <- average_purchase_price previous_price <- curr_price amd_df[i,5] <- accumulated_shares
}
roi <- ((total_profit_or_loss) / (capital_invested)) * 100 print(roi)
## [1] 17.9816 print(total_profit_or_loss) ## [1] 35688.99 print(capital_invested)
## [1] 198475
```


### Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```r
# Printing ROI from step 4
total_profit_or_loss <- 532197
capital_invested <- 1177759
ROI <- 45.18726
print(total_profit_or_loss)
## [1] 532197 print(ROI)
## [1] 45.18726
# Printing ROI from step 5 (profit taking strategy implemented)
total_profit_or_loss <- 35688.99
capital_invested <- 198475
ROI <- 17.9816
print(total_profit_or_loss) ## [1] 35688.99
print(ROI)
## [1] 17.9816
```
Discussion:
The outputted ROI for the strategy used in step 4 was 45%, while the outputted ROI for the implemented profit taking strategy was 17%, while the reported profits for the original strategy and the profit taking strategy were respectively $532,197 and $35,688.99. ROI decreased by 63%, whilst profit decreased by 94%. Thus, it can be concluded, with high certainty, that our ratios did not improve with the profit taking strategy for the given period. The chosen period was the start of 2023 to the start of 2024, where the AMD’s stock price rose almost exponentially after a fall from 2022. This coincides with our findings, where it is made clear that a ‘playing it safe’ method of selling stock the moment a small increase in its value occurs (i.e the profit taking strategy) is one that is not ideal for periods of time where a stock’s value is increasing steadily at a fast rate.
The increase in its value throughout 2023 could be attributed to AMD CEO Lisa Su’s presentation during the CES 2023 Keynote, where new developments in the technology produced by AMD were announced. This includes the addition of the use of AI technology embedded within the core of its Ryzen 7040 series of notebook processors, as well as an improved mobile GPU and Smartshift RSR technology, thus optimising the performance of their newer line of products and increasing their attractiveness on the market. At the start of 2023, this could potentially have garnered the attention of investors, who could have decided to buy AMD stock based on the prediction that AMD would generate more revenue with its improved technology, thus explaining its initial rise in the new year. Furthermore, as the products were released onto the market in June 2023, AMD would generate more revenue and the value of its stock would increase and reach its peak for 2023. Thus, its increase in value throughout 2023 unfortunately resulted in a lower ROI for the profit taking strategy.




