const express = require('express');
const axios = require('axios');
const xgboost = require('xgboost-node');

const app = express();
const PORT = 3000;

app.get('/api/forecast', async (req, res) => {
  const { symbol, days } = req.query;
  const apiKey = 'YOUR_API_KEY';
  const url = `https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol=${symbol}&apikey=${apiKey}`;

  try {
    const response = await axios.get(url);
    const data = response.data['Time Series (Daily)'];
    const dates = Object.keys(data).reverse();
    const prices = dates.map(date => parseFloat(data[date]['4. close']));

    // Prepare data for XGBoost
    const trainData = prices.slice(0, prices.length - days);
    const testData = prices.slice(prices.length - days);

    // Train XGBoost model
    const dtrain = xgboost.DMatrix(trainData);
    const params = { max_depth: 3, eta: 0.1, silent: 1, objective: 'reg:squarederror' };
    const num_round = 100;
    const bst = xgboost.train(params, dtrain, num_round);

    // Predict future prices
    const dtest = xgboost.DMatrix(testData);
    const forecast = bst.predict(dtest);

    res.json({ dates, prices, forecast });
  } catch (error) {
    res.status(500).send('Error retrieving stock data');
  }
});

app.listen(PORT, () => {
  console.log(`Server running on port ${PORT}`);
});
