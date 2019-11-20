module.exports = {
  module: {
    rules: [{
      test: /\.elm$/,
      exclude: [/elm-stuff/, /node_modules/],
      use: {
        loader: 'elm-webpack-loader',
        options: { optimize: true }
      }
    }]
  },
  devServer: {
    hot: true,
  }
};
