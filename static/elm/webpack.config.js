const path = require('path');
const webpack = require('webpack');
const CompressionPlugin = require("compression-webpack-plugin");


const config = {
  entry: path.join(__dirname, 'dev-elm'),
  output: {
    path: path.resolve(__dirname, "../js"),
    filename: "elm-bundle.js",
    library: 'Elm'
  },
  cache: false,
  plugins: [
    new webpack.DefinePlugin({
      'process.env.NODE_ENV': '"production"'
    }),
    new webpack.optimize.UglifyJsPlugin({
      mangle: {
        reserved: ['Elm']
      },
      compress: {
        warnings: false, // Suppress uglification warnings
        pure_getters: true,
        unsafe: true,
        unsafe_comps: true,
        screw_ie8: true
      },
      output: {
        comments: false,
      },
      exclude: [/\.min\.js$/gi] // skip pre-minified libs
    }),
    new webpack.IgnorePlugin(/^\.\/locale$/, [/moment$/]),
    new CompressionPlugin({
      asset: "[path].gz[query]",
      algorithm: "gzip",
      test: /\.js$|\.css$|\.html$/,
      threshold: 10240,
      minRatio: 0
    })
  ],
};

module.exports = config;
