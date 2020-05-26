var webpack = require('webpack');
var path = require('path');
var HtmlWebpackPlugin = require('html-webpack-plugin');
var SplitByPathPlugin = require('webpack-split-by-path');
var CopyWebpackPlugin = require('copy-webpack-plugin');

var r = file => path.resolve(__dirname, file[0]);

var config = {
  entry: r`src/index.tsx`,
  output: {
    path: r`dist`,
    filename: "[name]-[hash].js",
    chunkFilename: "[name]-[hash].js"
  },
  mode: "production",
  devtool: 'source-map',
  resolve: {
    extensions: [
      '.webpack.js',
      '.web.js',
      '.ts',
      '.tsx',
      '.js',
    ]
  },
  module: {
    rules: [
      { test: /\.css$/, use: ['style-loader', 'css-loader'] },
      { test: /\.(jpe?g|png|gif|woff|woff2|eot|ttf|svg)$/i, use: "file-loader" },
      { test: /\.tsx?$/, use: 'ts-loader' },
      { test: /\.pegjs$/, use: 'pegjs-loader' }
    ]
  },
  plugins: [

    new webpack.ProvidePlugin({
      $: "jquery",
      jQuery: "jquery",
      "window.jQuery": "jquery"
    }),

    // generates two bundles: One for ./src code, one for node_modules code.
    /*new SplitByPathPlugin([
      {
        name: 'node_modules-bundle',
        path: r`node_modules`
      }
    ],
      { manifest: 'app-entry' }
    ),*/

    // generates an index.html
    new HtmlWebpackPlugin({
      title: "Dfa Test Demo"
    }),

    new CopyWebpackPlugin({
      patterns: [
        {
          from: 'node_modules/monaco-editor/min/vs',
          to: 'vs',
        }
      ]
    })
  ]
};

module.exports = config;
