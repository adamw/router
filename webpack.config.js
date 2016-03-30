var path = require('path');

// https://github.com/ethul/purescript-webpack-example
var PurescriptWebpackPlugin = require('purescript-webpack-plugin');

var purescriptWebpackPlugin = new PurescriptWebpackPlugin({
  pscArgs: {
    sourceMaps: true
  }
});

module.exports = {
  entry: "./app.js",
  output: {
    filename: "bundle.js"
  },
  module: {
    // see https://github.com/pixijs/pixi.js/issues/1854
    noParse: [ /.*(pixi\.js).*/ ],
    loaders: [
      {
        test: /\.js$/,
        exclude: /node_modules|bower_components/,
        loader: 'babel-loader',
        query: {
          cacheDirectory: true, 
          presets: ['es2015'] 
        }
      },
      {
        test: /\.purs$/,
        loader: 'purs-loader'
      }
    ]
  },
  // http://stackoverflow.com/questions/30870830/how-do-i-generate-sourcemaps-when-using-babel-and-webpack
  devtool: 'source-map',
  resolve: {
    modulesDirectories: ['node_modules', 'bower_components'],
    extensions: [ '', '.js', '.purs']
  },
  plugins: [ purescriptWebpackPlugin ]
};
