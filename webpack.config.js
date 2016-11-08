module.exports = {
  entry: "./app.js",
  output: {
    filename: "bundle.js"
  },
  module: {
    // https://github.com/pixijs/pixi.js/issues/1854#issuecomment-156074530
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
        loader: 'purs-loader',
        query: {
          psc: 'psa',
          src: ['bower_components/purescript-*/src/**/*.purs', 'src/**/*.purs'],
          pscArgs: { sourceMaps: true },
          bundle: false
        }
      }
    ]
  },
  // http://stackoverflow.com/questions/30870830/how-do-i-generate-sourcemaps-when-using-babel-and-webpack
  devtool: 'source-map',
  devserver: { stats: 'errors-only' },
  resolve: {
    modulesDirectories: ['node_modules', 'bower_components'],
    extensions: [ '', '.js', '.purs']
  }
};
