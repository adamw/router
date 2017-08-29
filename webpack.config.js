module.exports = {
  entry: "./app.js",
  output: {
    filename: "bundle.js"
  },
  module: {
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
          pscArgs: {
            sourceMaps: true,
            "censor-codes": "ImplicitImport,MissingTypeDeclaration"
          },
          bundle: false
        }
      }
    ]
  },
  // http://stackoverflow.com/questions/30870830/how-do-i-generate-sourcemaps-when-using-babel-and-webpack
  devtool: 'source-map',
  resolve: {
    modules: ['node_modules', 'bower_components']
  }
};
