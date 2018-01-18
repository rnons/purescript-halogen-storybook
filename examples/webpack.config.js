const HtmlWebpackPlugin = require("html-webpack-plugin");

module.exports = {
  context: __dirname,
  entry: "./src/main.js",
  output: {
    path: __dirname + "/../docs",
    filename: "main.js"
  },
  resolve: {
    modules: ["node_modules", "bower_components"],
    extensions: [".purs", ".js"]
  },
  module: {
    rules: [
      {
        test: /\.purs$/,
        use: [
          {
            loader: "purs-loader",
            options: {
              src: [
                "bower_components/purescript-*/src/**/*.purs",
                "src/**/*.purs"
              ],
              psc: "psa"
            }
          }
        ]
      },
      {
        test: /\.css$/,
        use: ["style-loader", "css-loader"]
      }
    ]
  },

  plugins: [
    new HtmlWebpackPlugin({
      template: "src/index.html",
      filename: "index.html",
      minify: {
        collapseWhitespace: true
      }
    })
  ]
};
