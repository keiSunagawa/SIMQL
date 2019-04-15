const path = require('path');
const webpack = require('webpack');

module.exports = {
  mode: 'development', // develop mode fast hot-reload iteration
//  target:"electron-renderer",
  devtool: 'source-map',

  entry: {
    app: './src',
  },

  // Configuration for dev server
  devServer: {
    contentBase: 'public',
    port: 3000
  },

  // バンドルファイルの出力場所
  output: {
    filename: "[name].js",
    path: path.resolve(__dirname, 'public/build'),
    publicPath: '/build/'
  },
  optimization: {
    splitChunks: {
      name: 'vendor',
      chunks: 'initial',
    }
  },
  resolve: {
    modules: [
      // srcディレクトリをimport解決のrootに設定
      path.resolve(__dirname, "src"),
      "node_modules",
    ],
    extensions: [".ts", ".tsx", ".js", ".jsx", ".css"]
  },

  module: {
    rules: [
      {
        // TypeScript のトランスパイル設定
        test: /\.tsx?$/,
        loader: "awesome-typescript-loader"
      },
      {
        test: /\.css/,
        use: [
          'style-loader',
          {loader: 'css-loader', options: {url: false}},
        ],
      },
    ]
  },
  plugins: []
};
