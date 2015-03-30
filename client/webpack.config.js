var HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
    context: __dirname + "/src",
    entry: './index.js',
    output: {
        path: __dirname + "/build",
        filename: "bundle.js"
    },
    module: {
        loaders: [
            { test: /\.js[x]?$/, loader: 'jsx-loader' },
            { test: /\.css$/, loader: 'style!css' },
            { test: /\.scss$/, loader: 'style!css!sass' },
            { test: /\.(jpe?g|gif|png|ttf|svg|woff2?|eot)$/, loader: 'file-loader' }
        ]
    },
    plugins: [new HtmlWebpackPlugin({title: 'HOJ'})]
};
