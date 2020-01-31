const path = require('path');
const webpack = require('webpack');
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
    entry: path.resolve('./index.js'),

    output: {
        path: path.resolve('./build'),
        filename: '[name]-[hash].js',
        publicPath: '/'
    },

    resolve: {
        extensions: [ '.js' ]
    },

    module: {
        noParse: /\.elm$/,
        rules: [
            {
                test: /\.elm$/,
                exclude: [ /elm-stuff/, /node_modules/ ],
                use: [
                    {
                        loader: 'elm-hot-webpack-loader'
                    },
                    {
                        loader: 'elm-webpack-loader',
                        options: {
                            pathToElm: path.resolve('./node_modules/.bin/elm'),
                            debug: true
                        }
                    }
                ]
            }
        ]
    },

    devServer: {
        historyApiFallback: true
    },

    plugins: [
        new HtmlWebpackPlugin({
            template: path.resolve('./index.html'),
            inject: 'body'
        })
    ],

    devtool: 'eval-source-map',
    mode: 'development'
};
