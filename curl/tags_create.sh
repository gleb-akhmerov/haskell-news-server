#!/bin/sh
curl '127.0.0.1:3000/tags?user=1' -X POST -d '{"name": "Python"}'
