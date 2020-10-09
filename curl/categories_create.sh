#!/bin/sh
curl '127.0.0.1:3000/categories?user=1' -X POST -d '{"parent_id": null, "name": "Programming Languages"}'
