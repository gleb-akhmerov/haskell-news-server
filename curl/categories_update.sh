#!/bin/sh
curl '127.0.0.1:3000/categories/1?user=1' -X PUT -d '{"new_name": "A category!"}'
