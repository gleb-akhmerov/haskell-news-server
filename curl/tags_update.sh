#!/bin/sh
curl '127.0.0.1:3000/tags/1?user=1' -X PUT -d '{"new_name": "A tag!"}'
