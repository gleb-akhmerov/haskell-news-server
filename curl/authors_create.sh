#!/bin/sh
curl '127.0.0.1:3000/authors?user=1' -X POST -d '{"user_id": 2, "short_description": ""}'
