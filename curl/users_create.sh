#!/bin/sh
curl '127.0.0.1:3000/users?user=1' -X POST -d '{"first_name": "Jack", "last_name": "Black", "avatar_id": 1}'
