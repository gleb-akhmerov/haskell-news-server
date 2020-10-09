#!/bin/sh
curl '127.0.0.1:3000/posts/1/comments?user=1' -X POST -d '{"content": "A commentary!"}'
