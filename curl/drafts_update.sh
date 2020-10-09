#!/bin/sh
curl '127.0.0.1:3000/drafts/1?user=1' -X PUT -d '{"new_short_name": "New draft name!", "new_text_content": "Text content."}'
