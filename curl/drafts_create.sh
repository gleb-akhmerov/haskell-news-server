#!/bin/sh
curl '127.0.0.1:3000/drafts?user=1' -X POST -d '{"short_name": "A post!", "category_id": 1, "text_content": "Breaking news!", "main_photo_id": 1, "additional_photo_ids": [], "tag_ids": []}'
