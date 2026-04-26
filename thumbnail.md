# Deck Thumbnail Upload Guide

This guide shows how to authenticate, find a deck, and upload a thumbnail image.

## 1) Login and get a JWT

Use the login endpoint and copy the returned `token`.

```bash
curl -sS -X POST "https://nimzochess.com/api/auth/login" \
  -H "Content-Type: application/json" \
  -d '{
    "username": "YOUR_USERNAME",
    "password": "YOUR_PASSWORD"
  }'
```

Store the token:

```bash
TOKEN="<paste token here>"
```

## 2) Search decks by name

Use instant search while typing:

```bash
curl -sS "https://nimzochess.com/api/deck/search/instant?query=Sicilian"
```

Or full search:

```bash
curl -sS "https://nimzochess.com/api/deck/search/full?query=Sicilian"
```

From the response, note the deck `deckId` you want to update.

## 3) Upload a thumbnail for a deck

Only the deck author can upload the image.

Prepare base64 data (no line wraps):

```bash
BASE64_IMAGE=$(base64 -w 0 ./thumbnail.png)
```

Upload:

```bash
DECK_ID=123

curl -sS -X POST "https://nimzochess.com/api/deck/${DECK_ID}/image" \
  -H "Authorization: Bearer ${TOKEN}" \
  -H "Content-Type: application/json" \
  -d "{\"mimeType\":\"image/png\",\"base64Data\":\"${BASE64_IMAGE}\"}"
```

Expected response:

```json
{
  "imageUrl": "/deck-images/deck-123.png?v=1712345678"
}
```

## Notes

- Supported image types: `image/jpeg`, `image/png`, `image/webp`
- Max image size: 5 MB
- For public display, ensure Nginx serves `/deck-images/` and the backend has `DECK_IMAGE_PUBLIC_BASE=/deck-images`
