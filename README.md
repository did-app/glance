# Glance

Glance at a url for [plummail.co](https://plummail.co) message preview.

## Development

Run with docker.

```
docker-compose up
```

Visit http://localhost:9000/?https://bbc.co.uk

## Heroku

This app uses the container stack.

```
heroku stack:set -a did-glance container
```
