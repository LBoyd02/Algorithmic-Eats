# Algorithmic-Eats (R Shiny + Docker)

This repo contains an R Shiny app packaged as a Docker image and published to Docker Hub.

## Prereqs
- Docker Desktop
- A Google Maps API key (provided at runtime via environment variable)

## Run the app (Docker Hub)
Pull the image:

```bash
docker pull lboyd02/algorithmic-eats:latest
```
If running on Apple Silicon:
```bash
docker pull --platform linux/amd64 lboyd02/algorithmic-eats:latest
```
To run the image:
```bash
docker run --rm -p 3838:3838 -e MAP_API_KEY="(api key)" lboyd02/algorithmic-eats:latest
```
