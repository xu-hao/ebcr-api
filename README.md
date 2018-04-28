# ebcr-api

## build docker image
```
stack docker pull
```

```
stack build
```

```
stack image container
```

## run docker image
```
docker run --rm -d -p 8080:8080 ebcr-api-ebcr-api-exe
```

## query api
```
curl -XGET "localhost:8080/get_ids_by_feature" -H "Content-Type: application/json" -d "{}" -v
```