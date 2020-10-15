# Scrabble-Scotty Web App

API wrapper for [Scrabble AI](https://github.com/jzwood/ScrabbleOracle).


# local

```haskell
stack install
scrabble-scotty-app-exe
```

hit api via CURL or Postman:
```haskell
localhost:3000/board/<board>/rack/<rack>
params:
  - board:
    * 225 characters of scrabble board (left to right and top to bottom)
    * uppercase
    * empty space = '_'
  - rack
    * 7 characters of rack (5-7 allowed)
    * uppercase
```

# deploy

## Docker
### Build
`docker build -t "scrabble-scotty-app:Dockerfile" .`
or better
`docker build -t "jzwood/scrabble-oracle-api:v0.0.0"`

### Run
`docker run -p 5000:3000 -it <tag> /bin/bash`

you can now hit the api at `localhost:5000/board/<board>/rack/<rack>`

## Deploying
### update tag for remote repo
`docker tag scrabble-scotty-app:Dockerfile jzwood/scrabble-oracle-api:v0.0.0`

### publish
`docker push jzwood/scrabble-oracle-api:v0.0.0`
