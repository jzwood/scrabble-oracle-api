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

### Run
`docker run -it <tag> /bin/bash`
