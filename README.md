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
`docker build -t jzwood/scrabble-oracle-api:v0.0.0 .`

### Run
`docker run -p 5000:3000 -it <tag> /bin/bash`
`docker run -p 5000:3000 -e PORT=3000 -it <tag>`

you can now hit the api at `localhost:5000/board/<board>/rack/<rack>`


## Deploy to Heroku
either make a new app called "scrabble-oracle-api" in heroku web UI or via CLI: `$ heroku create -a scrabble-oracle-api`, then,
```
$ heroku container:login
$ heroku container:push web -a scrabble-oracle-api
$ heroku container:release web -a scrabble-oracle-api
```



## Publishing to docker hub (not required for deployment)
### update tag for remote repo
`docker tag scrabble-scotty-app:Dockerfile jzwood/scrabble-oracle-api:v0.0.0`

### publish
`docker push jzwood/scrabble-oracle-api:v0.0.0`
