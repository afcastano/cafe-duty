## Cafe duty

Generates a roster of pairs given a team of people.


## Development
- First time usage: `stack setup`

- build: `stack build`

- run: `./run.sh`

- ghci: `stack ghci`


## Deploy
 - To create a docker image to deploy: `docker build -t afcastano/hs-cafe-duty:latest .` this will create the image afcastano/hs-cafe-duty
 - to run the image: `docker run --rm -p 3000:3000 -v ${LOCAL_DB_PATH}:/db afcastano/hs-cafe-duty`
 
# Useful information:
- install stack mac: `brew update` and then `brew install haskell-stack`

# Docker help
- Stop all containers `docker stop $(docker ps -a -q)`
- Remove all containers `docker rm $(docker ps -a -q)`
- Remove an image `docker rmi {imageid}


### Wanted refactors

- Make code less imperative
- Review architecture
- Create a DBResult monad instead of IO Maybe
- Handle errors
- Type alias to variables for better readibility
- WRITE TESTS!!!
