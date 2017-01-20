## Cafe duty

The build system generates docker images. See https://docs.haskellstack.org/en/stable/docker_integration/ to set up the environment.

- First of all: `stack docker pull` to pull the right image.

- build: `stack build`

- run: `./run.sh`

- ghci: `stack ghci`

- First time usage: `stack setup`


## Deploy
 - To create a docker image to deploy: `stack image container` this will create afcastano/hs-cafe-duty
 - to run the image: `docker run --rm -p 3000:3000 -v ${LOCAL_DB_PATH}:/db afcastano/hs-cafe-duty`
 - To push: `docker push afcastano/hs-cafe-duty`


# Useful information:
- install stack mac: `brew update` and then `brew install haskell-stack`
- add current user to docker group. Instructions: `sudo dseditgroup -o edit -a ${USER} -t user docker` 
- If docker group does not exist, go to users and groups and create it.

# Docker help
- Stop all containers `docker stop $(docker ps -a -q)`
- Remove all containers `docker rm $(docker ps -a -q)`
- Remove an image `docker rmi {imageid}

# references:
- [Docker Integration](https://docs.haskellstack.org/en/stable/docker_integration/) 
- [Image Configuration](https://docs.haskellstack.org/en/stable/yaml_configuration/#image)
- [Image config example](http://mdjnewman.me/2016/01/haskell-stack-yesod-docker/)


### Wanted refactors

- Make code less imperative
- Review architecture
- Create a DBResult monad instead of IO Maybe
- Handle errors
- Type alias to variables for better readibility
- WRITE TESTS!!!
