nix-build docker.nix -o image
TAG=`docker load < image | awk '{print $3}'`
LATEST='jeovazero/pepe-haskeller:latest'
docker tag $TAG $LATEST
docker push $TAG
docker push $LATEST
# setting an environment variable in the Github Workflow
# https://docs.github.com/en/free-pro-team@latest/actions/reference/workflow-commands-for-github-actions#setting-an-environment-variable
echo "TAG=$TAG" >> $GITHUB_ENV