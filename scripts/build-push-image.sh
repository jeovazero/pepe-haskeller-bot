nix-build docker.nix -o image
TAG=`docker load < image | awk '{print $3}'`
docker push $TAG
# setting an environment variable in the Github Workflow
# https://docs.github.com/en/actions/reference/workflow-commands-for-github-actions#setting-an-environment-variable
echo "::set-env name=TAG::$TAG"
