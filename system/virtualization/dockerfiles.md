# Dockerfiles

Often times it's best to start from a known (supported) image. 
A lot to learn from how those are configured. 

If you need a custom one, it's fine to track settings in your own Dockerfile. Here are some examples. 

### Python

https://hub.docker.com/_/python

```
FROM python:3

WORKDIR /usr/src/app

COPY requirements.txt ./
RUN pip install --no-cache-dir -r requirements.txt

COPY . .

CMD [ "python", "./your-daemon-or-script.py" ]
```

$ docker build -t my-python-app .
$ docker run -it --rm --name my-running-app my-python-app

### Node

https://hub.docker.com/_/node/


```
FROM node:lts

# Use the official image as a parent image.
FROM node:current-slim

# Set the working directory.
WORKDIR /usr/src/app

# Copy the file from your host to your current location.
COPY package.json .

# Run the command inside your image filesystem.
RUN npm install

# Inform Docker that the container is listening on the specified port at runtime.
EXPOSE 8080

# Run the specified command within the container.
CMD [ "npm", "start" ]

# Copy the rest of your app's source code from your host to your image filesystem.
COPY . .
```

https://docs.docker.com/develop/develop-images/dockerfile_best-practices/

