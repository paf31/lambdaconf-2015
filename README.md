# lambdaconf-2015

Materials for my LambdaConf 2015 Workshop

## Instructions for Attendees

This repository contains all of the code required for the workshop, but you will need to install some tools and dependencies.

- Install the PureScript compiler for your operating system. You should install the [0.6.9.5 release](https://github.com/purescript/purescript/releases/tag/v0.6.9.5). If you prefer to build from source, be sure to check out the `v0.6.9.5` tag of the `purescript` compiler repository. The code will **not** build using `master`.
- Install Node and the NPM package manager
- Install the `pulp` command line tool (`npm install -g pulp`)
- Clone this repository

### Warmup Exercises

```bash
cd warmup
pulp dep update
pulp -w browserify --to js/Main.js
```

Open `index.html` to view the warmup project. The instructions will be displayed in the browser. When the source files under `src/` change, the project will be rebuilt automatically.

### Building the client

```bash
cd client
pulp dep update
pulp -w browserify --to js/Main.js
```

### Running the server

The server is written in PureScript and will listen on port 9000. In a separate terminal:

```bash
cd server
pulp dep update
pulp run
```

Open your browser to http://localhost:9000 to view the project.
