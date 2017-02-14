# purescript-drumloops

Experimenting with purescript-howler

## Getting Started

```
npm install -g bower pulp
bower update
pulp -w build -O --to html/index.js
  or alternatively: pulp -w browserify --to html/index.js
```

You may need to run a local HTTP server to get around security restrictions. I use Python's SimpleHTTPServer:

```
python -m SimpleHTTPServer
```

and then open http://localhost:8000/html.
