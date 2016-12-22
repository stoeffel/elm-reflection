# elm-reflection
> find all elm files in a directory (recursively) and get infos about them.
It will tell you the module name and if it contains elm-css or elm-tests/doc-tests.

## installation

```bash
$ npm i elm-reflection -g
```

## Usage

```
Usage: elm-reflection [--path ARG] [--filter ARG]

Available options:
  -h,--help                Show this help text
  --path ARG               Default is whatever is in your "source-directories"
  --filter ARG             Possible values: [Test,ExposesTests,DocTest,Css]
```

```bash
$ elm-reflection > files.json
$ elm-reflection -- filter Css > css-files.json
$ elm-reflection --filter DocTest,ExposesTests > tests.json
```

## The Json

```json
[
    {
        "path": "/Users/stoeffel/src/Foo/Bar.elm",
        "types": [],
        "name": "Foo.Bar"
    },
    {
        "path": "/Users/stoeffel/src/tests/FooSpec.elm",
        "types": ["Test", "ExposesTests"],
        "name": "FooSpec"
    }
]
```
