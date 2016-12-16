# elm-reflection
> find all elm files in a directory (recursively) and get infos about them.
It will tell you the module name and if it contains elm-css or elm-tests/doc-tests.

## installation

```bash
$ npm i elm-reflection -g
```

## Usage

```bash
$ elm-reflection > files.json
$ elm-reflection Css > css-files.json
$ elm-reflection DocTest ExposesTests > tests.json
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
