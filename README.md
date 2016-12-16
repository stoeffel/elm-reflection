# elm-reflection

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
        "types": [Test, ExposesTests],
        "name": "FooSpec"
    }
]
```
