# Tablecloth-rescript

[![CircleCI](https://circleci.com/gh/darklang/tablecloth.svg?style=shield)](https://circleci.com/gh/darklang/tablecloth)
[![Npm](https://badge.fury.io/js/tablecloth-rescript.svg)](https://www.npmjs.com/package/tablecloth-rescript)

Tablecloth is a library that shims over various standard libraries so they have the same function and module names, which using idiomatic types and patterns in each language.

This is the Rescript implementation, which uses Belt, pipe-first, and camelCase.

**Tablecloth is alpha-quality software, and is pre-1.0. It is currently undergoing
some significant shifts and some libraries listed below are not available yet.
Caveat emptor.**

Check out the [website](https://www.tablecloth.dev) for our interactive API
documentation, or join the community in the [Tablecloth
Discord](https://www.tablecloth.dev/discord-invite).

## Installation

**Note: these instructions are for the upcoming new version of tablecloth**

Install via npm by:

`npm install tablecloth-rescript`

Then add to your `bsconfig.json` file:

`"bs-dependencies" : ["tablecloth-rescript"]`

## Usage

The recommended way to use Tablecloth is with a top-level open at the beginning of a file.

This will ensure that all the built-in modules are replaced.

```
open Tablecloth

let () =
  String.toList("somestring")
  ->List.map(Char.toCode)
  ->List.map((x) => x+1))
  ->List.filterMap(Char.fromCode)
  ->String.fromList
```

## Supported versions

Tablecloth supports Rescript 9 and 10. [Older versions of Tablecloth](https://www.npmjs.com/package/tablecloth-bucklescript) supported older versions of bs-platform.

### Development

When developing Tablecloth, you can test it against different versions of
rescript, using the following commands:

- `TC_RESCRIPT_VERSION=10.0.0 make deps`

## Contributions

The maintainers are warm and friendly, and the project abides by a [Code of Conduct](./CODE_OF_CONDUCT.md).

There are many small tasks to be done - a small change to a single function can be extremely
helpful. We also welcome new versions of tablecloth for other languages, or even for the same
language but based on other libraries.

Check out the [dedicated guide](./documentation/contributing.md) on contributing for more.

## Developing

Please refer to the `package.json` for a complete list of supported actions. Here is
a handful of useful, supported commands:

```
npm install
npm run build
npm run test
npm run format
```

## License

Tablecloth-rescript uses the [MIT](./LICENSE) license.

## Authors

Initially written by [Darklang](https://darklang.com).
