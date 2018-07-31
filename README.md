<img align="right" src="https://user-images.githubusercontent.com/1218615/43453962-374b7d44-94c4-11e8-83c2-5d9c703fce36.png" width=300/>

# ironhide [![CircleCI](https://circleci.com/gh/HealthSamurai/matcho.svg?style=shield)](https://circleci.com/gh/HealthSamurai/matcho) [![Join gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/healthsamurai/matcho)

Ironhide, the data transformer.

[![Clojars Project](http://clojars.org/healthsamurai/matcho/latest-version.svg)](http://clojars.org/healthsamurai/matcho)

## Idea

Create a language agnostic bidirectional data-driven transformation [dsl](https://en.wikipedia.org/wiki/Domain-specific_language) for fun and profit.

### Problem

There are a lot of data, which has to be represented in different shapes. For
this reason created a lot of query/transformation languages such as
[XSLT](https://www.w3.org/standards/xml/transformation),
[AWK](https://en.wikipedia.org/wiki/AWK), etc, but most of them have a
significant disadvantage: they work only in one direction and you can't get
original data from the result of transformation.

It worth noting that there are other languages like
[boomerang](http://www.seas.upenn.edu/~harmony/), which doesn't have this
significant (in some cases) weakness, but have others : )

Simplified real life example of representation person name in different systems:

```json
"form": {
  "name": "Firstname Lastname"
}
```

```json
"fhir": {
  "name": {
    "given": [
      "Firstname"
    ],
    "family": "Lastname"
  }
}
```

By different reasons both respresentations should be availiable and moreover
syncronized. Syncronization can be done by implementing and applying when needed
two function *f* and *f <sup>-1</sup>* for each field or subset of fields, but
you already probably know how hard to maintain such code?)

It is hard to implement such functions for big nested tree data structures and
much harder to keep *f <sup>-1</sup>* in sync with *f*.

### Solution

[ironhide](https://github.com/abcdw/ironhide) is attempt to create a
bidirectional data transformation language described by a data structure stored
in [EDN](https://github.com/edn-format/edn) (you can think about edn like a
better [JSON](https://json.org/)). `ironhide` still in early stage of
development, but already covers some practical usecases. It's declarative,
bidirectional, data-driven and simple.

Following code in ironhide solves example above:

```clj
#:ih{:direction [:fhir :form]
     :rules     [{:form [:name :ihp/str<->vector [0]]
                  :fhir [:name [0] :given [0]]}

                 {:form [:name :ihp/str<->vector [1]]
                  :fhir [:name [0] :family]}]}
```

The direction of transformation controlled by `:ih/direction` key and this
simple snippet allows to transform data in both ways out of the box.

## Usage

Description of the program in ironhide

### transformer

### parsers

### paths

### micros

## Thanks

Special thanks to:

* [Nathan Marz](https://github.com/nathanmarz) for [specter](https://github.com/nathanmarz/specter)
* [Nikolai Ryzhikov](https://github.com/niquola/) for [matcho](https://github.com/healthsamurai/matcho)


## License

Copyright © 2018 HealthSamurai

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
