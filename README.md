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
development, but already covers some practical usecases. It's also declarative,
bidirectional, data-driven and simple.

Following code in ironhide solves example above:

```clj
#:ih{:direction [:fhir :form]
     :rules     [{:form [:name :ihs/str<->vector [0]]
                  :fhir [:name [0] :given [0]]}

                 {:form [:name :ihs/str<->vector [1]]
                  :fhir [:name [0] :family]}]}
```

The direction of transformation controlled by `:ih/direction` key and this
simple snippet allows to transform data in both ways out of the box.

## Usage

### deps and require

`deps.edn`:

```clj
{healthsamurai/matcho {:mvn/version "RELEASE"}}
```

`hello_world.clj`:

```clj
(ns hello-world.core
  (:require [ironhide.core :as ih]))
  
;; (ih/execute shell)
;; or
;; (ih/get-data shell)
;; (ih/get-data shell :charge1)
```

## Description 

Main object for `ironhide` is a `shell`. `ironhide` can execute `shell`'s.
`shell` is a tree datastructure, which contains declaration of transformation
rules + data itself.

It consists of few main parts:

* `:ih/data` a data for transformation
* `:ih/values` similar to previous one, but used mostly for default values
* `:ih/micros` shortcuts for long repetitive pathes in rules
* `:ih/direction` default transformation direction (rule can define its own)
* `:ih/rules` vector of transformation rules

Simple `shell` executed with `get-data`:

```clj
(get-data
 #:ih{:direction [:form :fhir]
      :data      {:form {:first-name "Firstname"}
                  :fhir {}}
      :rules     [{:form [:first-name]
                   :fhir [:name [0] :given [0]]}]})
;; => {:form {:first-name "Firstname"}, :fhir {:name [{:given ["Firstname"]}]}}
```

### Bullet

A `bullet` is any leaf value (subtree is also a leaf value).

### Charge

A `charge` is name of the part of subtree inside the `shell`, most often placed
under the `:ih/data` key.

Source `charge` is used for getting `bullet`s and sink `charge` for updating.

### Path and pelem

`Path` is a vector consist of `pelem`s, which describes how to get to
`bullet`s. Something similar to [XPath](https://en.wikipedia.org/wiki/XPath),
[JsonPath](http://goessner.net/articles/JsonPath/), but not exactly.

There are few types of `pelem`s:

* `mkey`
* `vnav`
* `sight`
* `micro`

| term      | definition                                                                                    |
|-----------|-----------------------------------------------------------------------------------------------|
| `mkey`    | a simple edn `:keyword`, which tells `shell` executor to navigate to specific key in the map. |
| `vnav`    | a vector, which consists of `vkey` and optional `vfilter`.                                    |
| `vkey`    | an `index` (some non-negative integer) or wildcard `:*` (keyword).                            |
| `vfilter` | a map used for pattern matching and templating                                                |
| `sight`   | `:ihs/` namespaced keyword or `{:ih/sight :ihs/sight-name :arg1 :value1}`         |
| `micro`   | `:ihm/` namespaced keyword or `{:ih/micro :ihm/micro-name :arg1 :value1}`                     |

Example of paths and `get-value` results: 

```clj

;; {:k1 {:k2 :v3}}
[:k1] ;; => [[{:k2 :v3}]]
[:k1 :k2] ;; => [[:v3]]

;; [{:a :b} {:k :v :k1 :v2}]
[[0]] ;; => [[{:a :b}]]
[[:*]] ;; => [[0 {:a :b}] [1 {:k :v :k1 :v1}]]
[[1 {:a :b}]] ;; => [[nil]]
[[:* {:k :v}]] ;; => [[0 {:k :v :k1 :v1}]]

;; {:name "Firstname, Secondname"}
[:name :ihs/str<->vector [0]] ;; => [["Firstname,"]]
;; [:name {:ih/sight :ihs/str<->vector :separator ", "} [0]]
[:ihm/first-name] ;; => [["Firstname"]]
```

### Sight

`sight` is a special `pelem`, which allows to percieve `bullet` differently.
It's useful when you want to treat a string as a vector of words for example:

```clj
;; {:name "Firstname Secondname"}
[:name :ihs/str<->vector [0]] ;; => [["Firstname"]]
```

It allows to navigate inside `bullet` differently and more preciesly, but don't
change original structure of it.


### Micro

`micro` is a parametrized shortcut for part of the path. 

```clj
(microexpand-path
 #:ih{:micros #:ihm {:name [:name [:index] :given [0]]}}
 [:ihm/name])
;; => [:name [:index] :given [0]]

(microexpand-path
 #:ih{:micros #:ihm {:name [:name [:index] :given [0]]}}
 [{:ih/micro :ihm/name :index 10}])
;; => [:name [10] :given [0]]
```

Default values for micros not supported yet.


### rule

Rule is a hashmap, which can contain few different things:

* `name` for data source and `path` to node in it
* `:ih/direction` specific direction for current rule by datasource names
* `:ih/defaults` 


## Usage


## Thanks

Special thanks to:

* [Nathan Marz](https://github.com/nathanmarz) for [specter](https://github.com/nathanmarz/specter)
* [Nikolai Ryzhikov](https://github.com/niquola/) for [matcho](https://github.com/healthsamurai/matcho) and [2way](https://github.com/niquola/2way)


## License

Copyright © 2018 HealthSamurai

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
