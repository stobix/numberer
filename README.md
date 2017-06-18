# README

* [Summary](#markdown-header-summary)
* [Installation](#markdown-header-installation)
    * [Setup](#markdown-header-setup)
    * [Dependencies](#markdown-header-dependencies)
* [Usage](#markdown-header-usage)
    * [`numberer`](#markdown-header-numberer)
        * [Functions returning numbers](#markdown-header-functions-returning-numbers)
        * [Functions handling binds](#markdown-header-functions-handling-binds)
        * [Tests](#markdown-header-tests)
        * [Server related](#markdown-header-server-related)
    * [`pid_numberer`](#markdown-header-pid_numberer)
* [Examples](#markdown-header-examples)
* [TODO](#markdown-header-todo)
* [Meta info](#markdown-header-meta)

## Summary

This app keeps track of things that needs to be numbered sequentially, keeping a queue of occupied and freed numbers. It comes in two flavours: one general numbering server, [`numberer`](numberer#markdown-header-numberer),  and one specifically for creating unique names for, and for registering processes, [`pid_numberer`](numberer#markdown-header-pid_numberer). (These might be split up into two separate apps in a future update.)

## Installation

### Setup
1. Use [rebar][] to fetch all dependencies and compile the app.
1. Place it somewhere in your erlang runtime path.

### Dependencies

none

[rebar]: https://github.com/basho/rebar "An erlang repository/dependency handler"

## Usage

### `numberer`

__NOTE__ _This module was written when I still had little Erlang experiece, and probably needs to be updated before proffessional usage_.

Keeps track of different lists of numbers, returning a unique number on each call to `get/1`. Aside from returning numbers sequentially, an alternative numbering schedule can be used, wherein an identifier can be "bound" (or a number "named"), returning the same number for each call to `get/2` with the identifier in question, until `release/2` is called and the number is free to be rebound. Each identifier can have exactly one number, and each number can have exactly one identifier.

Contains the following functions:

#### functions returning numbers 

Name | Argument types | Description
-- | -- | --
`get/1`| |
`get/2`| |

#### functions handling binds

Name | Argument types | Description
-- | -- | --
`number/2`| |
`n2i/2`| |
`i2n/2`| |
`name/3`| |
`rename/3`| |
`release/2`| |


#### tests

Name | Argument types | Description
-- | -- | --
`is_named/2`| |
`is_numbered/2`| |
`is_used/2`| |

#### server related

Name | Argument types | Description
-- | -- | --
`initiate/1` | |
`initiate/2` | |
`count_occupied/1` | |
`reset/1`| |
`reset/2`| |
`list_bound/1`| |

### `pid_numberer`

An app for registering pids to unique names, keeping tracks of when pids are terminated and the names freed. (Basically, this enables one to have a nice tree of named processes in observer without being afraid of exhausting the atom definition stack.)


## Examples

## TODO

## Meta

Created by Joel Ericson <http://bitbucket.org/volatile> under the GPL license.