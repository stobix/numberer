# README

* [Summary](numberer#markdown-header-summary)
* [Installation](numberer#markdown-header-installation)
    * [Setup](numberer#markdown-header-setup)
    * [Dependencies](numberer#markdown-header-dependencies)
* [Usage](numberer#markdown-header-usage)
    * [`numberer`](numberer#markdown-header-numberer)
    * [`pid_numberer`](numberer#markdown-header-pid_numberer)
* [Examples](numberer#markdown-header-examples)
* [TODO](numberer#markdown-header-todo)
* [Meta info](numberer#markdown-header-meta)

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

### `pid_numberer`

## Examples

## TODO

## Meta

Created by Joel Ericson <http://bitbucket.org/volatile> under the GPL license.