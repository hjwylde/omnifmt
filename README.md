# omnifmt

[![Project Status: Wip - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/1.0.0/wip.svg)](http://www.repostatus.org/#wip)
[![Build Status](https://travis-ci.org/hjwylde/omnifmt.svg?branch=master)](https://travis-ci.org/hjwylde/omnifmt)
[![Release](https://img.shields.io/github/release/hjwylde/omnifmt.svg)](https://github.com/hjwylde/omnifmt/releases/latest)

A pretty-printer wrapper to faciliate ease of formatting during development.
omnifmt automatically formats code via external pretty-printers.
The idea was taken from gofmt, just with a bit of expansion to more languages.

Formatted code is:

* Easier to write: never worry about minor formatting concerns while hacking away.
* Easier to read: when all code looks the same you need not mentally convert others' formatting
  style into something you can understand.
* Easier to maintain: mechanical changes to the source don't cause unrelated changes to the file's
  formatting; diffs show only the real changes.
* Uncontroversial: never have a debate about spacing or brace position ever again.

(Bullet points taken from [https://blog.golang.org/go-fmt-your-code](https://blog.golang.org/go-fmt-your-code).)

### Installing

Installing omnifmt is easiest done using either
    [stack](https://github.com/commercialhaskell/stack) (recommended) or
    [Cabal](https://github.com/haskell/cabal).

**Using stack:**

```bash
stack install omnifmt
export PATH=$PATH:~/.local/bin
```

**Using Cabal:**

```bash
cabal-install omnifmt
export PATH=$PATH:~/.cabal/bin
```

### Usage

The omnifmt binary provides an interface for selecting files and piping them through external
    pretty-printers.
It supports both prettifying the files immediately and performing dry-runs to see which files are
    ugly.

**The basics:**

By default omnifmt formats on all files found from the root directory;
    the root directory is the first parent directory with an '.omnifmt.yaml' config file.

Passing arguments to omnifmt will override this and only operate on the given files and directories.

**Modes:**

omnifmt can run in three different modes, *normal*, *dry-run* and *diff*.

Normal mode writes to (prettifies) all ugly files immediately and outputs the prettified file paths
    to *stdout*.

Dry-run mode outputs the ugly file paths to stdout.

Diff mode outputs a diff of all ugly files with their prettified version.

#### Configuration

Configuration is done via an '.omnifmt.yaml' file in the root directory.
The file contains a list of *programs* that link *extensions* to a prettifying *command*, e.g.,
```yaml
haskell:
    extensions: ["hs", "lhs"]
    command:    "stylish-haskell {{input}} > {{output}}"

javascript:
    extensions: ["js"]
    command:    "js-beautify -f {{input}}"

json:
    extensions: ["json"]
    command:    "json_pp"

ruby:
    extensions: ["rb"]
    command:    "ruby-beautify"
```

Each command declares how to read the *input file* and how to write to the *output file*.
If the input variable is omitted, the file contents are fed to the command through *stdin*.
Likewise if the output variable is omitted, the pretty contents are read from stdout.
The output file is used to compare whether the original was pretty or ugly before writing to it.

The extensions field is pretty self explanatory, but if you use the same extension more than once
    then precedence goes to the program defined first.

#### Examples

See the [docs/example-configs/](https://github.com/hjwylde/omnifmt/tree/master/docs/example-configs/)
    directory for some common pretty-printers and their corresponding omnifmt config (pull requests
    are welcome for adding more).
Just don't forget to actually call the config file '.omnifmt.yaml'!

**NB:** I haven't tested them fully, be careful in case one is buggy.

### Auto-completion

Add the following (depending on your shell) to include support for auto-completion.

**Bash:**

```bash
source <(omnifmt --bash-completion-script `which omnifmt`)
```

**zsh:**

```zsh
autoload -Uz bashcompinit && bashcompinit
source <(omnifmt --bash-completion-script `which omnifmt`)
```

