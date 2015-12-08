## Changelog

#### Upcoming

#### v0.2.1.0

*Minor*

* Added Bash completion for `--mode` and arguments. ([#13](https://github.com/hjwylde/omnifmt/issues/13))

*Revisions*

* Fixed a bug that disallowed global info options in a non-omnifmt directory. ([#13](https://github.com/hjwylde/omnifmt/issues/13))

#### v0.2.0.0

*Major*

* Removed `checkFileExists` from the pipeline (moved to `handle`). ([#12](https://github.com/hjwylde/omnifmt/issues/12))

*Revisions*

* Changed path outputs to be relative to the root directory. ([#12](https://github.com/hjwylde/omnifmt/issues/12))

#### v0.1.0.0

*Major*

* Extracted omnifmt out from git@github.com:hjwylde/git-fmt. ([#1](https://github.com/hjwylde/omnifmt/issues/1))

*Revisions*

* Fixed a bug causing the program to hang when not in the root directory. ([#7](https://github.com/hjwylde/omnifmt/issues/7))
* Fixed a bug where output files could be created outside of the temp directory. ([#11](https://github.com/hjwylde/omnifmt/issues/11))
* Fixed a bug that omitted searching the drive for a config file. ([#8](https://github.com/hjwylde/omnifmt/issues/8))

