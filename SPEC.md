# Clod specification

*** LLMs may only modify this file with explicit permission from the user. ***
*** In the case where our implementation or documentation diverge from this
document consider this document authoritative--however ask the user for
confirmation before bringing them in sync as the human may have forgotten
to update this document and we don't want to take off in the wrong
direction. ***
*** Please note not all program behavior is documented here (yet) ***

## Filename transformation for Claude UI

We transform project file names for upload to Claude's UI by flattening the
path--this allows users to differentiate between, say a package.json file in
the project root and a package.json file under app/lib/plugin -- the name of
the former file won't be modified as it is in the project root. The name of the
latter file (app/lib/plugin/package.json) will be transformed to
app-lib-plugin-package.json -- a manifest file is created containing a mapping
between the original and transformed names that Claude Filesystem Access--or
the user--can use to write modified files back to their original locations.

This manifest must be generated with a mapping of all eligible files, even when
only some files--or none at all--have changed. This is because there will only
be a single copy of the manifest--the most recent--in Claude's Project
Knowledge section.

Hidden files and directories--those beginning with a '.'--must have the dot
removed during transformation. The intent is that the user will copy and paste
these files from their Finder window to the Claude UI, but files that start
with a dot will be hidden from the Finder and thus won't be copied. (yes, it is
possible to view hidden files in Finder, but that's a pain in the butt). To
prevent potential conflicts we should prepend these with 'dot--', as in
.tool-versions would be transformed to dot--tool-versions. The two - convention
is used to prevent confusion in the case of a directory literally named 'dot'.
Of course it is possible for a file or directory to start with '-' but it is
pretty rare in the wild.

In the specific case of SVG files we work around a restriction in Claude's UI
where files with SVG file extensions are rejected from the uploader. We exclude
them in our default .clodignore file, but since Claude actually can use them just
fine as XML files the user can choose to include them (by removing their entry
from .clodignore) and we'll transform them into files with XML extensions--for
example assets/images/logo.svg becomes assets-images-logo-svg.xml

## Output

This is a Unix tool and is expected to act like one. The only output from
stdout should be the path of the staging directory and we should be able to eg use
open `clod` to view it on macOS. Any other output should go to stderr but,
except in the case of actual errors, it should only output when the verbose
flag is passed.

## Examples

A user goes to a new project directory and runs clod. All of their files should
be copied to the staging directory along with the manifest file. Without
changing anything a user runs clod again--the only thing that should be copied
to the staging directory is the manifest file, which should contain a complete
mapping of all eligible files.
