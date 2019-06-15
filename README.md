# Overview

This repository holds a first shot of a documentation infrastructure for various Scheme dialects.

The complete functionality is split into various layers:

1. An API server, which provides an HTTP API for various ways to request documentation relevant information

2. An API middleware, which can optionally be used on the developers systems to access the API server. It defines an API
   client, which performs the communication with the API server, but it can also define additional features such as
   caching API responses or access to documentation-related features of a given Scheme dialect such as the
   `describe {identifier}` command of various Scheme REPLs.

3. An editor client, that can either access the API server, or - if installed - the API middleware.

## History

It all started with various discussions in the [srfi-discuss](https://srfi-email.schemers.org/srfi-discuss/) mailing
list, which revolved around the topic as how to set-up an infrastructure allowing access to the documentation of Scheme
procedures and other symbols defined by [Scheme SRFIs](https://srfi.schemers.org/). The initial idea in this discussion
was to add structural information to existing and new Scheme SRFIs in order to potentially support various mostly
documentation-related tools. A summary of these discussions can be
found [here](https://github.com/schemedoc/schemedoc.el/blob/master/Initial_Discussion_Summary.md).

Some concretization with focus on some simple Emacs helpers incl. some links on exisiting Emacs infrastucture has been
noted [here](https://github.com/schemedoc/schemedoc.el/blob/master/how-to-access-scheme-api.org).

And finally a first shot on collecting some relevant meta data to drive the Scheme documentation infrastructure can be
found [here](https://github.com/schemedoc/implementation-metadata).

## Prior art:

* Superior Lisp Interaction Mode for Emacs (*SLIME*): https://github.com/slime/slime

  The Emacs SLIME integration provides a very deep integration from Emacs document editing into a running Common Lisp
  image, and this integration heavily extend the scope and the complexity of what we discuss here. So we will limit the
  scope compared to what is available with Emacs and SLIME. And yes, we know that there is SLIME support at least for
  Kawa Scheme and MIT Scheme - but that simply doesn't mean that all SLIME features are magically available for many
  more Scheme implementations and for all tool deployment options we're discussing here.

  Also note that there already exists the [Geiser](https://www.nongnu.org/geiser/) package for Emacs, which provides some
  SLIME-like functionality and that some Scheme implementations provide their own Emacs modes with deep integration, e.g.
  Bigloo's [Bee mode](https://www-sop.inria.fr/mimosa/fp/Bigloo/doc/bigloo-36.html).

  Here we explicitly focus on documentation access support, meaning that we consider the following high-level SLIME
  features as being out-of-scope: Evaluation, Compilation, Macro-expansion, Disassembly, Profiling or semantic indentation.

  In addition to the well-established functionality provided by Emacs and SLIME there are also the extented features
  provided by the REPLs of some Scheme implementations, which are also very well of interest for what we're discussing
  here. Making the functionality of these REPLS available in the context of their respective Schemes can be made possible
  by considering additional deployment options for the documentation tools discussed here.

* Microsoft Language Server Protocol (*LSP*): https://microsoft.github.io/language-server-protocol/specification;
  https://langserver.org/; https://github.com/Microsoft/language-server-protocol

  The LSP includes a large set of features, from which only very few are considered to be in scope of what we're
  discussing here. LSP features are excluded here either because they do not match the purpose of the tool we're
  discussing here and/or because they would significantly increase complexity.

  E.g. many LSP features are based on the fact that both the server and the client have a shared knowledge of the current
  document or even of the whole current workspace (meaning the complete set of currently "relevant" files). This shared
  knowledge allows the communication based on text positions and text ranges and it allows document-wide actions initiated
  by the server side.

  For now we want to concentrate on the subset of features where the client only sents a symbol to the server and where
  the server does only have access to internet-wide visible documents, but not to documents stored on the client.

  This means that we consider the following high-level features being out-of-scope: code refactoring, code error detection
  and auto-correction, symbol renaming, symbol highlightning and syntactic coloring, code formatting, folding or
  indentation, collection of document-wide categories of text ranges such as syntactic identifiers URLs or color codes.

## Status

- This is currently all work in progress and of no immediate use.

- The implementation currently implements the API server in Guile Scheme; the server-side code is highly dependendent
  from Guile Scheme.

- The implementation currently implements limited features for an API middleware in Guile Scheme and in Gauche Scheme.
  The invocation of an API middleware instance is currently just a stub for Guile Scheme and is not yet implemented for
  Gauche Scheme.

# Implementation

## Sharing common code

Folder `~/src/common` defines some common code shared by all implementations. Since I cannot expect to find a
sufficiently common module feature set over all implementations, I do not even try to use modules for this task, instead
I'm using more low-level file inclusion. This section holds some notes on what I found out while trying to combine the
module mechanism specific for a given Scheme dialect and the low-level file inclusion.

Note that it's necessary that the calling module already imports all required dependencies *before* including a common
Scheme file, so that these files can be written without having to require their dependencies - as the loading of these
dependencies might again force implementation-specific code to be used.

The following procedure naming convention has been chosen for the common include files:

- `%%procedure` denotes as procedure that is only supposed to be used with the current include file.

- `%procedure` denotes as procedure that is supposed to be wrapped by an implementation specific `procedure`.

- `procedure` denotes as procedure for which no restrictions are assumed; that procedure might also be exported by the
  including module.

### Gauche

Both `include` and `load` do work, assuming that the folder for the common sources (`~/src/common/sdp/common`)
has been added to the load path; see `Makefile`. Still when using `load`, Gauche requires to pass the environment of
the current module as shown in the source block below:

```scheme
(load "metadata" :environment (find-module 'sdp.common.metadata-gauche))
```

Note that when including a file into a Gauche modul, it seems to be necessary to first explicitly select the current
module using something like `(select-module sdp.common.model-gauche)` to avoid errors such as "Attempted to create a
binding (...) into gauche....".

### Guile

When using `load`, Guile requires `eval-when`, see the example source block below (and
the [documentation](https://www.gnu.org/software/guile/manual/html_node/Loading.html)). Guile also requires to add the
root folder for the common sources (`~/src/common`) to be added to
the [load path](https://www.gnu.org/software/guile/manual/html_node/Load-Paths.html). Alternatively
`primitive-load-path` can be used within `eval-when` and with using a relative path.

As it is [supposed to do](https://www.gnu.org/software/guile/manual/html_node/Local-Inclusion.html), `include` does not
require `eval-when`, but it also did not resolve paths as expected. So instead for Guile I'm using `include-from-path`,
which again requires a relative path and assumes that the root folder for the common sources (`~/src/common`) has been
added to the load path; see `Makefile`.

```scheme
(eval-when (expand load eval)
;; requires -L "$(CURDIR)/common": this will call (primitive-load-path "sdp/common/metadata.scm")
(load "metadata.scm")) ; or: (primitive-load-path "sdp/common/metadata.scm")
```

*Note:* Guile has a non-standard extension which allows to customize the default printing behavior of records,
see [here](https://www.gnu.org/software/guile/manual/html_node/SRFI_002d9-Records.html), chapter "Custom Printers". This
is currently not used.

## Testing included and imported bindings

### Gauche

- To just test some bindings imported and exported from the module below, run the following:

```shell
gosh -A"./gauche" -A"./common/sdp/common" -e"(use sdp.common.model-gauche)" -e"(display (list make-client-info-gauche make-request))" -e"(exit)"
```

- Allow to read S-expression content from file `{implementation}.scm` in `get-metadata-file`:
  `MD_PATH=$(MD_PATH)`

- Add file `metadata.scm` with common API procedures to load path:
  `-A"$(CURDIR)/common/sdp/common"`;

  *Note:* To get a stacktrace in Gauche, run the tests without the trailing `-e"(test)"`; this will only load the
  files to test, then execute `(test)` in the REPL.

### Guile

- To just test some bindings imported and exported from the module below, run the following:

```shell
guile  -L "./guile" -L "./common" -c "(use-modules (sdp common model-guile)) (display (list make-client-info-guile make-request))"
```

- Allow to read S-expression content from file `{implementation}.scm` in `get-metadata-file`:
  `MD_PATH=$(MD_PATH)`

- Add file `metadata.scm` with common API procedures to load path:
  `-L "$(CURDIR)/common"`

## Reading metadata

Some of the features provided by this application are implemented by accessing the schemedoc metadata defined in this
repository: https://github.com/schemedoc/implementation-metadata. The files from this repository are expected locally in
a filesystem folder, which needs to be passed to the helper procedures defined below; see `Makefile` variable `MD_PATH`.

The helper procedures wrap the access to the metadata, as far as the content is related to the Scheme documentation.

## API server, implemented in Guile Scheme

The API server is implemented as an HTTP server, so it will run as a central instance and hence there is no (urgent)
need to implement it in a portable way. The current implementation is using Guile Scheme.

The HTTP API server obviously exposes its features as HTTP requests/responses, where each supported URL exposes one
specific documentation feature and where each such feature is provided by one of the lower-level modules, e.g. the
modules wrapping access to the metadata or the REPL-specific documentation helpers.

Note that since the server is implemented dependent from a specific Scheme dialect, we can only support the
implementation of that specific Scheme for those features, that forward documentation-search to Scheme-specific code.
Concretely we can e.g. only call the Guile-specific REPL documentation helpers from Guile in the API server.

### Guile server runner

The code in module `(sdp server http)` is mostly a hack/stub, which currently just tests the end-to-end call chain from
the request dispatcher to response generation, dispatching from the requested URL-infix and the related feature to the
lower-level implementation of that feature.

### API middleware and API client

The API server supports a simple 2-tier architecture, where its API client (usually the editor) can directly access that
central HTTP server, no further infrastructure required.

But an API client written for a specific editor can only support those features that have been implemented for that
central API server. So using that 2-tier architecture there is no way for the documentation infrastructure
implemented in Guile Scheme to allow access to the Scheme-specific REPL documentation helpers from any other Scheme
dialect - we need a local Scheme instance for the given dialect also to support that. So while complicating both the
implementation as well as the setup of the documentation infrastructure, a local API middleware can in exchange provide
some additional features:

- Add additional features provided by a given Scheme's REPL, or similar documentation introspection.

- Allow direct access to locally cached/bundled middleware without requiring access to the HTTP server.

- Support both REPL access and socket access, e.g. using the `--listen` option from Guile's REPL. Socket access might
  allow more complex communication between the client (editor) and the middleware and - other than a REPL-based
  middleware - it can allow access from multiple clients to a single middleware instance.
