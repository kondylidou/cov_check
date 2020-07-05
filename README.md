# CovCheck
- CovCheck is a WebApp implemented in Haskell 


## Usage
- CovCheck provides real time informations about the current situation of COVID-19 Worldwide 
- CovCheck enables an automatic test (Quiz) to check whether someones is infected by the Corona Virus or not. 

## Sources & Literature
- [WebApp Tutorial](https://jaxenter.com/tutorial-developing-web-applications-in-haskell-107334.html)
- [Postman](https://covid-19-apis.postman.com)
- [GUI Programming](https://www.yesodweb.com/book)
- [App example for Quiz](https://covapp.charite.de)


## Frontend Design & Implementation
- [Example](https://africovid19.info/diagnostic)
- [Yesod](https://www.yesodweb.com/book)

## Server & Client Communication
### REST API
- HTTP/2 more features than HTTP/1.x [Tutorial](https://www.imel.ba/edukacija/learninghttp2.pdf)
- [Postman REST client Tool](https://www.postman.com/downloads/) for Visualization. 

### Client Server Model
Which one?
- Two-tier
/var/folders/v0/1hcm36tj5bzfsr_cxj6th8d40000gn/T/TemporaryItems/(A Document Being Saved By screencaptureui)/Screenshot 2020-06-27 at 01.55.41.png
- Three-tier ( + Database Application )

### Web Server 
- [Apache](http://httpd.apache.org), [Installation Tutorial](https://www.youtube.com/watch?v=woAZe7kBuEI)
- [Salvia](https://hackage.haskell.org/package/salvia-0.0.5)


### JSON Web Service 
- JSON parsing with [Aeson](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json)
- [Warp Server library](https://hackage.haskell.org/package/warp)(for  HTTP/1.x and HTTP/2 based Web Application inferface (WAI))   


## Backend Design 
- Cabal 
- [Postman (covid tracking)](https://covid-19-apis.postman.com)

## Tickets
Luca: Database connection, 
Lidia: backend,
Kevin: Client Server Technologie. 

## Haskell Setup

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`
3. Build libraries: `stack build`

If you have trouble, refer to the [Yesod Quickstart guide](https://www.yesodweb.com/page/quickstart) for additional detail.

## Development

Start a development server with:

```
stack exec -- yesod devel
```

As your code changes, your site will be automatically recompiled and redeployed to localhost.

## Tests

```
stack test --flag CovCheck:library-only --flag CovCheck:dev
```

(Because `yesod devel` passes the `library-only` and `dev` flags, matching those flags means you don't need to recompile between tests and development, and it disables optimization to speed up your test compile times).

## Documentation

* Read the [Yesod Book](https://www.yesodweb.com/book) online for free
* Check [Stackage](http://stackage.org/) for documentation on the packages in your LTS Haskell version, or [search it using Hoogle](https://www.stackage.org/lts/hoogle?q=). Tip: Your LTS version is in your `stack.yaml` file.
* For local documentation, use:
	* `stack haddock --open` to generate Haddock documentation for your dependencies, and open that documentation in a browser
	* `stack hoogle <function, module or type signature>` to generate a Hoogle database and search for your query
* The [Yesod cookbook](https://github.com/yesodweb/yesod-cookbook) has sample code for various needs

## Getting Help

* Ask questions on [Stack Overflow, using the Yesod or Haskell tags](https://stackoverflow.com/questions/tagged/yesod+haskell)
* Ask the [Yesod Google Group](https://groups.google.com/forum/#!forum/yesodweb)
* There are several chatrooms you can ask for help:
	* For IRC, try Freenode#yesod and Freenode#haskell
	* [Functional Programming Slack](https://fpchat-invite.herokuapp.com/), in the #haskell, #haskell-beginners, or #yesod channels.
