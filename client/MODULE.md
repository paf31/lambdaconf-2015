# Module Documentation

## Module Main


Main.main is the entry point into the project, called at startup.


## Module UI


This module defines the views and application logic for the Programming Language
Database application.

The client is structured as a SPA (single-page application) written using the
Thermite library.

The page is defined by its _state_, and we define _actions_ which act on those
states. Actions can be connected to buttons or other inputs, as we will see.

#### `State`

``` purescript
data State
  = Home [LangSummary] [TagSummary]
  | ViewLang Lang
  | ViewTag Tag [LangSummary]
  | EditLang Lang
  | Loading 
  | Error String
```

The type of application states.

Our state type is a sum, with one constructor for each subpage.

There are four main subpages:

- The main page, which shows a list of languages and tags.
- A page for viewing a single language in detail
- A page for viewing languages with a selected tag
- A page for creating and editing languages

We also define subpages for errors and the loading message.

#### `Action`

``` purescript
data Action
  = LoadList 
  | LoadLang Key
  | LoadTag Tag
  | LoadNewLang 
  | LoadEditLang Lang
  | UpdateForm (Lang -> Lang)
  | SaveLang 
```

The action type defines the various asynchronous actions which we can
attach to the buttons, links and inputs in our UI.

Many of the actions will load the data required to display a new page,
and then display that page.

The `UpdateLang` action applies a function to synchronize the state value
with the state of the form.

#### `initialState`

``` purescript
initialState :: State
```

The initial state is the `Loading` state. We will use a hook to change to the
home page after our component is mounted.

#### `render`

``` purescript
render :: T.Render _ State _ Action
```

The rendering function, which takes the page state and produces a HTML document.

This function is broken into several helper functions to aid readability.

#### `performAction`

``` purescript
performAction :: T.PerformAction _ State _ Action
```

This function takes an action and produces a computation in Thermite's 
`Action` monad.

The `Action` monad can read the current state, update the state, and perform
asynchronous tasks, including AJAX requests.

#### `spec`

``` purescript
spec :: T.Spec _ State _ Action
```

The specification for our component, along with a hook to perform an
action after it is mounted.


## Module UI.AJAX


Functions for creating AJAX requests in the `Action` monad. 

#### `AjaxAction`

``` purescript
type AjaxAction eff response = forall state. T.Action (ajax :: AJAX | eff) state (Either String response)
```

A type synonym for readability.

#### `listLangs`

``` purescript
listLangs :: forall eff. AjaxAction eff [LangSummary]
```

List languages in the database.

#### `listTags`

``` purescript
listTags :: forall eff. AjaxAction eff [TagSummary]
```

List all available tags.

#### `getLang`

``` purescript
getLang :: forall eff. Key -> AjaxAction eff Lang
```

Get a language using its primary key.    

#### `putLang`

``` purescript
putLang :: forall eff. Lang -> AjaxAction eff Ok
```

Insert or update a language.

#### `getTag`

``` purescript
getTag :: forall eff. Tag -> AjaxAction eff [LangSummary]
```

Get a list of language summaries for a tag.             


## Module UI.Types


Types used by the Programming Languages Database client application.

Instances of the `IsForeign` class are provided so that various types
can be safely read from AJAX responses.

#### `Key`

``` purescript
type Key = String
```

A primary key for a language.

#### `Tag`

``` purescript
type Tag = String
```

A tag name.

#### `Ok`

``` purescript
data Ok
  = Ok 
```

An AJAX response with no content.

#### `TagSummary`

``` purescript
newtype TagSummary
  = TagSummary { tag :: Tag }
```

A tag summary record.

#### `LangSummary`

``` purescript
newtype LangSummary
  = LangSummary { name :: String, key :: Key }
```

A language summary record.

#### `Lang`

``` purescript
newtype Lang
  = Lang { tags :: [Tag], rating :: Int, homepage :: String, description :: String, name :: String, key :: Key }
```

A full language record.

#### `emptyLang`

``` purescript
emptyLang :: Lang
```

The empty language record.

#### `isForeignOk`

``` purescript
instance isForeignOk :: IsForeign Ok
```


#### `tagSummaryIsForeign`

``` purescript
instance tagSummaryIsForeign :: IsForeign TagSummary
```


#### `langSummaryIsForeign`

``` purescript
instance langSummaryIsForeign :: IsForeign LangSummary
```


#### `langIsForeign`

``` purescript
instance langIsForeign :: IsForeign Lang
```



## Module UI.Utils


Some helper functions

#### `stringify`

``` purescript
stringify :: Lang -> String
```

Convert a language record to a JSON document.

#### `inputValue`

``` purescript
inputValue :: T.FormEvent -> String
```

Get the current value from an input field.



