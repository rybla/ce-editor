module Template where

import Prelude

import Editor.Example as Example
import Effect (Effect)
import Node.ChildProcess (execSync)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Utility (format)

dist_dirpath = "docs/"
assets_dirpath = "assets/"

main :: Effect Unit
main = do
  -- main.js
  void $ execSync $
    "bun spago bundle --bundle-type app --module {{app_modulename}} --outfile {{main_js_filepath}}"
      # format { app_modulename: "Ui", main_js_filepath: dist_dirpath <> "main.js" }

  -- index.html
  do
    let editor = Example.mkEditor {}
    content <- readTextFile UTF8 (assets_dirpath <> "index.html")
    writeTextFile UTF8 (dist_dirpath <> "index.html") $ content
      # format
          { title: editor.name
          , main_js_url: "main.js"
          , main_css_url: "main.css"
          }

  -- main.css
  do
    content <- readTextFile UTF8 (assets_dirpath <> "main.css")
    writeTextFile UTF8 (dist_dirpath <> "main.css") $ content
      # format {}

  pure unit

