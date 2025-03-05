module Template where

import Prelude

import Editor.Example (editor)
import Effect (Effect)
import Node.ChildProcess (execSync)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Utility (format)

main_js_url = "main.js"

main :: Effect Unit
main = do
  let index_html_filepath = "docs/index.html"
  let main_js_filepath = "docs/" <> main_js_url

  void $ execSync $ "bun spago bundle --bundle-type app --module {{app_modulename}} --outfile {{main_js_filepath}}"
    # format { app_modulename: "Ui", main_js_filepath }

  index_html <- readTextFile UTF8 "assets/index.html"
  writeTextFile UTF8 index_html_filepath $ index_html
    # format
        { title: editor.name
        , main_js_url: "/main.js"
        }

  pure unit

