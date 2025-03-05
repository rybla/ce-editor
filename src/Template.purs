module Template where

import Prelude

import Editor.Example (editor)
import Effect (Effect)
import Node.ChildProcess (execSync)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)
import Utility (format)

main_js_url = "main.js"

main :: Effect Unit
main = do
  let index_html_filepath = "docs/index.html"
  let main_js_filepath = "docs/" <> main_js_url

  void $ execSync $ "bun spago bundle --bundle-type app --module {{app_modulename}} --outfile {{main_js_filepath}}"
    # format { app_modulename: "Ui", main_js_filepath }
  writeTextFile UTF8 index_html_filepath index_html

  pure unit

index_html =
  format
    { title: editor.name
    , main_js_url: "/main.js"
    }
    """
<!DOCTYPE html>
<html lang="en">

<body></body>

<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>{{title}}</title>
<script src="{{main_js_url}}"></script>
<style>

html, body {
  margin: 0;
  padding: 0;
}

.Expr {
  padding: 0.5em;

  display: inline-flex;
  flex-direction: row;
  gap: 0.5em;

  box-shadow: 0 0 0 1px black;

  user-select: none;
  cursor: pointer;
  
  background-color: white;

  transition-property: background-color;
  transition-duration: 0.5s;
}

.Expr.ping {
  background-color: color-mix(in hsl, blue, white 50%);
}

.ExprLabel {
  font-weigth: bold;
}

.ConsoleMessageContent {
  background-color: white;

  transition-property: background-color;
  transition-duration: 0.5s;
}

.ConsoleMessageContent.new {
  background-color: color-mix(in hsl, blue, white 50%);
}

</style>
</head>

</html>
  """
