# TODO

- [ ] reorganize modules so that not all UI is in one place since that doesn't make sense, and get rid of App0,App1,etc.
- [ ] use different colors or something to show how something different will happen depending on what kind of insertion you're about to do or what is in your clipboard
- [ ] refactor to allow for diagnostics reporting some places
  - [ ] need to reorg modules to make more sense now that diagnostics are allowed in Expr stuff

- [ ] up/down movement
- [ ] clicking on an atom
  - [ ] half and half atom should corrresponds to the point before and after
  - [ ] when drag, should get the whole atom even if you are only over part of it 
- [ ] when move across grouping boundaries, doesnt quite go where expected (when move from inner to outer, should go to the innermost)

- [ ] IDEA: when delete right before a kid, the kid's kids could be spliced in place of it
  - but, this often breaks well-formedness
- [ ] IDEA: make re-rendering more efficient by using keys. even though things are not nested, this can still work, and perhaps will work especially well since Halogen can't re-nest things via diffs, but it can re-arrange things that are all children of the same parent (which non-nesting allows)
  - [ ] **first**: use browser tools to profile how long rendering takes

- [x] when delete zipper, should go at the left or right end of boundary span depending on focus of deleted zipper handle
- [x] zap movement doesn't need to be invertible -- its ok that lateral movement at beginning/end of kids jumps up to parent and can't jump back down via lateral movement
- [x] close paren should move forward
  - VETO: this isn't just an s-expression editor
- [x] tab moves forward
- [x] shift+tab moves backward
- [x] shift+space move backward
- [x] need to make re-rendering more efficient, since right now it grows asymptotically too fast. perhaps using nested and keyed elements will help with the Halogen diffing?
  - VETO: nesting is bad
- [x] use `alt+<arrow>` to move across siblings
  - [x] need a difference key to cycle handle foci: `cmd+<arrow>`
- [x] make delete do something useful at a Point
  - [x] delete' is same as delete except when handle is a Point, first drags back one movement before deleting.
- [x] implement indentation in UlcV1
- [x] figure out better way of visually representing both sides of the linebreak grammar construct
- [x] do nested indentation levels
- [x] rendering organization
  - [x] option 1: render as just a sequence rather than nested tree structure
  - [ ] option 2: fix the way that highlighting works to be more generic over the shape of thing
    - this could work if i use `*:has(...)` css selector carefully as a parent selector, but it doesnt _actually_ seem to work
    - VETO: best to just simplify and work with sequences rather than try to be fancy
- [x] alter the Editor interface to generating edits to do anything it wants with the root Expr, and has to decide where to place handle afterwards
- [x] in Editor spec, allow shortcut keys such as "(" for "#Group" in Lisp and "\n" for newlines, etc
- [x] use custom rendering in the buffer results as well
- [x] searchable menu of edits
  - [x] give Editor proper interface to computing these edits
    - [x] Editor's bufferOptions should receive Handle and root Expr instead of just Point
  - [x] edit an existing label (i.e. re-query it??)
    - VETO: actually no -- can just delete and re-wrap just as easily, and that's what you'd do in text anyway
- [x] give Editor interface to custom rendering
- [x] make stuff in `Ui.Editor` generic over label
- [x] undo/redo
- [x] copy/cut/paste
- [x] delete
- [x] keyboard movement
- [x] escape action
- [x] drag from one point to another

## Bugs

- [ ] BUG: dragging that lets the inner span be outside the outer span
  - [ ] what caused this???
- [ ] BUG: during dragging, something messes up here when I move from down-right and it goes down-left instead???
- [x] BUG: copy does delete instead of copy (need to implement copy)
- [x] BUG: redo takes two tries to work?? and can't redo final undo
    - turned out I want dropping a snapshot by not using the current state snapshot in `undo` and `redo`
- [x] BUG: why is there one fewer indent than there should be at the beginning?
- [x] BUG: when paste a zipper, cursor doesnt go in right place and sometimes duplicates
- [x] BUG: when delete a zipper, if its only 1 deep, then lose everything
- [x] BUG: cursor isn't placed correctly after inserting a zipper at a zipper handle
- [x] BUG: cycling is off by 1 somtimes in edit menu (via up/down arrows)

## Old Stuff

### branch = "direct-dom-manipulation"

- [ ] InsertSpanTooth_Diff
- [ ] DeleteSpanTooth_Diff
- [ ] InsertSpan_Diff
- [ ] DeleteSpan_Diff
- [ ] escpae action
- [ ] copy/paste
- [ ] undo/redo
- [ ] fix bug in dragging that lets the inner span be outside the outer span
- [x] make points have styles such that the inner left and right points get set as focus separately even when they're at the same point
- [x] refactor out `State` argument into a MonadReader via `ReaderT Effect`
- [x] refactor Element manipulation out of `Common` into its own module
- [x] maintain a tree of element references that supports looking up both kids and parents- [x] make it so that the `create*` functions don't require the parent element
  to `appendChild` to
    - VETO: what did i mean by this?: turns out this doesn't quite work out to.
- [x] need to allow for initialization stage of Component (something run right after it's added to the DOM)
  - solved by not using Components


### branch = "halogen"

- [x] drag from one point to another
  - [x] save origin point until done dragging
  - [x] if points are siblings, then make cursor
  - [x] if points make a selection, then make selection
  - [x] otherwise, put cursor around nearest common ancestor node
- [x] two kinds of handles: ones with foci and ones without
  - there are `SpanH` and `ZipperH` which do not have foci, and `Handle` which does have a foci
- [x] how to deal with top level being a span or not? it makes a lot of sense for the logic in Expr for the top level to be a span. but does it always?
  - useful cases:
    - `atSpan`
    - `atZipper`
  - not useful cases:
    - rendering (since then I need to manufactor a Root Expr component)
  - **answer**: top level is still Root, but now we also have a Context type for encoding a Zipper that is from a top Expr (often, the Root Expr) specifically
- [x] adjust ends of handle with mouse
  - [x] adjust ends of span with mouse
    - [x] test if this works
  - [x] adjust ends of zipper with mouse
- [ ] what happens when paste a span on a zipper handle? 
  - [x] currently, just replaces the entire zipper+inside with the pasted span, but this isn't what _should_ happen, probably
- [x] need different kind of tooth at bottom of zipper
- [x] Escape to move handle to beginning of file
- [x] Backspace to delete stuff inside handle
- [x] undo and redo
- [x] better console message filtering
- [x] bug when pasting (a zipper, for example), it goes around teh top level expression rather than the span handle
- [ ] keyboard movement
  - [x] move point around
  - [ ] shift+move to emulate drag
    - [ ] there are some weird behaviors
- [ ] buffer (query textbox)
- [ ] shift-click to emulate dragging from source handle to that target point
- [ ] refactor Editor subcomponents into different modules (and a shared Types module)

