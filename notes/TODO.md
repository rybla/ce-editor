# TODO

## halogen-simplified

- [ ] rendering organization
  - [ ] option 1: render as just a sequence rather than nested tree structure
  - [ ] option 2: fix the way that highlighting works to be more generic over the shape of things
- [ ] alter the Editor interface to generating edits to do anything it wants with the root Expr, and has to decide where to place handle afterwards
- [ ] in Editor spec, allow shortcut keys such as "(" for "#Group" in Lisp and "\n" for newlines, etc
- [ ] BUG: when paste a zipper, cursor doesnt go in riht place and sometimes duplicates
- [ ] BUG: when delete a zipper, if its only 1 deep, then lose everything
- [ ] fix bug in dragging that lets the inner span be outside the outer span
- [ ] fix other bugs in dragging
- [ ] fix bug that redo takes two tries to work?? and can't redo final undo
- [x] use custom rendering in the buffer results as well
- [x] searchable menu of edits
  - [x] give Editor proper interface to computing these edits
    - [x] Editor's bufferOptions should receive Handle and root Expr instead of just Point
  - [x] edit an existing label (i.e. re-query it??)
    - VETO: actually no -- can just delete and re-wrap just as easily, and that's what you'd do in text anyway
  - [x] BUG: cycling is off by 1 somtimes
- [x] give Editor interface to custom rendering
- [x] make stuff in `Ui.Editor` generic over label
- [x] undo/redo
- [x] copy/cut/paste
- [x] delete
- [x] keyboard movement
- [x] escape action
- [x] drag from one point to another

## direct-dom-manipulation

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


# halogen

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

