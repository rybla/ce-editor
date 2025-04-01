# TODO

- [x] drag from one point to another
  - [x] save origin point until done dragging
  - [x] if points are siblings, then make cursor
  - [x] if points make a selection, then make selection
  - [x] otherwise, put cursor around nearest common ancestor node
- [ ] adjust ends of handle with mouse
- [ ] two kinds of handles: ones with foci and ones without
  

- how to deal with top level being a span or not? it makes a lot of sense for the logic in Expr for the top level to be a span. but does it always?
  - useful cases:
    - `atSpan`
    - `atZipper`
  - not useful cases:
    - rendering (since then I need to manufactor a Root Expr component)
