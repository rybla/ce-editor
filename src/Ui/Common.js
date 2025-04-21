export const htmlDoc = document;
export const htmlBody = document.body;

export const setText_Element = (text) => (e) => () => {
  e.innerText = text;
}

export const getText_Element = (e) => () => {
  return e.innerText;
}

export const removeAllChildren = (e) => () => {
  while (e.firstChild) {
    e.remove(e.firstChild)
  }
}


export const getChildren = (e) => () => {
  const children = []
  for (const child of e.children) {
    children.append(child)
  }
  return children
}