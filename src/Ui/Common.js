export const htmlDoc = document;

export const setText_Element = (text) => (e) => () => {
  e.innerText = text;
}

export const getText_Element = (e) => () => {
  return e.innerText;
}

