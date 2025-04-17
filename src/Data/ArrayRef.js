export const new_ = () => []

export const length = (xs) => () => xs.length

export const pop_ = ({ pure, none }) => (xs) => () => {
  const x = xs.pop();
  if (x === undefined) {
    return none;
  }
  return pure(x)
}

export const push = (x) => (xs) => () => {
  xs.push(x)
}

export const freeze = (xs) => [...xs]

