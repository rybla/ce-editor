export const navigator_clibpoard_writeText = (s) => () =>
  navigator.clipboard.writeText(s);

export const play_audio_ = (uri) => () => new Audio(uri).play();
