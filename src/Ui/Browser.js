export const navigator_clibpoard_writeText = (s) => () =>
  navigator.clipboard.writeText(s);

export const play_audio_ = (uri) => () => {
  const audio = new Audio(uri);
  audio.volume = 0.1;
  audio.play();
};
