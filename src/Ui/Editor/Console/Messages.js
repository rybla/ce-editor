const messages = [];

export const get = () => messages;

export const push = (content) => () => {
  messages.push({ timestamp: Date.now(), content })
};

