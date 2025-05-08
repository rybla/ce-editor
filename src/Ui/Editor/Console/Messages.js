const max_messages = 100;
const messages = [];
var timestamp = Date.now();

export const get_messages = () => messages;

export const get_timestamp = () => timestamp;

export const push_message = (content) => () => {
  timestamp = Date.now();
  const length = messages.push({ timestamp, content })
  if (length > max_messages) {
    messages.splice(0, length - max_messages);
  }
};

