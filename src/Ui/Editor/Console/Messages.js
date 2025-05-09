const max_messages = 100;
const messages = [];
var timestamp = 0;

export const get_messages = () => messages;

export const get_timestamp = () => timestamp;

export const push_message = (content) => () => {
  timestamp++;
  const length = messages.push({ timestamp, content })
  if (length > max_messages) {
    messages.splice(0, length - max_messages);
  }
};

